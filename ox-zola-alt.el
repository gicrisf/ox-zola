;;; ox-zola-alt.el --- Zola export via ox-hugo with minimal advice -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2024 Giovanni Crisalfi
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Version: 0.2.0
;; Keywords: org, markdown, zola
;; Homepage: https://github.com/gicrisf/ox-zola
;; Package-Requires: ((emacs "27.2") (ox-hugo "0.8"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Minimal Zola export backend using ox-hugo with targeted advice.
;;
;; Architecture:
;;   - Single :around advice on `org-hugo--gen-front-matter' (per-export only)
;;   - Transforms ox-hugo's alist to Zola TOML with [taxonomies] section
;;   - Loading this file does NOT affect regular ox-hugo exports
;;
;;; Code:

(require 'ox-hugo)

;;; Frontmatter transformation

(defvar ox-zola-alt--active nil
  "Non-nil during ox-zola-alt export.
Used to conditionally apply Zola transformations.")

(defun ox-zola-alt--transform-frontmatter (data)
  "Transform DATA alist for Zola: taxonomies section, field renames.
DATA is an alist of the form ((KEY1 . VAL1) (KEY2 . VAL2) ...).
Returns a new alist suitable for Zola's TOML frontmatter."
  (let ((result (copy-alist data)))
    ;; Rename lastmod → updated
    (when-let ((lastmod (alist-get 'lastmod result)))
      (setf (alist-get 'updated result) lastmod)
      (setq result (assq-delete-all 'lastmod result)))
    ;; Rename layout → template
    (when-let ((layout (alist-get 'layout result)))
      (setf (alist-get 'template result) layout)
      (setq result (assq-delete-all 'layout result)))
    ;; Remove Hugo-specific fields not used by Zola
    (dolist (key '(publishDate expiryDate blackfriday logbook menu resources))
      (setq result (assq-delete-all key result)))
    ;; Build [taxonomies] section from tags/categories
    (let ((tags (alist-get 'tags result))
          (categories (alist-get 'categories result)))
      (when (or tags categories)
        (let ((taxonomies nil))
          (when categories
            (push (cons 'categories categories) taxonomies))
          (when tags
            (push (cons 'tags tags) taxonomies))
          (setf (alist-get 'taxonomies result) taxonomies))
        (setq result (assq-delete-all 'tags result))
        (setq result (assq-delete-all 'categories result))))
    result))

(defun ox-zola-alt--gen-front-matter-advice (orig data format)
  "Advice for `org-hugo--gen-front-matter'.
When `ox-zola-alt--active' is non-nil, transform DATA for Zola
and encode as TOML.  Otherwise, call ORIG with DATA and FORMAT."
  (if ox-zola-alt--active
      (let ((transformed (ox-zola-alt--transform-frontmatter data))
            (tomelr-indent-multi-line-strings t))
        (format "+++\n%s\n+++\n" (tomelr-encode transformed)))
    (funcall orig data format)))

;;; Per-export advice management

(defun ox-zola-alt--with-advice (fn)
  "Install Zola advice, call FN, then remove advice.
This ensures regular ox-hugo exports are unaffected."
  (advice-add 'org-hugo--gen-front-matter :around
              #'ox-zola-alt--gen-front-matter-advice)
  (advice-add 'org-hugo--copy-ltximg-maybe :around
              #'ox-zola-alt--copy-ltximg-advice)
  (unwind-protect
      (let ((ox-zola-alt--active t))
        (funcall fn))
    (advice-remove 'org-hugo--gen-front-matter
                   #'ox-zola-alt--gen-front-matter-advice)
    (advice-remove 'org-hugo--copy-ltximg-maybe
                   #'ox-zola-alt--copy-ltximg-advice)))

;;; Advice to guard org-hugo--copy-ltximg-maybe against nil :hugo-base-dir

(defun ox-zola-alt--copy-ltximg-advice (orig info)
  "Advice to guard `org-hugo--copy-ltximg-maybe' against nil :hugo-base-dir.
Only calls ORIG when :hugo-base-dir is set in INFO."
  (when (plist-get info :hugo-base-dir)
    (funcall orig info)))

;;; Derived backend

(org-export-define-derived-backend 'zola-alt 'hugo
  :options-alist
  '(;; ZOLA_* keyword aliases — HUGO_* keywords are inherited from parent
    (:hugo-base-dir "ZOLA_BASE_DIR" nil nil t)
    (:hugo-section "ZOLA_SECTION" nil nil t)
    (:hugo-slug "ZOLA_SLUG" nil nil nil)
    (:hugo-draft "ZOLA_DRAFT" nil nil nil)
    (:hugo-weight "ZOLA_WEIGHT" nil nil nil)
    (:hugo-tags "ZOLA_TAGS" nil nil newline)
    (:hugo-categories "ZOLA_CATEGORIES" nil nil newline)
    ;; Cross-named (Zola field name ≠ Hugo keyword name)
    (:hugo-lastmod "ZOLA_UPDATED" nil nil nil)
    (:hugo-layout "ZOLA_TEMPLATE" nil nil nil)
    ;; Extended
    (:hugo-aliases "ZOLA_ALIASES" nil nil space)
    (:hugo-custom-front-matter "ZOLA_CUSTOM_FRONT_MATTER" nil nil space)))

;;; Output path computation

(defun ox-zola-alt-debug-info ()
  "Debug: show what keywords are being read from current buffer.
Run this interactively to diagnose keyword recognition issues."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes 'zola-alt)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'zola-alt))))
    (message "DEBUG ox-zola-alt info:\n  :hugo-base-dir = %S\n  :hugo-section = %S\n  :hugo-tags = %S"
             (plist-get info :hugo-base-dir)
             (plist-get info :hugo-section)
             (plist-get info :hugo-tags))))

(defun ox-zola-alt-debug-options ()
  "Debug: show all options available for zola-alt backend.
Check if HUGO_BASE_DIR is in the list."
  (interactive)
  (let* ((all-opts (org-export-get-all-options 'zola-alt))
         (base-dir-opts (seq-filter (lambda (o) (eq (car o) :hugo-base-dir)) all-opts)))
    (message "DEBUG: :hugo-base-dir options:\n%S" base-dir-opts)))

(defun ox-zola-alt-debug-buffer-keywords ()
  "Debug: scan buffer for HUGO/ZOLA keywords and show what's found."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (re-search-forward "^#\\+\\(hugo\\|zola\\)_[^:]+:" nil t)
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              found))
      (message "DEBUG: Found keywords in buffer:\n%s"
               (if found
                   (mapconcat #'identity (nreverse found) "\n")
                 "(none)")))))

(defun ox-zola-alt--output-path (info)
  "Compute output file path from INFO plist."
  (let* ((base-dir (plist-get info :hugo-base-dir))
         (section (or (plist-get info :hugo-section) "posts"))
         (slug (or (plist-get info :hugo-slug)
                   (file-name-base (or (buffer-file-name) "export"))))
         (filename (concat slug ".md")))
    (if base-dir
        (expand-file-name filename
                          (expand-file-name section
                                            (expand-file-name "content" base-dir)))
      filename)))

;;; Export commands

;;;###autoload
(defun ox-zola-alt-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown buffer.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-buffer'."
  (interactive)
  (ox-zola-alt--with-advice
   (lambda ()
     (org-export-to-buffer 'zola-alt "*Org Zola Alt Export*"
       async subtreep visible-only))))

;;;###autoload
(defun ox-zola-alt-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown file.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-file'."
  (interactive)
  (ox-zola-alt--with-advice
   (lambda ()
     (let* ((info (org-combine-plists
                   (org-export--get-export-attributes 'zola-alt subtreep visible-only)
                   (org-export--get-buffer-attributes)
                   (org-export-get-environment 'zola-alt subtreep)))
            (outfile (ox-zola-alt--output-path info)))
       (when (file-name-directory outfile)
         (make-directory (file-name-directory outfile) t))
       (org-export-to-file 'zola-alt outfile async subtreep visible-only)))))

(provide 'ox-zola-alt)
;;; ox-zola-alt.el ends here
