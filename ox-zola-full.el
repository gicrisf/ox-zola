;;; ox-zola-full.el --- Zola export via ox-hugo with minimal advice -*- lexical-binding: t; -*-
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
;; Full-featured Zola export backend using ox-hugo with targeted advice.
;;
;; Architecture:
;;   - Single :around advice on `org-hugo--gen-front-matter' (per-export only)
;;   - Transforms ox-hugo's alist to Zola TOML with [taxonomies] section
;;   - Loading this file does NOT affect regular ox-hugo exports
;;
;; Keyword priority (highest to lowest):
;;
;;   1. In-buffer keywords: #+ZOLA_BASE_DIR: or #+HUGO_BASE_DIR:
;;      - If BOTH appear in the same file, the LAST one wins (behavior `t`)
;;      - Case-insensitive: #+zola_base_dir: works too
;;
;;   2. Global variable: `org-hugo-base-dir' (set in Emacs config)
;;      - Used only if no in-buffer keyword is found
;;
;; The same priority applies to all paired keywords:
;;   ZOLA_SECTION / HUGO_SECTION  →  :hugo-section  (default: org-hugo-section)
;;   ZOLA_TAGS / HUGO_TAGS        →  :hugo-tags
;;   ZOLA_DRAFT / HUGO_DRAFT      →  :hugo-draft
;;   etc.
;;
;;; Code:

(require 'ox-hugo)

;;; Frontmatter transformation

(defvar ox-zola-full--active nil
  "Non-nil during ox-zola-full export.
Used to conditionally apply Zola transformations.")

(defun ox-zola-full--transform-frontmatter (data)
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

(defun ox-zola-full--gen-front-matter-advice (orig data format)
  "Advice for `org-hugo--gen-front-matter'.
When `ox-zola-full--active' is non-nil, transform DATA for Zola
and encode as TOML.  Otherwise, call ORIG with DATA and FORMAT."
  (if ox-zola-full--active
      (let ((transformed (ox-zola-full--transform-frontmatter data))
            (tomelr-indent-multi-line-strings t))
        (format "+++\n%s\n+++\n" (tomelr-encode transformed)))
    (funcall orig data format)))

;;; Per-export advice management

(defun ox-zola-full--with-advice (fn)
  "Install Zola advice, call FN, then remove advice.
This ensures regular ox-hugo exports are unaffected."
  (advice-add 'org-hugo--gen-front-matter :around
              #'ox-zola-full--gen-front-matter-advice)
  (advice-add 'org-hugo--copy-ltximg-maybe :around
              #'ox-zola-full--copy-ltximg-advice)
  (unwind-protect
      (let ((ox-zola-full--active t))
        (funcall fn))
    (advice-remove 'org-hugo--gen-front-matter
                   #'ox-zola-full--gen-front-matter-advice)
    (advice-remove 'org-hugo--copy-ltximg-maybe
                   #'ox-zola-full--copy-ltximg-advice)))

;;; Advice to guard org-hugo--copy-ltximg-maybe against nil :hugo-base-dir

(defun ox-zola-full--copy-ltximg-advice (orig info)
  "Advice to guard `org-hugo--copy-ltximg-maybe' against nil :hugo-base-dir.
Only calls ORIG when :hugo-base-dir is set in INFO."
  (when (plist-get info :hugo-base-dir)
    (funcall orig info)))

;;; Options filter (workaround for ox-hugo 0.12.1 missing behavior field)

(defun ox-zola-full--filter-options (info backend)
  "Ensure :hugo-base-dir is set from in-buffer keyword if present.
INFO is the export info plist, BACKEND is ignored.
This works around ox-hugo 0.12.1 missing the behavior field on HUGO_BASE_DIR."
  (unless (plist-get info :hugo-base-dir)
    ;; Try to extract from buffer keywords directly
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward
               "^[ \t]*#\\+\\(hugo_base_dir\\|zola_base_dir\\):[ \t]*\\(.+\\)"
               nil t)
          (setq info (plist-put info :hugo-base-dir
                                (substring-no-properties
                                 (string-trim (match-string 2)))))))))
  info)

;;; Derived backend

;; Format: (PROPERTY KEYWORD DEFAULT EVAL-DEFAULT BEHAVIOR)
;; - EVAL-DEFAULT (4th element) gets evaluated by org-export--get-global-options
;; - BEHAVIOR (5th element): t, newline, space, split, parse
(org-export-define-derived-backend 'zola-full 'hugo
  :filters-alist '((:filter-options . ox-zola-full--filter-options))
  :menu-entry
  '(?Z "Export to Zola Markdown (full)"
       ((?z "Subtree/File (WIM)"
            (lambda (a s v _b)
              (ox-zola-full-export-wim-to-md nil a v)))
        (?Z "File to buffer"
            (lambda (a s v _b)
              (ox-zola-full-export-as-md a s v)))
        (?f "File to file"
            (lambda (a s v _b)
              (ox-zola-full-export-to-md a s v)))
        (?o "File and open"
            (lambda (a s v _b)
              (if a
                  (ox-zola-full-export-to-md :async s v)
                (org-open-file (ox-zola-full-export-to-md nil s v)))))))
  :options-alist
  '(;; Site structure — both ZOLA_* and HUGO_* keywords
    (:hugo-base-dir "ZOLA_BASE_DIR" nil org-hugo-base-dir t)
    (:hugo-base-dir "HUGO_BASE_DIR" nil org-hugo-base-dir t)
    (:hugo-section "ZOLA_SECTION" nil org-hugo-section t)
    (:hugo-section "HUGO_SECTION" nil org-hugo-section t)
    ;; Basic metadata
    (:hugo-slug "ZOLA_SLUG" nil nil t)
    (:hugo-slug "HUGO_SLUG" nil nil t)
    (:hugo-draft "ZOLA_DRAFT" nil nil t)
    (:hugo-draft "HUGO_DRAFT" nil nil t)
    (:hugo-weight "ZOLA_WEIGHT" nil nil space)
    (:hugo-weight "HUGO_WEIGHT" nil nil space)
    ;; Taxonomies
    (:hugo-tags "ZOLA_TAGS" nil nil newline)
    (:hugo-tags "HUGO_TAGS" nil nil newline)
    (:hugo-categories "ZOLA_CATEGORIES" nil nil newline)
    (:hugo-categories "HUGO_CATEGORIES" nil nil newline)
    ;; Cross-named (Zola field name ≠ Hugo keyword name)
    (:hugo-lastmod "ZOLA_UPDATED" nil nil t)
    (:hugo-lastmod "HUGO_LASTMOD" nil nil t)
    (:hugo-layout "ZOLA_TEMPLATE" nil nil t)
    (:hugo-layout "HUGO_LAYOUT" nil nil t)
    ;; Extended
    (:hugo-aliases "ZOLA_ALIASES" nil nil space)
    (:hugo-aliases "HUGO_ALIASES" nil nil space)
    (:hugo-custom-front-matter "ZOLA_CUSTOM_FRONT_MATTER" nil nil space)
    (:hugo-custom-front-matter "HUGO_CUSTOM_FRONT_MATTER" nil nil space)))

;;; Output path computation

(defun ox-zola-full--output-path (info)
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
(defun ox-zola-full-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown buffer.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-buffer'."
  (interactive)
  (ox-zola-full--with-advice
   (lambda ()
     (org-export-to-buffer 'zola-full "*Org Zola Export*"
       async subtreep visible-only))))

;;;###autoload
(defun ox-zola-full-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown file.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-file'."
  (interactive)
  (ox-zola-full--with-advice
   (lambda ()
     (let* ((info (org-combine-plists
                   (org-export--get-export-attributes 'zola-full subtreep visible-only)
                   (org-export--get-buffer-attributes)
                   (org-export-get-environment 'zola-full subtreep)))
            ;; Apply filter to fix :hugo-base-dir (workaround for ox-hugo 0.12.1)
            (info (ox-zola-full--filter-options info 'zola-full))
            (outfile (ox-zola-full--output-path info)))
       (when (file-name-directory outfile)
         (make-directory (file-name-directory outfile) t))
       (org-export-to-file 'zola-full outfile async subtreep visible-only)))))

;;;###autoload
(defun ox-zola-full-export-wim-to-md (&optional all-subtrees async visible-only noerror)
  "Export the current subtree/all subtrees/current file to a Zola post.
Uses ox-hugo's WIM (What I Mean) detection logic.
ALL-SUBTREES, ASYNC, VISIBLE-ONLY, and NOERROR are passed to ox-hugo."
  (interactive "P")
  (ox-zola-full--with-advice
   (lambda ()
     (org-hugo-export-wim-to-md all-subtrees async visible-only noerror))))

;;;###autoload
(defun ox-zola-full-export-to-md-and-open (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown file and open it."
  (interactive)
  (if async
      (ox-zola-full-export-to-md async subtreep visible-only)
    (find-file-other-window (ox-zola-full-export-to-md nil subtreep visible-only))))

(provide 'ox-zola-full)
;;; ox-zola-full.el ends here
