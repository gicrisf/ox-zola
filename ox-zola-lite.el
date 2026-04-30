;;; ox-zola-lite.el --- Lightweight Org to Zola Markdown exporter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2024 Giovanni Crisalfi
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Version: 0.1.0
;; Keywords: org, markdown, zola
;; Homepage: https://github.com/gicrisf/ox-zola
;; Package-Requires: ((emacs "27.2") (org "9.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A lightweight Org-mode export backend for Zola static site generator.
;; Derives from ox-md (built-in) with no external dependencies.
;;
;; For advanced features (subtree exports, property inheritance, auto-export),
;; use ox-zola (full) which builds on ox-hugo.
;;
;;; Code:

(require 'ox-md)
(require 'org-element)

;;; Commentary

;; ox-zola-lite derives from ox-md (built-in, no deps).
;; It uses the shared custom variables from ox-zola.el:
;;   ox-zola-base-dir, ox-zola-section
;; These are wired into the :options-alist below.

;;; Define Backend

(org-export-define-derived-backend 'zola-lite 'md
  :menu-entry
  '(?Z "Export to Zola (lite)"
       ((?z "To Markdown file" ox-zola-lite-export-to-md)
        (?Z "To Markdown buffer" ox-zola-lite-export-as-md)
        (?o "To file and open" ox-zola-lite-export-to-md-and-open)))
  :options-alist
  '(
    ;; Site structure
    (:zola-base-dir "ZOLA_BASE_DIR" nil ox-zola-base-dir t)
    (:zola-base-dir "HUGO_BASE_DIR" nil nil t)
    (:zola-section "ZOLA_SECTION" nil ox-zola-section t)
    (:zola-section "HUGO_SECTION" nil nil t)
     ;; Basic metadata
    (:zola-slug "ZOLA_SLUG" nil nil t)
    (:zola-slug "HUGO_SLUG" nil nil t)
    (:zola-updated "ZOLA_UPDATED" nil nil t)
    (:zola-updated "HUGO_UPDATED" nil nil t)
    (:zola-draft "ZOLA_DRAFT" nil nil t)
    (:zola-draft "HUGO_DRAFT" nil nil t)
    (:zola-weight "ZOLA_WEIGHT" nil nil t)
    (:zola-weight "HUGO_WEIGHT" nil nil t)
    ;; Taxonomies
    (:zola-tags "ZOLA_TAGS" nil nil split)
    (:zola-tags "HUGO_TAGS" nil nil split)
    (:zola-categories "ZOLA_CATEGORIES" nil nil split)
    (:zola-categories "HUGO_CATEGORIES" nil nil split)
    ;; Page settings
    (:zola-template "ZOLA_TEMPLATE" nil nil t)
    (:zola-template "HUGO_TEMPLATE" nil nil t))
  :translate-alist
  '((template . ox-zola-lite-template)
    (src-block . ox-zola-lite-src-block)))

;;; Frontmatter Generation

(defun ox-zola-lite--format-toml-value (value)
  "Format VALUE as a TOML value (string, list, or boolean)."
  (cond
   ((null value) nil)
   ((and (stringp value) (member value '("true" "false")))
    value)  ; boolean
   ((stringp value)
    (format "%S" value))  ; quoted string
   ((listp value)
    (format "[%s]" (mapconcat (lambda (v) (format "%S" v)) value ", ")))
   (t (format "%s" value))))

(defun ox-zola-lite--indent-lines (str indent)
  "Indent each line of STR with INDENT spaces."
  (let ((prefix (make-string indent ?\s)))
    (mapconcat (lambda (line) (concat prefix line))
               (split-string str "\n" t)
               "\n")))

(defun ox-zola-lite--build-frontmatter (info)
  "Build TOML frontmatter string from INFO plist."
  (let* ((title (let ((t_ (plist-get info :title)))
                  (when t_ (substring-no-properties (org-export-data t_ info)))))
         (author (let ((a (plist-get info :author)))
                   (when a (substring-no-properties (org-export-data a info)))))
         (date (let ((d (plist-get info :date)))
                 (when d (substring-no-properties (org-export-data d info)))))
         (updated (plist-get info :zola-updated))
         (slug (plist-get info :zola-slug))
         (draft (plist-get info :zola-draft))
         (weight (plist-get info :zola-weight))
         (template (plist-get info :zola-template))
         (description (let ((d (plist-get info :description)))
                        (when d (substring-no-properties (org-export-data d info)))))
         (tags (plist-get info :zola-tags))
         (categories (plist-get info :zola-categories))
         (lines nil)
         (taxonomies nil))
    ;; Build main fields
    (when (org-string-nw-p title)
      (push (format "title = %S" title) lines))
    (when (org-string-nw-p author)
      (push (format "authors = [%S]" author) lines))
    (when (org-string-nw-p date)
      (push (format "date = %s" date) lines))
    (when (org-string-nw-p updated)
      (push (format "updated = %s" updated) lines))
    (when (org-string-nw-p slug)
      (push (format "slug = %S" slug) lines))
    (when (org-string-nw-p draft)
      (push (format "draft = %s" draft) lines))
    (when (org-string-nw-p weight)
      (push (format "weight = %s" weight) lines))
    (when (org-string-nw-p template)
      (push (format "template = %S" template) lines))
    (when (org-string-nw-p description)
      (push (format "description = %S" description) lines))
    ;; Build taxonomies section (indented)
    (when tags
      (push (format "    tags = %s" (ox-zola-lite--format-toml-value tags)) taxonomies))
    (when categories
      (push (format "    categories = %s" (ox-zola-lite--format-toml-value categories)) taxonomies))
    ;; Combine everything
    (concat
     "+++\n"
     (mapconcat #'identity (nreverse lines) "\n")
      (when taxonomies
        (concat "\n\n[taxonomies]\n"
                (mapconcat #'identity (nreverse taxonomies) "\n")))
      "\n+++\n\n")))

;;; Transcoders

(defun ox-zola-lite-template (contents info)
  "Return complete document with TOML frontmatter.
CONTENTS is the document body, INFO is the export plist."
  (concat (ox-zola-lite--build-frontmatter info) contents))

(defun ox-zola-lite-src-block (src-block _contents info)
  "Transcode SRC-BLOCK to Zola-compatible fenced code.
INFO is the export plist."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info)))
    (format "```%s\n%s```" (or lang "") code)))

;;; Export Functions

(defun ox-zola-lite--output-path (info)
  "Compute output file path from INFO."
  (let* ((base-dir (plist-get info :zola-base-dir))
         (section (or (plist-get info :zola-section) "posts"))
         (slug (or (plist-get info :zola-slug)
                   (file-name-base (or (buffer-file-name) "export"))))
         (filename (concat slug ".md")))
    (if base-dir
        (expand-file-name filename
                          (expand-file-name section
                                            (expand-file-name "content" base-dir)))
      filename)))

;;;###autoload
(defun ox-zola-lite-export-as-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown buffer.
Optional arguments ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and
EXT-PLIST are passed to `org-export-to-buffer'."
  (interactive)
  (org-export-to-buffer 'zola-lite "*Org Zola Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (text-mode))))

;;;###autoload
(defun ox-zola-lite-export-to-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown file.
Optional arguments ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and
EXT-PLIST are passed to `org-export-to-file'."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes 'zola-lite subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'zola-lite subtreep)))
         (outfile (ox-zola-lite--output-path info)))
    ;; Ensure directory exists
    (when (file-name-directory outfile)
      (make-directory (file-name-directory outfile) t))
    (org-export-to-file 'zola-lite outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-zola-lite-export-to-md-and-open (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown file and open it.
Optional arguments ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and
EXT-PLIST are passed to `ox-zola-lite-export-to-md'."
  (interactive)
  (if async
      (ox-zola-lite-export-to-md async subtreep visible-only body-only ext-plist)
    (find-file (ox-zola-lite-export-to-md nil subtreep visible-only body-only ext-plist))))

(provide 'ox-zola-lite)
;;; ox-zola-lite.el ends here
