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

;;; User options

(defcustom ox-zola-full-special-block-type-properties
  '(("audio" . (:raw t))
    ("youtube" . (:trim-pre t :trim-post t)))
  "Alist mapping special block types to properties.

Each element of the alist is of the form (TYPE . PLIST) where
TYPE is a string holding the special block's type and PLIST is a
property list for that TYPE.

Properties recognized in the PLIST:

- :raw :: When set to t, the contents of the special block are
          exported raw i.e. as typed in the Org buffer.

- :trim-pre :: When set to t, the whitespace before the special
               block is removed.

- :trim-post :: When set to t, the whitespace after the special
               block is removed.

For the special block types not specified in this variable, the
default behavior is same as if (:raw nil :trim-pre nil :trim-post
nil) plist were associated with them."
  :group 'ox-zola
  :type '(alist :key-type string :value-type (plist :key-type symbol :value-type boolean)))

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

;;; Transcoders (special-block, link)

(defun ox-zola-full-link (link desc info)
  "Convert LINK to Markdown with Zola figure shortcodes.
DESC is the link description.  INFO is the export plist.

For inline images that would use Hugo's figure shortcode,
converts to Zola's syntax: {{ figure(src=...) }}"
  (let* ((raw-path (org-element-property :path link))
         (type (org-element-property :type link))
         (link-is-url (member type '("http" "https" "ftp" "mailto"))))
    ;; Handle inline images with figure shortcode
    (if (and (org-export-inline-image-p link org-html-inline-image-rules)
             (not (org-html-standalone-image-p
                   (org-export-get-parent link) info)))
        ;; Inline image - use Markdown syntax
        (let* ((parent (org-export-get-parent link))
               (parent-type (org-element-type parent))
               (grand-parent (when (eq parent-type 'link)
                               (org-export-get-parent parent)))
               (useful-parent (or grand-parent parent))
               (attr (org-export-read-attribute :attr_html useful-parent))
               (alt-text (plist-get attr :alt))
               (source (if link-is-url
                           (concat type ":" raw-path)
                         (org-hugo--attachment-rewrite-maybe raw-path info))))
          (format "![%s](%s)" (or alt-text "") source))
      ;; Check for standalone figure (paragraph with only an image)
      (if (and (org-export-inline-image-p link org-html-inline-image-rules)
               (org-html-standalone-image-p
                (org-export-get-parent link) info))
          ;; Standalone figure - use Zola shortcode
          (let* ((parent (org-export-get-parent link))
                 (parent-type (org-element-type parent))
                 (grand-parent (when (eq parent-type 'link)
                                 (org-export-get-parent parent)))
                 (useful-parent (or grand-parent parent))
                 (attr (org-export-read-attribute :attr_html useful-parent))
                 (caption (or
                           (org-string-nw-p
                            (org-export-data
                             (org-export-get-caption (org-export-get-parent-element link))
                             info))
                           (plist-get attr :caption)))
                 (source (if link-is-url
                             (concat type ":" raw-path)
                           (org-hugo--attachment-rewrite-maybe raw-path info)))
                 (figure-params `((src . ,source)
                                  (alt . ,(plist-get attr :alt))
                                  (caption . ,(when (org-string-nw-p caption)
                                                (replace-regexp-in-string "\"" "\\\\\"" caption)))
                                  (title . ,(plist-get attr :title))
                                  (class . ,(plist-get attr :class))
                                  (width . ,(plist-get attr :width))
                                  (height . ,(plist-get attr :height))
                                  (link . ,(plist-get attr :link))
                                  (target . ,(plist-get attr :target))
                                  (rel . ,(plist-get attr :rel))
                                  (attrlink . ,(plist-get attr :attrlink))))
                 (figure-param-str ""))
            (dolist (param figure-params)
              (let ((name (car param))
                    (val (cdr param)))
                (when val
                  (setq figure-param-str (concat figure-param-str
                                                 (format "%s=\"%s\" " name val))))))
            (format "{{ figure(%s) }}" (org-trim figure-param-str)))
        ;; Fall back to ox-hugo's link handling
        (org-hugo-link link desc info)))))

(defun ox-zola-full-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK to Zola shortcode syntax.
CONTENTS is the block contents.  INFO is the export plist.

Zola shortcode syntax:
- Inline: {{ shortcode(args) }}
- Block: {% shortcode(args) %}...{% end %}"
  (let* ((block-type (org-element-property :type special-block))
         (block-type-plist (cdr (assoc block-type ox-zola-full-special-block-type-properties)))
         (header (org-babel-parse-header-arguments
                  (car (org-element-property :header special-block))))
         (trim-pre (or (alist-get :trim-pre header)
                       (plist-get block-type-plist :trim-pre)))
         (trim-pre (org-hugo--value-get-true-p trim-pre))
         (trim-pre-tag (if trim-pre org-hugo--trim-pre-marker ""))
         (last-element-p (null (org-export-get-next-element special-block info)))
         (trim-post (unless last-element-p
                      (or (alist-get :trim-post header)
                          (plist-get block-type-plist :trim-post))))
         (trim-post (org-hugo--value-get-true-p trim-post))
         (trim-post-tag (if trim-post org-hugo--trim-post-marker ""))
         (paired-shortcodes (let* ((str (plist-get info :hugo-paired-shortcodes))
                                   (str-list (when (org-string-nw-p str)
                                               (split-string str " "))))
                              str-list))
         (sc-regexp "\\`%%?%s\\'")
         (html-attr (org-export-read-attribute :attr_html special-block))
         (contents (when (stringp contents)
                     (org-trim
                      (if (plist-get block-type-plist :raw)
                          (org-element-interpret-data (org-element-contents special-block))
                        contents)))))
    (when contents
      (cond
       ;; Paired shortcode (block-style)
       ((cl-member block-type paired-shortcodes
                   :test (lambda (b sc)
                           (string-match-p (format sc-regexp b) sc)))
        (let* ((attr-sc (org-export-read-attribute :attr_shortcode special-block))
               ;; Positional arguments
               (pos-args (and (null attr-sc)
                              (let* ((raw-list (org-element-property :attr_shortcode special-block))
                                     (raw-str (mapconcat #'identity raw-list " ")))
                                (org-string-nw-p raw-str))))
               ;; Named arguments
               (named-args (unless pos-args
                             (let* ((couples
                                     (cl-loop for (a b) on attr-sc by #'cddr while b
                                              collect (list (substring (symbol-name a) 1) b)))
                                    (raw-str
                                     (mapconcat (lambda (x) (concat (car x) "=" (cadr x))) couples ", ")))
                               (org-string-nw-p raw-str))))
               (sc-args (or pos-args named-args))
               (sc-args (if sc-args (concat " " sc-args " ") ""))
               (sc-begin (format "%s{%s %s(%s) %s}"
                                 trim-pre-tag "%" block-type sc-args "%"))
               (sc-end (format "{%s end %s}%s"
                               "%" "%" trim-post-tag)))
          (format "%s\n%s\n%s" sc-begin contents sc-end)))
       ;; Default: fall back to ox-hugo's handling
       (t
        (org-hugo-special-block special-block contents info))))))

;;; Derived backend

;; Format: (PROPERTY KEYWORD DEFAULT EVAL-DEFAULT BEHAVIOR)
;; - EVAL-DEFAULT (4th element) gets evaluated by org-export--get-global-options
;; - BEHAVIOR (5th element): t, newline, space, split, parse
(org-export-define-derived-backend 'zola-full 'hugo
  :filters-alist '((:filter-options . ox-zola-full--filter-options))
  :translate-alist '((link . ox-zola-full-link)
                     (special-block . ox-zola-full-special-block))
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
