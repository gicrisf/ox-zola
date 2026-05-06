;;; ox-zola.el --- Org export to Zola static site generator -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2024 Giovanni Crisalfi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.2") (ox-hugo "0.8"))
;; Keywords: wp, hypermedia, org, zola
;; Homepage: https://github.com/gicrisf/ox-zola
;;
;;; Commentary:
;;
;; Org-mode export backend for Zola static site generator.
;;
;; This package derives from ox-hugo to produce Zola-compatible
;; TOML frontmatter and shortcodes.  It supports subtree exports,
;; taxonomies with [taxonomies] section, and Zola shortcode syntax.
;;
;; Usage:
;;   M-x ox-zola-export-to-md       ; Export to file
;;   M-x ox-zola-export-as-md       ; Export to buffer
;;   M-x ox-zola-export-wim-to-md   ; "What I Mean" export
;;
;;; Code:

(require 'ox-hugo)

;;; User Options

(defgroup ox-zola nil
  "Org-mode exporter for Zola static site generator."
  :group 'org-export
  :prefix "ox-zola-")

(defcustom ox-zola-base-dir nil
  "Default base directory for Zola site.
Can be overridden with #+ZOLA_BASE_DIR keyword."
  :group 'ox-zola
  :type '(choice (const nil) directory))

(defcustom ox-zola-section "posts"
  "Default section (subdirectory under content/).
Can be overridden with #+ZOLA_SECTION keyword."
  :group 'ox-zola
  :type 'string)

(defcustom ox-zola-special-block-type-properties
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

(defvar ox-zola--active nil
  "Non-nil during ox-zola export.
Used to conditionally apply Zola transformations.")

(defun ox-zola--transform-frontmatter (data)
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
    (dolist (key '(publishDate expiryDate blackfriday logbook menu resources
                   outputs headless isCJKLanguage markup series linkTitle
                   type url videos))
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

(defun ox-zola--gen-front-matter-advice (orig data format)
  "Advice for `org-hugo--gen-front-matter'.
When `ox-zola--active' is non-nil, transform DATA for Zola
and encode as TOML.  Otherwise, call ORIG with DATA and FORMAT."
  (if ox-zola--active
      (let ((transformed (ox-zola--transform-frontmatter data))
            (tomelr-indent-multi-line-strings t))
        (format "+++\n%s\n+++\n" (tomelr-encode transformed)))
    (funcall orig data format)))

;;; Per-export advice management

(defun ox-zola--with-advice (fn)
  "Install Zola advice, call FN, then remove advice.
This ensures regular ox-hugo exports are unaffected."
  (advice-add 'org-hugo--gen-front-matter :around
              #'ox-zola--gen-front-matter-advice)
  (advice-add 'org-hugo--copy-ltximg-maybe :around
              #'ox-zola--copy-ltximg-advice)
  (unwind-protect
      (let ((ox-zola--active t))
        (funcall fn))
    (advice-remove 'org-hugo--gen-front-matter
                   #'ox-zola--gen-front-matter-advice)
    (advice-remove 'org-hugo--copy-ltximg-maybe
                   #'ox-zola--copy-ltximg-advice)))

;;; Advice to guard org-hugo--copy-ltximg-maybe against nil :hugo-base-dir

(defun ox-zola--copy-ltximg-advice (orig info)
  "Advice to guard `org-hugo--copy-ltximg-maybe' against nil :hugo-base-dir.
Only calls ORIG when :hugo-base-dir is set in INFO."
  (when (plist-get info :hugo-base-dir)
    (funcall orig info)))

;;; Options filter (workaround for ox-hugo 0.12.1 missing behavior field)

(defun ox-zola--filter-options (info _backend)
  "Ensure :hugo-base-dir is set from in-buffer keyword if present.
INFO is the export info plist.  _BACKEND is ignored.
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

;;; Zola internal link helper

(defun ox-zola--compute-internal-link (file-path info)
  "Compute Zola internal link path from FILE-PATH.
Returns a path like @/section/post.md.

FILE-PATH can be an absolute path, relative path, or filename.
Uses :hugo-base-dir and :hugo-section from INFO to determine
the target section."
  (let* ((base-dir (plist-get info :hugo-base-dir))
         (default-section (or (plist-get info :hugo-section) ""))
         (slug (file-name-base (file-name-nondirectory file-path)))
         (target-section default-section))
    (when base-dir
      (let* ((buffer-dir (when buffer-file-name
                           (file-name-directory buffer-file-name)))
             (expanded (expand-file-name file-path buffer-dir))
             (content-dir (expand-file-name "content" base-dir)))
        (when (string-prefix-p content-dir expanded)
          (let ((relative (substring expanded (length content-dir))))
            (when (string-prefix-p "/" relative)
              (setq relative (substring relative 1)))
            (let ((dir (ignore-errors (directory-file-name
                                       (file-name-directory relative)))))
              (when (and dir (not (string-empty-p dir)))
                (setq target-section dir)))))))
    (if (string-empty-p target-section)
        (format "@/%s.md" slug)
      (format "@/%s/%s.md" target-section slug))))

;;; Transcoders (special-block, link)

(defun ox-zola-link (link desc info)
  "Convert LINK to Markdown with Zola figure shortcodes.
DESC is the link description.  INFO is the export plist.

For inline images that would use Hugo's figure shortcode,
converts to Zola's syntax: {{ figure(src=...) }}.
For internal links, uses Zola's @/ path syntax instead of Hugo relref."
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
        ;; Zola-compatible link handling (no Hugo relref)
        (cond
         ;; Custom protocol links
         ((org-export-custom-protocol-maybe link desc 'md info))
         ;; External URLs → standard Markdown
         (link-is-url
          (let ((path (concat type ":" raw-path)))
            (if (org-string-nw-p desc)
                (format "[%s](%s)" desc path)
              (format "<%s>" path))))
         ;; Coderef links → anchor link
         ((string= type "coderef")
          (let* ((ref-label (org-element-property :path link))
                 (ref-info (org-hugo-link--resolve-coderef ref-label info))
                 (ref-desc (format (org-export-get-coderef-format ref-label desc)
                                   (plist-get ref-info :ref))))
            (format "[%s](#%s-%s)"
                    ref-desc
                    (plist-get ref-info :anchor-prefix)
                    (plist-get ref-info :line-num))))
         ;; Radio links → anchor link
         ((string= type "radio")
          (let ((destination (org-export-resolve-radio-link link info)))
            (format "[%s](#%s%s)"
                    desc
                    (org-blackfriday--get-ref-prefix 'radio)
                    (org-blackfriday--valid-html-anchor-name
                     (org-element-property :value destination)))))
         ;; Internal links: fuzzy, id, custom-id, file
         (t
          (ox-zola--internal-or-file-link link desc info)))))))

(defun ox-zola--internal-or-file-link (link desc info)
  "Handle fuzzy/id/custom-id and file: links with Zola syntax.
Replaces Hugo's relref shortcode with Zola's @/ path syntax."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link)))
    ;; First handle fuzzy/id/custom-id (internal references)
    (if (member type '("custom-id" "id" "fuzzy"))
        ;; Resolve the internal reference
        (let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          (pcase (org-element-type destination)
            ;; Same-file heading → [desc](#anchor)
            (`headline
             (let ((title (org-export-data
                           (org-element-property :title destination) info)))
               (format "[%s](#%s)"
                       (cond ((org-string-nw-p desc))
                             ((org-export-numbered-headline-p destination info)
                              (mapconcat #'number-to-string
                                         (org-export-get-headline-number
                                          destination info) "."))
                             (t title))
                       (org-hugo--get-anchor destination info))))
            ;; Cross-file reference (resolved via ID to another file)
            (`plain-text
             (let* ((anchor (org-hugo-link--heading-anchor-maybe link info))
                    (zola-path
                     (if (and (org-string-nw-p anchor)
                              (not (string-prefix-p "#" anchor)))
                         ;; Anchor is a post slug (subtree export)
                         (ox-zola--compute-internal-link
                          (concat anchor ".org") info)
                       (ox-zola--compute-internal-link destination info))))
               (when (and anchor (string-prefix-p "#" anchor))
                 (setq zola-path (concat zola-path anchor)))
               (format "[%s](%s)"
                       (or desc (file-name-base destination)) zola-path)))
            ;; Other elements: source blocks, tables, targets, figures
            (_
             (let* ((description
                     (or (org-string-nw-p desc)
                         (let ((number (org-export-get-ordinal
                                        destination info
                                        nil #'org-html--has-caption-p)))
                           (when number
                             (if (atom number)
                                 (number-to-string number)
                               (mapconcat #'number-to-string number "."))))))
                    (dest-link
                     (cond
                      ((memq (org-element-type destination)
                             '(src-block table))
                       (org-blackfriday--get-reference destination))
                      ((and (org-html-standalone-image-p destination info)
                            (eq (org-element-type destination) 'paragraph))
                       (let ((figure-ref (org-blackfriday--get-reference
                                          destination)))
                         (if (org-string-nw-p figure-ref)
                             (replace-regexp-in-string
                              "\\`org-paragraph--"
                              (org-blackfriday--get-ref-prefix 'figure)
                              figure-ref)
                           (org-export-get-reference destination info))))
                      ((eq (org-element-type destination) 'target)
                       (org-blackfriday--get-target-anchor destination))
                      (t
                       (org-export-get-reference destination info)))))
               (if description
                   (format "[%s](#%s)" description dest-link)
                 "")))))
      ;; Handle file: links
      (let* ((path1 (replace-regexp-in-string "\\`file://" "" raw-path))
             (path-lc (downcase path1)))
        (if (string= ".org" (file-name-extension path-lc "."))
            ;; Link to an Org file → compute Zola internal link
            (let* ((ref (file-name-sans-extension
                         (file-name-nondirectory path1)))
                   (anchor "")
                   (is-dummy (string-suffix-p
                              org-hugo--preprocessed-buffer-dummy-file-suffix
                              path-lc)))
              (if is-dummy
                  (progn
                    (setq ref (string-remove-suffix
                               org-hugo--preprocessed-buffer-dummy-file-suffix
                               (file-name-nondirectory path1)))
                    (when (string-match ".*\\.org::\\(#.*\\)" raw-link)
                      (setq anchor (match-string-no-properties 1 raw-link))))
                ;; Regular Org file → resolve search part
                (let ((link-search-str
                       (when (string-match ".*\\.org::\\(.*\\)" raw-link)
                         (match-string-no-properties 1 raw-link))))
                  (when link-search-str
                    (setq anchor (org-hugo--search-and-get-anchor
                                  raw-path link-search-str info)))))
              (let ((zola-path
                     (if (and (org-string-nw-p anchor)
                              (not (string-prefix-p "#" anchor)))
                         ;; Post subtree link (anchor is the slug)
                         (ox-zola--compute-internal-link
                          (concat anchor ".org") info)
                       (ox-zola--compute-internal-link path1 info))))
                (when (and anchor (string-prefix-p "#" anchor))
                  (setq zola-path (concat zola-path anchor)))
                (if (org-string-nw-p desc)
                    (format "[%s](%s)" desc zola-path)
                  (format "[%s](%s)"
                          (or ref zola-path) zola-path))))
          ;; Non-org file → treat as attachment
          (let ((rewritten (org-hugo--attachment-rewrite-maybe path1 info)))
            (if (org-string-nw-p desc)
                (format "[%s](%s)" desc rewritten)
              (format "<%s>" rewritten))))))))

(defun ox-zola--format-shortcode-args (attr-sc)
  "Format ATTR-SC plist as Zola shortcode named arguments.
Returns a string like: id=\"abc123\", width=\"100\""
  (let ((pairs nil)
        (plist attr-sc))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (val-str (if (stringp val)
                          (replace-regexp-in-string
                           "\\`\"\\(.*\\)\"\\'" "\\1" val)
                        val)))
        (push (format "%s=\"%s\""
                      (substring (symbol-name key) 1)
                      val-str)
              pairs)))
    (mapconcat #'identity (nreverse pairs) ", ")))

(defun ox-zola-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK to Zola shortcode syntax.
CONTENTS is the block contents.  INFO is the export plist.

Zola shortcode syntax:
- Inline: {{ shortcode(args) }}
- Block: {% shortcode(args) %}...{% end %}"
  (let* ((block-type (org-element-property :type special-block))
         (block-type-plist (cdr (assoc block-type ox-zola-special-block-type-properties)))
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
         (contents (when (stringp contents)
                     (org-trim
                      (if (plist-get block-type-plist :raw)
                          (org-element-interpret-data (org-element-contents special-block))
                        contents)))))
    (cond
     ;; Paired shortcode (block-style): requires content + in paired list
     ((and contents
           (cl-member block-type paired-shortcodes
                      :test (lambda (b sc)
                              (string-match-p (format sc-regexp b) sc))))
      (let* ((attr-sc (org-export-read-attribute :attr_shortcode special-block))
             ;; Positional arguments (raw text, no :key prefix)
             (pos-args (and (null attr-sc)
                            (let* ((raw-list (org-element-property :attr_shortcode special-block))
                                   (raw-str (mapconcat #'identity raw-list " ")))
                              (org-string-nw-p raw-str))))
             ;; Named arguments (quoted Zola style)
             (named-args (when (and (not pos-args) attr-sc)
                           (ox-zola--format-shortcode-args attr-sc)))
             (sc-args (or pos-args named-args))
             (sc-args (if sc-args (concat " " sc-args " ") ""))
             (sc-begin (format "%s{%s %s(%s) %s}"
                               trim-pre-tag "%" block-type sc-args "%"))
             (sc-end (format "{%s end %s}%s"
                             "%" "%" trim-post-tag)))
        (format "%s\n%s\n%s" sc-begin contents sc-end)))
     ;; Description block: store in frontmatter, suppress body output
     ((string= block-type "description")
      (when contents
        (setq info (plist-put info :description contents)))
      "")
     ;; Details block: semantic HTML with optional <summary>
     ((string= block-type "details")
      (if (not contents)
          ""
        (let* ((sep-re "\\(?:\\`\\|\n\\)[ \t]*---[ \t]*\n")
               (sep-pos (string-match sep-re contents))
               (summary (when sep-pos
                          (substring contents 0 (match-beginning 0))))
               (body (if sep-pos (substring contents (match-end 0)) contents)))
          (format "%s<details>\n%s%s\n</details>%s"
                  (or trim-pre-tag "")
                  (if (org-string-nw-p summary)
                      (format "<summary>%s</summary>\n" summary)
                    "")
                  body
                  (or trim-post-tag "")))))
     ;; Aside block: semantic HTML
     ((string= block-type "aside")
      (if contents
          (format "%s<aside>\n%s\n</aside>%s" trim-pre-tag contents trim-post-tag)
        ""))
     ;; Non-paired shortcode: Zola inline syntax {{ name(args) }}
     (t
      (let* ((attr-sc (org-export-read-attribute :attr_shortcode special-block))
             ;; Positional args from attr_shortcode raw text
             (pos-args-from-attrs (and (null attr-sc)
                                       (let* ((raw-list (org-element-property :attr_shortcode special-block))
                                              (raw-str (mapconcat #'identity raw-list " ")))
                                         (org-string-nw-p raw-str))))
             ;; Positional args from block body content
             (pos-args-from-body (and (null attr-sc)
                                      (not pos-args-from-attrs)
                                      contents
                                      (org-string-nw-p contents)))
             ;; Named args from attr_shortcode :key val pairs
             (named-args (when (and attr-sc
                                    (not pos-args-from-attrs)
                                    (not pos-args-from-body))
                           (ox-zola--format-shortcode-args attr-sc)))
             ;; Final args string
             (sc-args (or pos-args-from-attrs
                          (when pos-args-from-body
                            (format "\"%s\"" pos-args-from-body))
                          named-args
                          "")))
        (format "%s{{ %s(%s) }}%s" trim-pre-tag block-type sc-args trim-post-tag))))))

;;; Derived backend

;; Format: (PROPERTY KEYWORD DEFAULT EVAL-DEFAULT BEHAVIOR)
;; - EVAL-DEFAULT (4th element) gets evaluated by org-export--get-global-options
;; - BEHAVIOR (5th element): t, newline, space, split, parse
(org-export-define-derived-backend 'zola 'hugo
  :filters-alist '((:filter-options . ox-zola--filter-options))
  :translate-alist '((link . ox-zola-link)
                     (special-block . ox-zola-special-block))
  :menu-entry
  '(?Z "Export to Zola Markdown"
       ((?z "Subtree/File (WIM)"
            (lambda (a s v _b)
              (ox-zola-export-wim-to-md nil a v)))
        (?Z "File to buffer"
            (lambda (a s v _b)
              (ox-zola-export-as-md a s v)))
        (?f "File to file"
            (lambda (a s v _b)
              (ox-zola-export-to-md a s v)))
        (?o "File and open"
            (lambda (a s v _b)
              (if a
                  (ox-zola-export-to-md :async s v)
                (org-open-file (ox-zola-export-to-md nil s v)))))))
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
    (:hugo-custom-front-matter "HUGO_CUSTOM_FRONT_MATTER" nil nil space)
    ;; Paired shortcodes
    (:hugo-paired-shortcodes "ZOLA_PAIRED_SHORTCODES" nil org-hugo-paired-shortcodes space)
    (:hugo-paired-shortcodes "HUGO_PAIRED_SHORTCODES" nil org-hugo-paired-shortcodes space)
    ;; Additional commonly used
    (:hugo-audio "ZOLA_AUDIO" nil nil)
    (:hugo-images "ZOLA_IMAGES" nil nil newline)))

;;; Output path computation

(defun ox-zola--output-path (info)
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
(defun ox-zola-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown buffer.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-buffer'."
  (interactive)
  (ox-zola--with-advice
   (lambda ()
     (org-export-to-buffer 'zola "*Org Zola Export*"
       async subtreep visible-only))))

;;;###autoload
(defun ox-zola-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown file.
Optional arguments ASYNC, SUBTREEP, and VISIBLE-ONLY are passed
to `org-export-to-file'."
  (interactive)
  (ox-zola--with-advice
   (lambda ()
     (let* ((info (org-combine-plists
                   (org-export--get-export-attributes 'zola subtreep visible-only)
                   (org-export--get-buffer-attributes)
                   (org-export-get-environment 'zola subtreep)))
            ;; Apply filter to fix :hugo-base-dir (workaround for ox-hugo 0.12.1)
            (info (ox-zola--filter-options info 'zola))
            (outfile (ox-zola--output-path info)))
       (when (file-name-directory outfile)
         (make-directory (file-name-directory outfile) t))
       (org-export-to-file 'zola outfile async subtreep visible-only)))))

;;;###autoload
(defun ox-zola-export-wim-to-md (&optional all-subtrees async visible-only noerror)
  "Export the current subtree/all subtrees/current file to a Zola post.
Uses ox-hugo's WIM (What I Mean) detection logic.
ALL-SUBTREES, ASYNC, VISIBLE-ONLY, and NOERROR are passed to ox-hugo."
  (interactive "P")
  (ox-zola--with-advice
   (lambda ()
     (org-hugo-export-wim-to-md all-subtrees async visible-only noerror))))

;;;###autoload
(defun ox-zola-export-to-md-and-open (&optional async subtreep visible-only)
  "Export current buffer to a Zola Markdown file and open it."
  (interactive)
  (if async
      (ox-zola-export-to-md async subtreep visible-only)
    (find-file-other-window (ox-zola-export-to-md nil subtreep visible-only))))

(provide 'ox-zola)
;;; ox-zola.el ends here
