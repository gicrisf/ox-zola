;;; ox-zola.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Giovanni Crisalfi
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: marzo 18, 2023
;; Modified: marzo 18, 2023
;; Version: 0.0.6
;; Keywords: Org, markdown, docs
;; Homepage: https://github.com/gicrisf/ox-zola
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)
(require 'ox-hugo)

;; This says that shell-mode is defined in shell.el (the ‘.el’ can be omitted).
;; The compiler takes for granted that that file really defines the function, and does not check.
(declare-function org-hugo-pandoc-cite--meta-data-generator "ox-hugo-pandoc-cite")

(defcustom ox-zola-special-block-type-properties '(("audio" . (:raw t))
                                                   ;; ("youtube" . (:raw t))
                                                   ("youtube" . (:trim-pre t :trim-post t)))

  "Alist for storing default properties for special block types.

Each element of the alist is of the form (TYPE . PLIST) where
TYPE is a string holding the special block's type and PLIST is a
property list for that TYPE.

The TYPE string could be any special block type like an HTML
inline or block tag, or name of a Hugo shortcode, or any random
string.

Properties recognized in the PLIST:

- :raw :: When set to t, the contents of the special block as
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

(defun ox-zola--get-front-matter (info)
  "Return a Zola front-matter string.

INFO is a plist used as a communication channel."
  ;; (message "[hugo front-matter DBG] info: %S" (pp info))
  (let* ((fm-format (plist-get info :hugo-front-matter-format))
         (author-list (and (plist-get info :with-author)
                           (let ((author-raw
                                  (org-string-nw-p
                                   (org-export-data (plist-get info :author) info)))) ;`org-export-data' required
                             (when author-raw
                               ;; Multiple authors can be comma or
                               ;; newline separated. The newline
                               ;; separated authors work only for the
                               ;; #+author keyword; example:
                               ;;   #+author: Author1
                               ;;   #+author: Author2
                               ;;
                               ;; If using the subtree properties they
                               ;; need to be comma-separated:
                               ;;   :EXPORT_AUTHOR: Author1, Author2
                               (let ((author-list-1 (org-split-string author-raw "[,\n]")))
                                 ;; Don't allow spaces around author names.
                                 ;; Also remove duplicate authors.
                                 (delete-dups (mapcar #'org-trim author-list-1)))))))
         (creator (and (plist-get info :with-creator)
                       (plist-get info :creator)))
         (locale (and (plist-get info :hugo-with-locale)
                      (org-hugo--get-lang info)))
         (description (org-string-nw-p (plist-get info :description)))
         (aliases-raw (let ((aliases-raw-1 (org-string-nw-p (plist-get info :hugo-aliases))))
                        (when aliases-raw-1
                          (org-split-string aliases-raw-1 " "))))
         (aliases (let (alias-list)
                    (dolist (alias aliases-raw)
                      (unless (string-match-p "/" alias)
                        (let ((section (file-name-as-directory ;Suffix section with "/" if it isn't already
                                        (org-export-data (plist-get info :hugo-section) info))))
                          (setq alias (concat "/" section alias))))
                      (setq alias-list (append alias-list `(,alias))))
                    alias-list))
         (outputs-raw (org-string-nw-p (plist-get info :hugo-outputs)))
         (outputs (when outputs-raw
                    (org-split-string outputs-raw " ")))
         (draft (org-hugo--parse-draft-state info))
         (headless (when (org-hugo--plist-get-true-p info :hugo-headless)
                     (org-hugo--front-matter-value-booleanize (org-hugo--plist-get-true-p info :hugo-headless))))
         (all-t-and-c-str (org-entry-get (point) "ALLTAGS")) ;Includes tags inherited from #+filetags: too.
         (all-t-and-c (or (when (stringp all-t-and-c-str)    ;tags/categories from `all-t-and-c' are used
                            (org-split-string all-t-and-c-str ":")) ;only if HUGO_TAGS or HUGO_CATEGORIES are not set.
                          (and (null (org-hugo--subtree-export-p info)) ;Use #+filetags: for file-based exports if #+hugo_tags are not set.
                               org-file-tags)))
         (tags (or
                ;; Look for tags set using HUGO_TAGS keyword, or
                ;; EXPORT_HUGO_TAGS property if available.
                (org-hugo--delim-str-to-list (plist-get info :hugo-tags))
                ;; Else use Org tags (the ones set in headings
                ;; and/or inherited) if any.
                (let* ((tags-list (cl-remove-if #'org-hugo--category-p all-t-and-c))
                       (tags-list (dolist (fn org-hugo-tag-processing-functions tags-list)
                                    (setq tags-list (funcall fn tags-list info)))))
                  ;; (message "[get fm DBG] tags: tags-list = %S" tags-list)
                  tags-list)))
         (categories (or
                      ;; Look for categories set using HUGO_CATEGORIES
                      ;; keyword, or EXPORT_HUGO_CATEGORIES property
                      ;; if available.
                      (org-hugo--delim-str-to-list (plist-get info :hugo-categories))
                      ;; Else use categories set using Org tags with
                      ;; "@" prefix (the ones set in headings and/or
                      ;; inherited) if any.
                      (let* ((categories-list (cl-remove-if-not #'org-hugo--category-p all-t-and-c))
                             (categories-list (dolist (fn org-hugo-tag-processing-functions categories-list)
                                                (setq categories-list (funcall fn categories-list info))))
                             (categories-list (mapcar (lambda (str)
                                                        ;; Remove "@" from beg of categories.
                                                        (replace-regexp-in-string "\\`@" "" str))
                                                      categories-list)))
                        ;; (message "dbg: categories: categories-list = %s" categories-list)
                        categories-list)))
         (keywords (org-hugo--delim-str-to-list (plist-get info :keywords)))
         (weight-data (let ((wt-raw-list (org-hugo--parse-property-arguments (plist-get info :hugo-weight)))
                            weight-data-1)
                        (dolist (wt-raw wt-raw-list)
                          (let (key value)
                            ;; (message "weight DBG wt-raw: %S" wt-raw)
                            ;; (message "weight DBG cdr wt-raw: %S" (cdr wt-raw))
                            ;; (message "weight DBG org-hugo--subtree-coord: %S" org-hugo--subtree-coord)
                            (cond
                             ((null (cdr wt-raw)) ;`wt-raw' is not of the type (TAXONOMY . WEIGHT)
                              (setq key 'weight)
                              (setq value (cond
                                           ((and org-hugo--subtree-coord
                                                 (equal (car wt-raw) 'auto)) ;(auto)
                                            (org-hugo--calc-weight))
                                           ((and (equal (car wt-raw) 'auto) ;Auto weight ineffective for file-based exports
                                                 (null org-hugo--subtree-coord))
                                            nil)
                                           (t
                                            (string-to-number (symbol-name (car wt-raw)))))))
                             (t
                              (setq key (if (equal (car wt-raw) 'page) ;`wt-raw' is of the type (page . WEIGHT)
                                            'weight
                                          (intern (format "%s_weight" (symbol-name (car wt-raw))))))
                              (setq value (cond
                                           ((and org-hugo--subtree-coord
                                                 (equal (cdr wt-raw) "auto")) ;(TAXONOMY . "auto") or (page . "auto")
                                            (org-hugo--calc-weight))
                                           ((numberp (cdr wt-raw))
                                            (cdr wt-raw))
                                           ((and (equal (cdr wt-raw) "auto") ;Auto weight ineffective for file-based exports
                                                 (null org-hugo--subtree-coord))
                                            nil)
                                           (t
                                            (user-error "Ox-hugo: Invalid weight %S" (cdr wt-raw)))))))
                            ;; (message "weight DBG key: %S" key)
                            ;; (message "weight DBG value: %S" value)
                            (push (cons key value) weight-data-1)))
                        ;; (message "weight DBG weight-data: %S" weight-data-1)
                        weight-data-1))
         (menu-alist (org-hugo--parse-menu-prop-to-alist info))
         (custom-fm-data (org-hugo--parse-property-arguments (plist-get info :hugo-custom-front-matter)))
         (resources (org-hugo--get-resources-alist
                     (org-hugo--parse-property-arguments (plist-get info :hugo-resources))))
         (blackfriday (unless (org-hugo--plist-get-true-p info :hugo-goldmark)
                        (require 'ox-hugo-deprecated)
                        (org-hugo--parse-blackfriday-prop-to-alist (plist-get info :hugo-blackfriday))))
         (data `(;; The order of the elements below will be the order in which the front-matter
                 ;; variables will be ordered.
                 (title . ,(org-hugo--get-sanitized-title info))
                 (audio . ,(org-hugo--string-unquote (plist-get info :hugo-audio)))
                 (author . ,author-list)
                 (description . ,description)
                 (date . ,(org-hugo--format-date :date info))
                 (publishDate . ,(org-hugo--format-date :hugo-publishdate info))
                 (expiryDate . ,(org-hugo--format-date :hugo-expirydate info))
                 (aliases . ,aliases)
                 (images . ,(org-hugo--delim-str-to-list (plist-get info :hugo-images)))
                 (isCJKLanguage . ,(org-hugo--plist-get-true-p info :hugo-iscjklanguage))
                 (keywords . ,keywords)
                 (layout . ,(plist-get info :hugo-layout))
                 (updated . ,(org-hugo--format-date :hugo-lastmod info))
                 (linkTitle . ,(plist-get info :hugo-linktitle))
                 (markup . ,(plist-get info :hugo-markup))
                 (outputs . ,outputs)
                 (series . ,(org-hugo--delim-str-to-list (plist-get info :hugo-series)))
                 (slug . ,(plist-get info :hugo-slug))
                 (tags . ,tags)
                 (categories . ,categories)
                 (type . ,(plist-get info :hugo-type))
                 (url . ,(plist-get info :hugo-url))
                 (videos . ,(org-hugo--delim-str-to-list (plist-get info :hugo-videos)))
                 (draft . ,draft)
                 (headless . ,headless)
                 (creator . ,creator)
                 (locale . ,locale)
                 (taxonomies . '())
                 (blackfriday . ,blackfriday)))
         (data `,(append weight-data custom-fm-data data
                         (list
                          (cons 'menu menu-alist)
                          (cons 'resources resources)
                          (cons 'logbook (plist-get info :logbook)))))
         ret)

    ;; (message "[get fm DBG] tags: %s" tags)
    ;; (message "dbg: hugo tags: %S" (plist-get info :hugo-tags))
    ;; (message "[get fm info DBG] %S" info)
    ;; (message "[get fm menu DBG] %S" menu-alist)
    ;; (message "[get fm menu override DBG] %S" menu-alist-override)
    ;; (message "[custom fm data DBG] %S" custom-fm-data)
    ;; (message "[fm resources OUT DBG] %S" resources)
    ;; (message "[fm data DBG] data: %S" data)
    ;; (progn (message "[fm data DBG] ") (pp data))
    ;; (message "[fm tags DBG] %S" tags)
    ;; (message "[fm categories DBG] %S" categories)
    ;; (message "[fm keywords DBG] %S" keywords)

    ;; Append group tags to user-set tags if tag groups are defined in
    ;; the buffer.
    (when (and org-group-tags org-tag-groups-alist)
      (let (tag-groups-alist-mod)

        ;; Copy `org-tag-groups-alist' to `tag-groups-alist-mod' while
        ;; modifying the tags and categories as defined by
        ;; `org-hugo-tag-processing-functions'.
        (dolist (group org-tag-groups-alist)
          (let ((group-mod group))
            (dolist (fn org-hugo-tag-processing-functions group-mod)
              (setq group-mod (funcall fn group-mod info)))
            (push group-mod tag-groups-alist-mod)))

        (dolist (t-or-c (append tags categories))
          (let ((to-be-searched `(,t-or-c)))
            (while (> (length to-be-searched) 0)
              ;; (message "[tag group DBG] t and c to search: %S" to-be-searched)
              (let ((tc (pop to-be-searched)))
                (dolist (group tag-groups-alist-mod)
                  ;; (message "[tag group DBG]   Searching %s in %S" tc group)
                  (when (member tc group)
                    (let ((head-tag (car group)))
                      (if (org-hugo--category-p head-tag)
                          (let ((head-cat (replace-regexp-in-string "\\`@" "" head-tag)))
                            (unless (member head-cat categories)
                              (push head-cat categories)
                              ;; (message "[tag group DBG] .... Adding cat %s" head-cat)
                              ))
                        (unless (member head-tag tags)
                          (push head-tag tags)
                          ;; (message "[tag group DBG] .... Adding tag %s" head-tag)
                          ))
                      ;; Add the current `head-tag' as the new tag to
                      ;; search if current tag or category (`tc') is not
                      ;; the `head-tag', and if it's not already in the
                      ;; search list.
                      (unless (or (string= tc head-tag)
                                  (member head-tag to-be-searched))
                        (push head-tag to-be-searched))))))))))
      ;; (message "[tag group DBG] updated tags: %S" tags)
      ;; (message "[tag group DBG] updated categories: %S" categories)

      ;; Overwrite the 'tags and 'categories key values in `data' with
      ;; the updated values.
      ;; https://stackoverflow.com/a/40815365/1219634
      (setf (alist-get 'tags data) tags)
      (setf (alist-get 'categories data) categories))
    ;; (setq taxonomies `(;; The order of elements below will be respected
    ;;                   (tags . tags)
    ;;                   (categories . categories)))
    ;; (setf (alist-get 'taxonomies data) taxonomies))

    (setq data (org-hugo--replace-keys-maybe data info))
    (setq ret (org-hugo--gen-front-matter data fm-format))
    (if (and (string= "toml" fm-format)
             (org-hugo--pandoc-citations-enabled-p info))
        (progn
          ;; Pandoc parses fields like csl and nocite from YAML
          ;; front-matter.  So create the `org-hugo--fm-yaml'
          ;; front-matter in YAML format just for Pandoc.
          (require 'ox-hugo-pandoc-cite)
          (setq org-hugo--fm-yaml
                (org-hugo-pandoc-cite--meta-data-generator data)))
      (setq org-hugo--fm-yaml ret))
    ;; (message "org-hugo--fm-yaml: `%s'" org-hugo--fm-yaml)
    ret))

(defun ox-zola--gen-front-matter (data format)
  "Generate the Zola post front-matter, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string.

Generate the front-matter in the specified FORMAT.  Valid values
are \"toml\" and \"yaml\"."
  (if (string= format "yaml")
      (org-hugo--gen-yaml-front-matter data)
    (let ((tomelr-indent-multi-line-strings t))
      ;; Will commit this block of comments to remember the process
      ;; Final manipulation of data in this position?
      ;; (setq taxonomies '((alist-get 'tags data) (alist-get 'categories data)))
      ;; (message "taxonomies list %s" taxonomies)
      ;;
      ;; TODO remove buffer gen below
      ;; (setq xbuff (generate-new-buffer "*ox-hugo alist*"))
      ;; (print (format "Show data alist %s" data) xbuff)
      ;;
      ;; (message "Show data alist %s" data)
      ;; (message "Tags are: %s" (alist-get 'tags data))
      ;; (message "Categories are: %s" (alist-get 'categories data))

      ;; Build taxonomies alist
      ;; TODO delete tags and categories field in place with `delete`
      ;; TODO Move this function up
      (setq taxonomies `(;; The order of elements below will be respected
                         (tags . ,(alist-get 'tags data))
                         (categories . ,(alist-get 'categories data))))

      ;; (message "Taxonomies will be: %s" taxonomies)

      ;; Replace taxonomies in data with actual taxonomies
      (setf (alist-get 'taxonomies data) taxonomies)
      ;; (setq data (append data taxonomies))
      ;; (message "Show data alist %s" data)

      (format "+++\n%s\n+++\n" (tomelr-encode data)))))

(defun ox-zola--set-pseudohugo-backend ()
  "Define a fake Hugo backend to support Zola metadata."
  (org-export-define-derived-backend 'hugo 'blackfriday ; hugo < blackfriday < md < html
    :menu-entry
    '(?H "Export to Hugo-compatible Markdown"
           ((?H "Subtree or File to Md file            "
                (lambda (a _s v _b)
                  (org-hugo-export-wim-to-md nil a v)))
            (?h "File to Md file"
                (lambda (a s v _b)
                  (org-hugo-export-to-md a s v)))
            (?O "Subtree or File to Md file and open   "
                (lambda (a _s v _b)
                  (if a
                      (org-hugo-export-wim-to-md nil :async v)
                    (org-open-file (org-hugo-export-wim-to-md nil nil v)))))
            (?o "File to Md file and open"
                (lambda (a s v _b)
                  (if a
                      (org-hugo-export-to-md :async s v)
                    (org-open-file (org-hugo-export-to-md nil s v)))))
            (?A "All subtrees (or File) to Md file(s)  "
                (lambda (a _s v _b)
                  (org-hugo-export-wim-to-md :all-subtrees a v)))
            ;; TODO File to a temporary Md buffer (ox-zola-export-as-md)
            (?t "File to a temporary Md buffer"
                (lambda (a s v _b)
                  (org-hugo-export-as-md a s v)))))
    ;;;; translate-alist
      :translate-alist '((code . org-hugo-kbd-tags-maybe)
                         (drawer . org-hugo-drawer)
                         (example-block . org-hugo-example-block)
                         (export-block . org-hugo-export-block)
                         (export-snippet . org-hugo-export-snippet)
                         (headline . org-hugo-heading)
                         (inner-template . org-hugo-inner-template)
                         (inline-src-block . org-hugo-inline-src-block)
                         (keyword . org-hugo-keyword)
                         (link . org-hugo-link)
                         (paragraph . org-hugo-paragraph)
                         (src-block . org-hugo-src-block)
                         (special-block . org-hugo-special-block))
    :filters-alist '((:filter-body . org-hugo-body-filter))
    ;;;; options-alist
      ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
    :options-alist '(;; Variables not setting the front-matter directly
                       (:with-toc nil "toc" org-hugo-export-with-toc)
                       (:section-numbers nil "num" org-hugo-export-with-section-numbers)
                       (:author "AUTHOR" nil user-full-name newline)
                       (:creator "CREATOR" nil org-hugo-export-creator-string)
                       (:with-smart-quotes nil "'" nil) ;Hugo/Goldmark does more correct conversion to smart quotes, especially for single quotes.
                       (:with-special-strings nil "-" nil) ;Hugo/Goldmark does the auto-conversion of "--" -> "–", "---" -> "—" and "..." -> "…"
                       (:with-sub-superscript nil "^" '{}) ;Require curly braces to be wrapped around text to sub/super-scripted
                       (:hugo-with-locale "ZOLA_WITH_LOCALE" nil nil)
                       (:hugo-front-matter-format "ZOLA_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                       (:hugo-level-offset "ZOLA_LEVEL_OFFSET" nil "1")
                       (:hugo-preserve-filling "ZOLA_PRESERVE_FILLING" nil org-hugo-preserve-filling) ;Preserve breaks so that text filling in Markdown matches that of Org
                       (:hugo-delete-trailing-ws "ZOLA_DELETE_TRAILING_WS" nil org-hugo-delete-trailing-ws)
                       (:hugo-section "ZOLA_SECTION" nil org-hugo-section)
                       (:hugo-bundle "ZOLA_BUNDLE" nil nil)
                       (:hugo-base-dir "ZOLA_BASE_DIR" nil org-hugo-base-dir)
                       (:hugo-goldmark "ZOLA_GOLDMARK" nil org-hugo-goldmark)
                       (:hugo-code-fence "ZOLA_CODE_FENCE" nil t) ;Prefer to generate triple-backquoted Markdown code blocks by default.
                       (:hugo-use-code-for-kbd "ZOLA_USE_CODE_FOR_KBD" nil org-hugo-use-code-for-kbd)
                       (:hugo-prefer-hyphen-in-tags "ZOLA_PREFER_HYPHEN_IN_TAGS" nil org-hugo-prefer-hyphen-in-tags)
                       (:hugo-allow-spaces-in-tags "ZOLA_ALLOW_SPACES_IN_TAGS" nil org-hugo-allow-spaces-in-tags)
                       (:hugo-auto-set-lastmod "ZOLA_AUTO_SET_LASTMOD" nil org-hugo-auto-set-lastmod)
                       (:hugo-custom-front-matter "ZOLA_CUSTOM_FRONT_MATTER" nil nil space)
                       (:hugo-blackfriday "ZOLA_BLACKFRIDAY" nil nil space) ;Deprecated. See https://github.com/kaushalmodi/ox-hugo/discussions/485.
                       (:hugo-front-matter-key-replace "ZOLA_FRONT_MATTER_KEY_REPLACE" nil nil space)
                       (:hugo-date-format "ZOLA_DATE_FORMAT" nil org-hugo-date-format)
                       (:hugo-paired-shortcodes "ZOLA_PAIRED_SHORTCODES" nil org-hugo-paired-shortcodes space)
                       (:hugo-pandoc-citations "ZOLA_PANDOC_CITATIONS" nil nil)
                       (:bibliography "BIBLIOGRAPHY" nil nil newline) ;Used in ox-hugo-pandoc-cite
                       (:html-container "HTML_CONTAINER" nil org-hugo-container-element)
                       (:html-container-class "HTML_CONTAINER_CLASS" nil "")
    
                       ;; Front-matter variables
                       ;; https://gohugo.io/content-management/front-matter/#front-matter-variables
                       ;; aliases
                       (:hugo-aliases "ZOLA_ALIASES" nil nil space)
                       ;; audio
                       (:hugo-audio "ZOLA_AUDIO" nil nil)
                       ;; date
                       ;; "date" is parsed from the Org #+date or subtree property EXPORT_HUGO_DATE
                       (:date "DATE" nil nil)
                       ;; description
                       (:description "DESCRIPTION" nil nil)
                       ;; draft
                       ;; "draft" value interpreted by the TODO state of a
                       ;; post as Org subtree gets higher precedence.
                       (:hugo-draft "ZOLA_DRAFT" nil nil)
                       ;; expiryDate
                       (:hugo-expirydate "ZOLA_EXPIRYDATE" nil nil)
                       ;; headless (only for Page Bundles - Hugo v0.35+)
                       (:hugo-headless "ZOLA_HEADLESS" nil nil)
                       ;; images
                       (:hugo-images "ZOLA_IMAGES" nil nil newline)
                       ;; isCJKLanguage
                       (:hugo-iscjklanguage "ZOLA_ISCJKLANGUAGE" nil nil)
                       ;; keywords
                       ;; "keywords" is parsed from the Org #+keywords or
                       ;; subtree property EXPORT_KEYWORDS.
                       (:keywords "KEYWORDS" nil nil newline)
                       ;; layout
                       (:hugo-layout "ZOLA_LAYOUT" nil nil)
                       ;; lastmod
                       (:hugo-lastmod "ZOLA_LASTMOD" nil nil)
                       ;; linkTitle
                       (:hugo-linktitle "ZOLA_LINKTITLE" nil nil)
                       ;; locale (used in Hugo internal templates)
                       (:hugo-locale "ZOLA_LOCALE" nil nil)
                       ;; markup
                       (:hugo-markup "ZOLA_MARKUP" nil nil) ;default is "md"
                       ;; menu
                       (:hugo-menu "ZOLA_MENU" nil nil space)
                       (:hugo-menu-override "ZOLA_MENU_OVERRIDE" nil nil space)
                       ;; outputs
                       (:hugo-outputs "ZOLA_OUTPUTS" nil nil space)
                       ;; publishDate
                       (:hugo-publishdate "ZOLA_PUBLISHDATE" nil nil)
                       ;; series
                       (:hugo-series "ZOLA_SERIES" nil nil newline)
                       ;; slug
                       (:hugo-slug "ZOLA_SLUG" nil nil)
                       ;; taxomonomies - tags, categories
                       (:hugo-tags "ZOLA_TAGS" nil nil newline)
                       ;; #+hugo_tags are used to set the post tags in Org
                       ;; files written for file-based exports.  But for
                       ;; subtree-based exports, the EXPORT_HUGO_TAGS
                       ;; property can be used to override inherited tags
                       ;; and Org-style tags.
                       (:hugo-categories "ZOLA_CATEGORIES" nil nil newline)
                       ;; #+hugo_categories are used to set the post
                       ;; categories in Org files written for file-based
                       ;; exports.  But for subtree-based exports, the
                       ;; EXPORT_HUGO_CATEGORIES property can be used to
                       ;; override inherited categories and Org-style
                       ;; categories (Org-style tags with "@" prefix).
                       ;; resources
                       (:hugo-resources "ZOLA_RESOURCES" nil nil space)
                       ;; title
                       ;; "title" is parsed from the Org #+title or the subtree heading.
                       ;; type
                       (:hugo-type "ZOLA_TYPE" nil nil)
                       ;; url
                       (:hugo-url "ZOLA_URL" nil nil)
                       ;; videos
                       (:hugo-videos "ZOLA_VIDEOS" nil nil newline)
                       ;; weight
                       (:hugo-weight "ZOLA_WEIGHT" nil nil space))
    )
  )

(defun ox-zola-special-block (special-block contents info)
  (let*
    ((block-type (org-element-property :type special-block))
     (block-type-plist (cdr (assoc block-type ox-zola-special-block-type-properties)))
     (header (org-babel-parse-header-arguments
              (car (org-element-property :header special-block))))
     (trim-pre (or (alist-get :trim-pre header) ;`:trim-pre' in #+header has higher precedence.
                   (plist-get block-type-plist :trim-pre)))
     (trim-pre (org-hugo--value-get-true-p trim-pre)) ;If "nil", converts to nil
     (trim-pre-tag (if trim-pre org-hugo--trim-pre-marker ""))
     (last-element-p (null (org-export-get-next-element special-block info)))
     (trim-post (unless last-element-p ;No need to add trim-post markers if this is the last element.
                  (or (alist-get :trim-post header) ;`:trim-post' in #+header has higher precedence.
                      (plist-get block-type-plist :trim-pre))))
     (trim-post (org-hugo--value-get-true-p trim-post)) ;If "nil", converts to nil
     (trim-post-tag (if trim-post org-hugo--trim-post-marker ""))
     (paired-shortcodes (let* ((str (plist-get info :hugo-paired-shortcodes))
                               (str-list (when (org-string-nw-p str)
                                           (split-string str " "))))
                          str-list))
     (sc-regexp "\\`%%?%s\\'") ;Regexp to match an element from `paired-shortcodes'
     (html-attr (org-export-read-attribute :attr_html special-block))
     (caption (plist-get html-attr :caption))
     (contents (when (stringp contents)
                 (org-trim
                  (if (plist-get block-type-plist :raw)
                      ;; https://lists.gnu.org/r/emacs-orgmode/2022-01/msg00132.html
                      (org-element-interpret-data (org-element-contents special-block))
                    contents)))))
    ;; (message "[ox-zola-spl-blk DBG] block-type: %s" block-type)
    ;; (message "[ox-zola-spl-blk DBG] last element?: %s" (null (org-export-get-next-element special-block info)))
    ;; (message "[ox-zola-spl-blk DBG] %s: header: %s" block-type header)
    ;; (message "[ox-zola-spl-blk DBG] %s: trim-pre (type = %S): %S" block-type (type-of trim-pre) trim-pre)
    ;; (message "[ox-zola-spl-blk DBG] %s: trim-post (type = %S): %S" block-type (type-of trim-post) trim-post)
    (plist-put info :type-plist block-type-plist)
    (plist-put info :trim-pre-tag trim-pre-tag)
    (plist-put info :trim-post-tag trim-post-tag)
    (when contents
      (cond
       ;; https://emacs.stackexchange.com/a/28685/115
       ((cl-member block-type paired-shortcodes
                   ;; If `block-type' is "foo", check if any of the
                   ;; elements in `paired-shortcodes' is "foo" or
                   ;; "%foo".
                   :test (lambda (b sc) ;`sc' would be an element from `paired-shortcodes'
                           (string-match-p (format sc-regexp b) sc)))
        (let* ((attr-sc (org-export-read-attribute :attr_shortcode special-block))
               ;; Positional arguments.
               (pos-args (and (null attr-sc)
                              ;; If the shortcode attributes are not of
                              ;; the type ":foo bar" but are something
                              ;; like "foo bar".
                              (let* ((raw-list (org-element-property :attr_shortcode special-block))
                                     (raw-str (mapconcat #'identity raw-list " ")))
                                (org-string-nw-p raw-str))))
        
               ;; Named arguments.
               (named-args (unless pos-args
                             (let* ((raw-list (org-element-property :attr_shortcode special-block))
                                    (couples
                                     (cl-loop for (a b) on attr-sc by #'cddr while b
                                              collect (list (substring (symbol-name a) 1) b)))
                                    (raw-str
                                     (mapconcat (lambda (x) (concat (car x) "=" (car (cdr x)) )) couples ", ")))
                               (org-string-nw-p raw-str))))
        
               (sc-args (or pos-args named-args))
               (sc-args (if sc-args
                            (concat " " sc-args " ")
                          ""))
               (matched-sc-str (car
                                (cl-member block-type paired-shortcodes
                                           :test (lambda (b sc) ;`sc' would be an element from `paired-shortcodes'
                                                   (string-match-p (format sc-regexp b) sc)))))
        
               (sc-begin (format "%s{%s %s(%s) %s}"
                                 trim-pre-tag "%" block-type sc-args "%"))
               (sc-end (format "{%s end %s}%s"
                               "%" "%" trim-post-tag)))
        
          (message "[ox-zola-spl-blk DBG] attr-sc1: %s"
                   (org-element-property :attr_shortcode special-block))
          (message "[ox-zola-spl-blk DBG] attr-sc: %s" attr-sc)
          (message "[ox-zola-spl-blk DBG] pos-args: %s" pos-args)
          (message "[ox-zola-spl-blk DBG] named-args: %s" named-args)
        
          (format "%s\n%s\n%s"
                  sc-begin contents sc-end))
        )
       (t
        (org-blackfriday-special-block special-block contents info)))
      )))

(defun ox-zola-link (link desc info)
  "Convert LINK to Markdown format.

DESC is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
  (let* ((raw-link (org-element-property :raw-link link))
         (raw-path (org-element-property :path link))
         (type (org-element-property :type link))
         (link-is-url (member type '("http" "https" "ftp" "mailto"))))
    ;; (message "[org-hugo-link DBG] raw-path 1: %s" raw-path)

    (when (and (stringp raw-path)
               link-is-url)
      (setq raw-path (org-blackfriday--url-sanitize-maybe
                      info (url-encode-url raw-path))))
    ;; (message "[org-hugo-link DBG] raw-link: %s" raw-link)
    ;; (message "[org-hugo-link DBG] raw-path 2: %s" raw-path)
    ;; (message "[org-hugo-link DBG] link: %S" link)
    ;; (message "[org-hugo-link DBG] link type: %s" type)
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'md))
     ((member type '("custom-id" "id"
                     "fuzzy")) ;, #+name, heading links
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        ;; (message "[org-hugo-link DBG] link type: %s" type)
        ;; (message "[org-hugo-link DBG] destination: %s" destination)
        ;; (message "[org-hugo-link DBG] link: %S" link)
        ;; (message "[org-hugo-link DBG] link destination elem type: %S" (org-element-type destination))
        (pcase (org-element-type destination)
          ;; External file.
          (`plain-text
           (let ((path (progn
                         ;; Treat links to `file.org' as links to `file.md'.
                         (if (string= ".org" (downcase (file-name-extension destination ".")))
                             (concat (file-name-sans-extension destination) ".md")
                           destination))))
             ;; (message "[org-hugo-link DBG] plain-text path: %s" path)
             (if (org-id-find-id-file raw-path)
                 (let* ((anchor (org-hugo-link--heading-anchor-maybe link info))
                        (ref (if (and (org-string-nw-p anchor)
                                      (not (string-prefix-p "#" anchor)))
                                 ;; If the "anchor" doesn't begin with
                                 ;; "#", it's a direct reference to a
                                 ;; post subtree.
                                 anchor
                               (concat path anchor))))
                   ;; (message "[org-hugo-link DBG] plain-text org-id anchor: %S" anchor)
                   (format "[%s]({{< relref \"%s\" >}})" (or desc path) ref))
               (if desc
                   (format "[%s](%s)" desc path)
                 (format "<%s>" path)))))

          ;; Links of type [[* Some heading]].
          (`headline
                     (let ((title (org-export-data (org-element-property :title destination) info)))
                       (format
                        "[%s](#%s)"
                        ;; Description
                        (cond ((org-string-nw-p desc))
                              ((org-export-numbered-headline-p destination info)
                               (mapconcat #'number-to-string
                                          (org-export-get-headline-number destination info)
                                          "."))
                              (t
                               title))
                        ;; Reference
                        (org-hugo--get-anchor destination info))))
          ;; Links to other Org elements like source blocks, tables,
          ;; paragraphs, standalone figures,  links, etc.
          
          )))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      ;; (message "[org-hugo-link DBG] processing an image: %s" desc)
      (let* ((parent (org-export-get-parent link))
             (parent-type (org-element-type parent))
             ;; If this is a hyper-linked image, it's parent type will
             ;; be a link too. Get the parent of *that* link in that
             ;; case.
             (grand-parent (when (eq parent-type 'link)
                             (org-export-get-parent parent)))
             (useful-parent (if grand-parent
                                grand-parent
                              parent))
             (attr (org-export-read-attribute :attr_html useful-parent))
             (caption (or
                       ;; Caption set using #+caption takes higher precedence.
                       (org-string-nw-p
                        (org-export-data  ;Look for caption set using #+caption
                         (org-export-get-caption (org-export-get-parent-element link))
                         info))
                       (plist-get attr :caption)))
             (caption (when (org-string-nw-p caption)
                        (format "%s %s"
                                (format (org-html--translate
                                         (concat
                                          (cdr (assoc 'figure org-blackfriday--org-element-string))
                                          " %d:")
                                         info)
                                        (org-export-get-ordinal
                                         useful-parent info
                                         nil #'org-html--has-caption-p))
                                (string-trim caption "\"" "\""))))
             (extension (file-name-extension raw-path))
             (inlined-svg (and (stringp extension)
                               (string= "svg" (downcase extension))
                               (plist-get attr :inlined))))
        ;; (message "[org-hugo-link DBG] Inline image: %s, extension: %s" raw-path extension)
        ;; (message "[org-hugo-link DBG] inlined svg? %S" inlined-svg)
        ;; (message "[org-hugo-link DBG] caption: %s" caption)
        ;;
        (if inlined-svg
            (let* ((svg-contents (with-temp-buffer
                                   (insert-file-contents raw-path)
                                   (fill-region (point-min) (point-max)) ;Make huge one-liner SVGs sane
                                   (buffer-substring-no-properties (point-min) (point-max))))
                   (svg-contents-sanitized (replace-regexp-in-string
                                            ;; Remove the HTML comments.
                                            "<!--\\(.\\|\n\\)*?-->" ""
                                            (replace-regexp-in-string
                                             ;; Remove the xml document tag as that cannot be inlined in-between
                                             ;; a Markdown (or even an HTML) file.
                                             "<\\?xml version=\"1\\.0\" encoding=\"UTF-8\" standalone=\"no\"\\?>" ""
                                             ;; Remove !DOCTYPE tag from the inlined SVG.
                                             (replace-regexp-in-string
                                              "<!DOCTYPE svg[^>]+>" ""
                                              svg-contents))))
                   (svg-html (if caption
                                 (format "<figure>\n%s\n<figcaption>\n\n  %s\n</figcaption>\n</figure>"
                                         svg-contents-sanitized caption)
                               svg-contents-sanitized)))
              ;; (message "[org-hugo-link DBG] svg contents: %s" svg-contents)
              ;; (message "[org-hugo-link DBG] svg contents sanitized: %s" svg-contents-sanitized)
              svg-html)
          (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
                 (inline-image (not (org-html-standalone-image-p useful-parent info)))
                 (source (if link-is-url
                             (concat type ":" path)
                           path))
                 (num-attr (/ (length attr) 2)) ;(:alt foo) -> num-attr = 1
                 (alt-text (plist-get attr :alt)))
            ;; (message "[org-hugo-link DBG] path: %s" path)
            ;; (message "[org-hugo-link DBG] inline image? %s" inline-image)
            ;; (message "[org-hugo-link DBG] attr: %s num of attr: %d"
            ;;           attr (length attr))
            ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
            ;; (message "[org-hugo-link DBG] useful-parent-type: %s"
            ;;           (org-element-type useful-parent))
            (cond
             (;; Use the Markdown image syntax if the image is inline and
              ;; there are no HTML attributes for the image, or just one
              ;; attribute, the `alt-text'.
              (and inline-image
                   (or (= 0 num-attr)
                       (and alt-text
                            (= 1 num-attr))))
              (let ((alt-text (if alt-text
                                  alt-text
                                "")))
                (format "![%s](%s)" alt-text source)))
             (;; Else if the image is inline (with non-alt-text
              ;; attributes), use HTML <img> tag syntax.
              inline-image
              ;; The "target" and "rel" attributes would be meant for <a>
              ;; tags. So do not pass them to the <img> tag.
              (plist-put attr :target nil)
              (plist-put attr :rel nil)
              (org-html--format-image source attr info))
             (t ;Else use the Hugo `figure' shortcode.
              ;; Hugo `figure' shortcode named parameters.
              ;; https://gohugo.io/content-management/shortcodes/#figure
              (let ((figure-params `((src . ,source)
                                     (alt . ,alt-text)
                                     (caption . ,(when (org-string-nw-p caption)
                                                   (replace-regexp-in-string "\"" "\\\\\\&" caption))) ;Escape the double-quotes, if any.
                                     (link . ,(plist-get attr :link))
                                     (title . ,(plist-get attr :title))
                                     (class . ,(plist-get attr :class))
                                     (attr . ,(plist-get attr :attr))
                                     (attrlink . ,(plist-get attr :attrlink))
                                     (width . ,(plist-get attr :width))
                                     (height . ,(plist-get attr :height))
                                     ;; While the `target' and `rel'
                                     ;; attributes are not supported by
                                     ;; the inbuilt Hugo `figure'
                                     ;; shortcode, they can be used as
                                     ;; intended if a user has a custom
                                     ;; `figure' shortcode with the
                                     ;; support added for those.
                                     (target . ,(plist-get attr :target))
                                     (rel . ,(plist-get attr :rel))))
                    (figure-param-str ""))
                (dolist (param figure-params)
                  (let ((name (car param))
                        (val (cdr param)))
                    (when val
                      (setq figure-param-str (concat figure-param-str
                                                     (format "%s=\"%s\" "
                                                             name val))))))
                ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
                ;; EDITED FROM OX-HUGO
                (format "{{ figure(%s) }}" (org-trim figure-param-str)))))))
        ))

     ((string= type "coderef")
      (let* ((ref-label (org-element-property :path link))
             (ref-info (org-hugo-link--resolve-coderef ref-label info))
             (desc (format (org-export-get-coderef-format ref-label desc)
                           (plist-get ref-info :ref))))
        ;; (message "[org-hugo-link DBG] coderef ref label: %s" ref-label)
        ;; (message "[org-hugo-link DBG] coderef ref str: %s" (plist-get ref-info :ref))
        ;; (message "[org-hugo-link DBG] coderef anchor prefix: %s" (plist-get ref-info :anchor-prefix))
        ;; (message "[org-hugo-link DBG] coderef line num: %s" (plist-get ref-info :line-num))
        ;; (message "[org-hugo-link DBG] coderef desc: %s" desc)
        (format "[%s](#%s-%s)"
                desc
                (plist-get ref-info :anchor-prefix)
                (plist-get ref-info :line-num))))
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (format "[%s](#%s%s)"
                desc
                (org-blackfriday--get-ref-prefix 'radio)
                (org-blackfriday--valid-html-anchor-name
                 (org-element-property :value destination)))))
     (t ;[[file:foo.png]], [[file:foo.org::* Heading]], [[file:foo.org::#custom-id]], link type: file
           (let* ((link-param-str "")
                  (path (cond
                         (link-is-url
                          ;; Taken from ox-html.el -- Extract attributes
                          ;; from parent's paragraph.  HACK: Only do this
                          ;; for the first link in parent (inner image link
                          ;; for inline images).  This is needed as long as
                          ;; attributes cannot be set on a per link basis.
                          (let* ((attr
                                  (let ((parent (org-export-get-parent-element link)))
                                    (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                         (org-export-read-attribute :attr_html parent))))
                                 ;; https://www.w3schools.com/tags/tag_link.asp
                                 (link-params `((title . ,(plist-get attr :title))
                                                (style . ,(plist-get attr :style))
                                                (referrerpolicy . ,(plist-get attr :referrerpolicy))
                                                (media . ,(plist-get attr :media))
                                                (target . ,(plist-get attr :target))
                                                (rel . ,(plist-get attr :rel))
                                                (sizes . ,(plist-get attr :sizes))
                                                (type . ,(plist-get attr :type)))))
                            (dolist (param link-params)
                              (let ((name (car param))
                                    (val (cdr param)))
                                (when val
                                  (setq link-param-str (concat link-param-str
                                                               (format "%s=\"%s\" "
                                                                       name val))))))
                            ;; (message "[org-hugo-link DBG] link params: %s" link-param-str)
                            )
                          (concat type ":" raw-path))
                         (;; Remove the "file://" prefix.
                          (string= type "file")
                          (message "[org-hugo-link DBG] raw-path: %s" raw-path)
                          (let* ((path1 (replace-regexp-in-string "\\`file://" "" raw-path))
                                 (path-lc (downcase path1)))
                            (cond
                             (;; foo.org, foo.org::* Heading, foo.org::#custom_id
                              (string= ".org" (file-name-extension path-lc "."))
                              (let ((ref "")
                                    (anchor ""))
                                (if (string-suffix-p org-hugo--preprocessed-buffer-dummy-file-suffix path-lc)
                                    (progn
                                      (setq ref (string-remove-suffix
                                                 org-hugo--preprocessed-buffer-dummy-file-suffix
                                                 (file-name-nondirectory path1)))
                                      ;; Dummy Org file paths created in
                                      ;; `org-hugo--get-pre-processed-buffer'
                                      ;; For dummy Org file paths, we are
                                      ;; limiting to only "#" style search
                                      ;; strings.
                                      (when (string-match ".*\\.org::\\(#.*\\)" raw-link)
                                        (setq anchor (match-string-no-properties 1 raw-link))))
                                  ;; Regular Org file paths.
                                  (setq ref (file-name-sans-extension (file-name-nondirectory path1)))
                                  (let ((link-search-str
                                         ;; If raw-link is "./foo.org::#bar",
                                         ;; set `link-search-str' to
                                         ;; "#bar".
                                         (when (string-match ".*\\.org::\\(.*\\)" raw-link)
                                           (match-string-no-properties 1 raw-link))))
                                    (message "[org-hugo-link DBG] link-search-str: %s" link-search-str)
                                    (when link-search-str
                                      (setq anchor (org-hugo--search-and-get-anchor raw-path link-search-str info)))))
                                ;; (message "[org-hugo-link file.org::*Heading DBG] ref    = %s" ref)
                                ;; (message "[org-hugo-link file.org::*Heading DBG] anchor = %s" anchor)
                                (cond
                                 ;; Link to a post subtree.  In this case,
                                 ;; the "anchor" is actually the post's
                                 ;; slug.
                                 ((and (org-string-nw-p anchor) (not (string-prefix-p "#" anchor)))
                                  (format "{{< relref \"%s\" >}}" anchor))
                                 ;; Link to a non-post subtree, like a subheading in a post.
                                 ((or (org-string-nw-p ref) (org-string-nw-p anchor))
                                  (format "{{< relref \"%s%s\" >}}" ref anchor))
                                 (t
                                  ""))))
                             (t ;; attachments like foo.png
                              (org-hugo--attachment-rewrite-maybe path1 info)))))
                         (t
                          raw-path)))
                  (link-param-str (org-string-nw-p (org-trim link-param-str))))
             ;; (message "[org-hugo-link DBG] desc=%s path=%s" desc path)
             ;; (message "[org-hugo-link DBG] link-param-str=%s" link-param-str)
             (cond
              ;; Link description is a `figure' shortcode but does not
              ;; already have the `link' parameter set.
              ((and desc
                    (string-match-p "\\`{{<\\s-*figure\\s-+" desc)
                    (not (string-match-p "\\`{{<\\s-*figure\\s-+.*link=" desc)))
               (replace-regexp-in-string "\\s-*>}}\\'"
                                         (format " link=\"%s\"\\&" path)
                                         desc))
              ;; Both link description and link attributes are present.
              ((and desc
                    link-param-str)
               (format "<a href=\"%s\" %s>%s</a>"
                       (org-html-encode-plain-text path)
                       link-param-str
                       (org-link-unescape desc)))
              ;; Only link description, but no link attributes.
              (desc
               (let* ((path-has-space (and
                                       (not (string-prefix-p "{{< relref " path))
                                       (string-match-p "\\s-" path)))
                      (path (if path-has-space
                                ;; https://github.com/kaushalmodi/ox-hugo/issues/376
                                ;; https://github.com/gohugoio/hugo/issues/6742#issuecomment-573924706
                                (format "<%s>" path)
                              path)))
                 (format "[%s](%s)" desc path)))
              ;; Only link attributes, but no link description.
              (link-param-str
               (let ((path (org-html-encode-plain-text path)))
                 (format "<a href=\"%s\" %s>%s</a>"
                         path
                         link-param-str
                         ;; Below trick is to prevent Hugo from
                         ;; auto-hyperlinking the link in the
                         ;; description. Idea from
                         ;; https://stackoverflow.com/q/25706012/1219634.
                         (replace-regexp-in-string ":" "&colon;" (org-link-unescape path)))))
              ;; Neither link description, nor link attributes.
              ((string-prefix-p "{{< relref " path)
               (format "[%s](%s)" path path))
              ((org-string-nw-p path)
               (format "<%s>" path))
              (t
               ""))))
     )))

(defun ox-zola--sandwiching (fun)
  "Execute Org-hugo FUN inside an environment tailed for Zola."
  (let ((original-hugo-backend (org-export-get-backend 'hugo)))
    (progn
      (ox-zola--set-pseudohugo-backend)
      (advice-add 'org-hugo--get-front-matter :override #'ox-zola--get-front-matter)
      (advice-add 'org-hugo--gen-front-matter :override #'ox-zola--gen-front-matter)
      (advice-add 'org-hugo-link :override #'ox-zola-link)
      (condition-case err
          (funcall fun)
        (error
         (message "%s" (replace-regexp-in-string "hugo" "zola"
                                                 (error-message-string err)))))
      (org-export-register-backend original-hugo-backend)
      (advice-remove 'org-hugo--get-front-matter #'ox-zola--get-front-matter)
      (advice-remove 'org-hugo--gen-front-matter #'ox-zola--gen-front-matter)
      (advice-remove 'org-hugo-link #'ox-zola-link))))

(defun ox-zola-export-wim-to-md ()
  "Export the current subtree/all subtrees/current file to a Zola post."
  (interactive)
  (ox-zola--sandwiching #'org-hugo-export-wim-to-md))

(defun ox-zola-export-to-md ()
  "Export current buffer to a Zola-compatible Markdown file."
  (interactive)
  (ox-zola--sandwiching #'org-hugo-export-to-md))

(provide 'ox-zola)
;;; ox-zola.el ends here
