;;; ox-zola.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Giovanni Crisalfi
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: marzo 18, 2023
;; Modified: marzo 18, 2023
;; Version: 0.0.1
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
      ;; DEBUG messages (temp)
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

(advice-add 'org-hugo--get-front-matter :override #'ox-zola--get-front-matter)
(advice-add 'org-hugo--gen-front-matter :override #'ox-zola--gen-front-matter)

(defalias 'ox-zola-export-wim-to-md 'org-hugo-export-wim-to-md)
(defalias 'ox-zola-export-to-md 'org-hugo-export-to-md)
(defalias 'ox-zola-debug-info 'org-hugo-debug-info)
(defalias 'ox-zola-auto-export-mode 'org-hugo-auto-export-mode)

(provide 'ox-zola)
;;; ox-zola.el ends here
