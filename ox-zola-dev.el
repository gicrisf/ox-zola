;;; ox-zola-dev.el --- Development utilities for ox-zola -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Giovanni Crisalfi
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Created: 2022-07-30
;; Modified: 2026-05-06
;; Version: 0.3.0
;; Keywords: wp, hypermedia, org, zola
;; Package-Requires: ((emacs "27.2"))
;; Homepage: https://github.com/gicrisf/ox-zola
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
;;; Commentary:
;;
;; Development and diagnostic utilities for ox-zola.
;; These functions help investigate problems with org-export options
;; inheritance, particularly the ox-hugo 0.12.1 missing behavior field issue.
;;
;; This file is NOT required for normal ox-zola usage.
;; Load it manually when debugging: (require 'ox-zola-dev)
;;
;;; Code:

(require 'ox-zola)

;;; Debug command

(defun ox-zola-dev-debug ()
  "Show current export settings for this buffer.
Requires ox-zola to be loaded."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (let ((info (ox-zola--filter-options
               (org-export-get-environment 'zola)
               'zola)))
    (message "base-dir=%S section=%S tags=%S draft=%S"
             (plist-get info :hugo-base-dir)
             (plist-get info :hugo-section)
             (plist-get info :hugo-tags)
             (plist-get info :hugo-draft))))

;;; Diagnostic functions

(defun ox-zola-dev-diagnose ()
  "Check if zola backend properly inherits HUGO_BASE_DIR."
  (interactive)
  (let* ((backend (org-export-get-backend 'zola))
         (parent (and backend (org-export-backend-parent backend)))
         (all-opts (org-export-get-all-options 'zola))
          (base-dir-opts
          (let (result)
            (dolist (o all-opts (nreverse result))
              (when (eq (car o) :hugo-base-dir)
                (push o result))))
         (keywords (mapcar (lambda (o) (nth 1 o)) base-dir-opts)))
    (message "Backend: %S, Parent: %S\nKeywords for :hugo-base-dir: %S"
             (and backend (org-export-backend-name backend))
             parent
             keywords)))

(defun ox-zola-dev-diagnose-parsing ()
  "Diagnose keyword parsing in current Org buffer.
Shows what each layer of org-export returns for :hugo-base-dir."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (let* ((buffer-keywords
          (save-excursion
            (goto-char (point-min))
            (let (found (case-fold-search t))
              (while (re-search-forward "^#\\+\\(hugo_base_dir\\|zola_base_dir\\):" nil t)
                (push (substring-no-properties (match-string 1)) found))
              (nreverse found))))
         (inbuffer-opts (org-export--get-inbuffer-options 'zola))
         (full-env (org-export-get-environment 'zola))
         (filtered-env (ox-zola--filter-options (copy-sequence full-env) 'zola))
         (hugo-inbuffer (org-export--get-inbuffer-options 'hugo)))
    (message
     "=== Parsing Diagnosis ===
Keywords in buffer: %S
inbuffer-options zola: %S
get-environment zola: %S
after filter-options: %S
inbuffer-options hugo: %S
org-hugo-base-dir global: %S"
     buffer-keywords
     (plist-get inbuffer-opts :hugo-base-dir)
     (plist-get full-env :hugo-base-dir)
     (plist-get filtered-env :hugo-base-dir)
     (plist-get hugo-inbuffer :hugo-base-dir)
     (bound-and-true-p org-hugo-base-dir))))

(defun ox-zola-dev-diagnose-keywords-alist ()
  "Show how HUGO_BASE_DIR entries are structured in the keywords alist.
Output goes to *ox-zola-dev-diag* buffer."
  (interactive)
  (let* ((all-opts (org-export-get-all-options 'zola))
         (keywords
          (let (kw)
            (dolist (entry all-opts)
              (let ((keyword (nth 1 entry)))
                (when keyword
                  (push (list keyword (nth 0 entry) (nth 4 entry)) kw))))
            kw))
         (hugo-entries
          (let (result)
            (dolist (e keywords (nreverse result))
              (when (string-equal (downcase (car e)) "hugo_base_dir")
                (push e result))))
         (assoc-result (assoc-string "HUGO_BASE_DIR" keywords t)))
    (with-current-buffer (get-buffer-create "*ox-zola-dev-diag*")
      (erase-buffer)
      (insert "=== Keywords Alist Diagnosis ===\n\n")
      (insert (format "Total options: %d, Total keywords: %d\n\n" (length all-opts) (length keywords)))
      (insert "HUGO_BASE_DIR entries (first = what assoc-string finds):\n")
      (dolist (e hugo-entries)
        (insert (format "  %S\n" e)))
      (insert (format "\nassoc-string result: %S\n" assoc-result))
      (display-buffer (current-buffer)))))

(defun ox-zola-dev-diagnose-raw-options ()
  "Show raw options-alist entries for :hugo-base-dir from each backend.
Output goes to *ox-zola-dev-diag* buffer."
  (interactive)
  (let* ((zola-backend (org-export-get-backend 'zola))
         (hugo-backend (org-export-get-backend 'hugo))
         (zola-opts (and zola-backend (org-export-backend-options zola-backend)))
         (hugo-opts (and hugo-backend (org-export-backend-options hugo-backend)))
          (zola-base-dir
           (let (result)
             (dolist (o zola-opts (nreverse result))
               (when (eq (car o) :hugo-base-dir)
                 (push o result))))
          (hugo-base-dir
           (let (result)
             (dolist (o hugo-opts (nreverse result))
               (when (eq (car o) :hugo-base-dir)
                 (push o result)))))
    (with-current-buffer (get-buffer-create "*ox-zola-dev-diag*")
      (erase-buffer)
      (insert "=== Raw Options-Alist for :hugo-base-dir ===\n\n")
      (insert "Format: (:property \"KEYWORD\" default eval-default behavior)\n\n")
      (insert "--- zola backend (own options): ---\n")
      (dolist (e zola-base-dir)
        (insert (format "  %S\n" e)))
      (insert "\n--- hugo backend (own options): ---\n")
      (dolist (e hugo-base-dir)
        (insert (format "  %S\n" e)))
      (insert (format "\nox-hugo version: %s\n"
                      (if (boundp 'org-hugo-version) org-hugo-version "unknown")))
      (display-buffer (current-buffer)))))

(provide 'ox-zola-dev)
;;; ox-zola-dev.el ends here
