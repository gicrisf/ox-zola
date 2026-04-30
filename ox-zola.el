;;; ox-zola.el --- Org export to Zola static site generator -*- lexical-binding: t; -*-
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
;; Org-mode export backends for Zola static site generator.
;;
;; This package provides two backends:
;;
;; - `lite' (default): Lightweight, derives from ox-md, no dependencies.
;;   Good for simple blogs and single-file exports.
;;
;; - `full': Feature-rich, requires ox-hugo (advice-based approach).
;;   Provides subtree exports, shortcodes, and full frontmatter.
;;
;; To select a backend, set `ox-zola-backend' before exporting:
;;
;;   (setq ox-zola-backend 'lite)  ; default, no dependencies
;;   (setq ox-zola-backend 'full)  ; requires ox-hugo
;;
;; Then use the unified export commands:
;;
;;   M-x ox-zola-export-to-md      ; export file to Markdown
;;   M-x ox-zola-export-as-md      ; export to buffer
;;   M-x ox-zola-export-wim-to-md  ; "what I mean" export (full backend)
;;
;;; Code:

(require 'org)

;;; User Options

(defgroup ox-zola nil
  "Org-mode exporter for Zola static site generator."
  :group 'org-export
  :prefix "ox-zola-")

(defcustom ox-zola-backend 'lite
  "The export backend to use.

- `lite': Lightweight backend (default).  Derives from ox-md with no
  external dependencies.  Supports file-based exports with basic
  frontmatter.

- `full': Full-featured backend.  Uses ox-hugo with an advice-based
  approach to produce Zola-compatible frontmatter and shortcodes."
  :group 'ox-zola
  :type '(choice (const :tag "Lite (no dependencies)" lite)
                 (const :tag "Full (requires ox-hugo)" full)))

(defcustom ox-zola-base-dir nil
  "Default base directory for Zola site.
Can be overridden with #+ZOLA_BASE_DIR keyword.
Used by both lite and full backends."
  :group 'ox-zola
  :type '(choice (const nil) directory))

(defcustom ox-zola-section "posts"
  "Default section (subdirectory under content/).
Can be overridden with #+ZOLA_SECTION keyword.
Used by both lite and full backends."
  :group 'ox-zola
  :type 'string)

;;; Backend Loading

(defvar ox-zola--lite-loaded nil
  "Non-nil if the lite backend has been loaded.")

(defvar ox-zola--full-loaded nil
  "Non-nil if the full backend has been loaded.")

(defun ox-zola--ensure-backend ()
  "Ensure the selected backend is loaded.
Returns the backend symbol to use for export."
  (pcase ox-zola-backend
    ('lite
     (unless ox-zola--lite-loaded
       (require 'ox-zola-lite)
       (setq ox-zola--lite-loaded t))
     'zola-lite)
    ('full
     (unless ox-zola--full-loaded
       (if (require 'ox-hugo nil t)
           (progn
             (require 'ox-zola-full)
             (setq ox-zola--full-loaded t))
         (if (yes-or-no-p
              "The 'full' backend requires ox-hugo which is not installed.\n\
Install it or switch to 'lite' backend? [y = switch to lite, n = abort] ")
             (progn
               (setq ox-zola-backend 'lite)
               (message "Switched to 'lite backend.  Set ox-zola-backend to 'full after installing ox-hugo.")
               (ox-zola--ensure-backend))
           (user-error "Please install ox-hugo to use the 'full backend"))))
     'zola-full)
    (_
     (user-error "Invalid ox-zola-backend value: %s (use 'lite or 'full)"
                 ox-zola-backend))))

;;; Unified Export Commands

;;;###autoload
(defun ox-zola-export-as-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown buffer.

Uses the backend specified by `ox-zola-backend'."
  (interactive)
  (let ((backend (ox-zola--ensure-backend)))
    (pcase backend
      ('zola-lite
       (ox-zola-lite-export-as-md async subtreep visible-only body-only ext-plist))
      ('zola-full
       (ox-zola-full-export-as-md async subtreep visible-only)))))

;;;###autoload
(defun ox-zola-export-to-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown file.

Uses the backend specified by `ox-zola-backend'."
  (interactive)
  (let ((backend (ox-zola--ensure-backend)))
    (pcase backend
      ('zola-lite
       (ox-zola-lite-export-to-md async subtreep visible-only body-only ext-plist))
      ('zola-full
       (ox-zola-full-export-to-md async subtreep visible-only)))))

;;;###autoload
(defun ox-zola-export-to-md-and-open (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zola Markdown file and open it.

Uses the backend specified by `ox-zola-backend'."
  (interactive)
  (let ((backend (ox-zola--ensure-backend)))
    (pcase backend
      ('zola-lite
       (ox-zola-lite-export-to-md-and-open async subtreep visible-only body-only ext-plist))
      ('zola-full
       (ox-zola-full-export-to-md-and-open async subtreep visible-only)))))

;;;###autoload
(defun ox-zola-export-wim-to-md (&optional all-subtrees async visible-only noerror)
  "Export \"What I Mean\" to Zola Markdown.

This command intelligently exports based on context:
- If on a subtree with EXPORT_FILE_NAME, export that subtree
- If ALL-SUBTREES is non-nil, export all valid subtrees
- Otherwise, do file-based export

Note: Subtree export features are only available with the \='full backend.
With the \='lite backend, this falls back to file-based export."
  (interactive "P")
  (let ((backend (ox-zola--ensure-backend)))
    (pcase backend
      ('zola-lite
       (when (and all-subtrees (called-interactively-p 'any))
         (message "Note: Subtree export requires 'full backend. Doing file export."))
       (ox-zola-lite-export-to-md nil nil visible-only))
      ('zola-full
       (ox-zola-full-export-wim-to-md all-subtrees async visible-only noerror)))))

;;;###autoload
(defun ox-zola-switch-backend (backend)
  "Switch to BACKEND (\\='lite or \\='full) for Zola export.

Interactive command to easily switch between backends."
  (interactive
   (list (intern (completing-read "Select backend: " '("lite" "full") nil t))))
  (setq ox-zola-backend backend)
  (message "Switched to '%s backend" backend))

;; Load the lite backend by default
(require 'ox-zola-lite)
(setq ox-zola--lite-loaded t)

(provide 'ox-zola)
;;; ox-zola.el ends here
