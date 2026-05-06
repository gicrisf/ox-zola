;;; ox-zola-test.el --- Tests for ox-zola -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;;; Commentary:

;; ERT tests for ox-zola export backend.

;;; Code:

(require 'ert)
(require 'ox-zola)

;;; Backend registration tests

(ert-deftest ox-zola-test-backend-exists ()
  "Test that the zola backend is properly registered."
  (should (org-export-get-backend 'zola)))

(ert-deftest ox-zola-test-backend-derives-from-hugo ()
  "Test that zola backend derives from hugo."
  (let ((backend (org-export-get-backend 'zola)))
    (should (eq (org-export-backend-parent backend) 'hugo))))

(ert-deftest ox-zola-test-menu-entry-registered ()
  "Test that the Zola menu entry is registered."
  (let ((backend (org-export-get-backend 'zola)))
    (should (org-export-backend-menu backend))))

;;; Custom variable tests

(ert-deftest ox-zola-test-base-dir-variable-exists ()
  "Test that ox-zola-base-dir variable exists."
  (should (boundp 'ox-zola-base-dir)))

(ert-deftest ox-zola-test-section-variable-exists ()
  "Test that ox-zola-section variable exists with correct default."
  (should (boundp 'ox-zola-section))
  (should (equal ox-zola-section "posts")))

(ert-deftest ox-zola-test-special-block-properties-exists ()
  "Test that ox-zola-special-block-type-properties exists."
  (should (boundp 'ox-zola-special-block-type-properties)))

;;; Options alist tests

(ert-deftest ox-zola-test-options-alist ()
  "Test that ZOLA_* keywords are recognized."
  (let ((backend (org-export-get-backend 'zola)))
    (let ((options (org-export-backend-options backend)))
      ;; Check expected options exist
      (should (assq :hugo-base-dir options))
      (should (assq :hugo-section options))
      (should (assq :hugo-tags options))
      (should (assq :hugo-categories options)))))

;;; End-to-end tests with fixture data

(defvar ox-zola-test-data-dir
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test Org fixture files.")

(defun ox-zola-test--read-fixture (filename)
  "Read the contents of test fixture FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ox-zola-test-data-dir))
    (buffer-string)))

;;; Keyword recognition tests

(ert-deftest ox-zola-test-reads-zola-base-dir ()
  "Test that #+ZOLA_BASE_DIR is read into :hugo-base-dir."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "basic-post.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/ox-zola-test"))
        (should (equal (plist-get info :hugo-section) "posts"))))))

(ert-deftest ox-zola-test-reads-tags-and-categories ()
  "Test that #+ZOLA_TAGS and #+ZOLA_CATEGORIES are read."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola)))
             (tags (plist-get info :hugo-tags))
             (categories (plist-get info :hugo-categories)))
        ;; Tags may be a string or list depending on ox-hugo version
        (should (or (and (stringp tags) (string-match-p "emacs" tags))
                    (and (listp tags) (member "emacs" tags))))
        (should (or (and (stringp tags) (string-match-p "org-mode" tags))
                    (and (listp tags) (member "org-mode" tags))))
        (should (or (and (stringp categories) (string-match-p "tutorials" categories))
                    (and (listp categories) (member "tutorials" categories))))))))

(ert-deftest ox-zola-test-reads-hugo-base-dir ()
  "Test that #+HUGO_BASE_DIR maps to :hugo-base-dir."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-hugo-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/hugo-site"))
        (should (equal (plist-get info :hugo-section) "docs"))))))

;;; Output path tests

(ert-deftest ox-zola-test-output-path-with-base-dir ()
  "Test output path computation when base-dir is set."
  (let ((info (list :hugo-base-dir "/tmp/my-site" :hugo-section "posts" :hugo-slug "hello")))
    (should (equal (ox-zola--output-path info)
                   "/tmp/my-site/content/posts/hello.md"))))

(ert-deftest ox-zola-test-output-path-without-base-dir ()
  "Test output path returns just filename when base-dir is nil."
  (let ((info (list :hugo-base-dir nil :hugo-section nil :hugo-slug "test")))
    (should (equal (ox-zola--output-path info) "test.md"))))

(ert-deftest ox-zola-test-output-path-default-section ()
  "Test output path falls back to 'posts section."
  (let ((info (list :hugo-base-dir "/tmp/site" :hugo-section nil :hugo-slug "post")))
    (should (equal (ox-zola--output-path info)
                   "/tmp/site/content/posts/post.md"))))

;;; Export to buffer tests

(ert-deftest ox-zola-test-export-to-buffer-basic ()
  "Export basic-post.org to buffer and verify frontmatter."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-prefix-p "+++" content))
                (should (string-match-p "title = \"My First Post\"" content))
                (should (string-match-p "This is the content of my first post" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-test-export-to-buffer-taxonomies ()
  "Export with-taxonomies.org and verify [taxonomies] section."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let ((export-buf (ox-zola-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "emacs" content))
                (should (string-match-p "org-mode" content))
                (should (string-match-p "tutorials" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-test-export-to-buffer-all-metadata ()
  "Export with-all-metadata.org and verify all frontmatter fields."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "title = \"All Metadata Post\"" content))
                (should (string-match-p "slug" content))
                (should (string-match-p "my-custom-slug" content))
                (should (string-match-p "draft" content))
                (should (string-match-p "weight" content))
                (should (string-match-p "\\[taxonomies\\]" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

;;; File export tests

(ert-deftest ox-zola-test-export-to-file ()
  "Export basic-post.org to a temp Zola site directory and verify output."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert (ox-zola-test--read-fixture "basic-post.org"))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+zola_base_dir:.*" nil t)
            (replace-match (format "#+zola_base_dir: %s" temp-dir)))
          (org-mode)
          (let ((result (ox-zola-export-to-md)))
            (should (file-exists-p result))
            (should (string-suffix-p ".md" result))
            (should (string-prefix-p temp-dir result))
            (with-temp-buffer
              (insert-file-contents result)
              (let ((content (buffer-string)))
                (should (string-match-p "title = \"My First Post\"" content))
                (should (string-match-p "This is the content of my first post" content))))))
      (delete-directory temp-dir :recursive))))

(ert-deftest ox-zola-test-export-to-file-taxonomies ()
  "Export with-taxonomies.org to temp dir and verify TOML taxonomies section."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert (ox-zola-test--read-fixture "with-taxonomies.org"))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+zola_base_dir:.*" nil t)
            (replace-match (format "#+zola_base_dir: %s" temp-dir)))
          (org-mode)
          (let ((result (ox-zola-export-to-md)))
            (should (file-exists-p result))
            (should (string-suffix-p ".md" result))
            (should (string-prefix-p temp-dir result))
            (with-temp-buffer
              (insert-file-contents result)
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "emacs" content))
                (should (string-match-p "org-mode" content))
                (should (string-match-p "tutorials" content))))))
      (delete-directory temp-dir :recursive))))

(provide 'ox-zola-test)
;;; ox-zola-test.el ends here
