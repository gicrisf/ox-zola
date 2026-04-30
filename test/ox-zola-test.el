;;; ox-zola-test.el --- Tests for ox-zola -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>

;;; Commentary:

;; ERT tests for ox-zola export backends.

;;; Code:

(require 'ert)
(require 'ox-zola)

;;; ox-zola entry point tests

(ert-deftest ox-zola-test-backend-variable-exists ()
  "Test that ox-zola-backend variable exists and has correct default."
  (should (boundp 'ox-zola-backend))
  (should (eq ox-zola-backend 'lite)))

(ert-deftest ox-zola-test-backend-variable-customizable ()
  "Test that ox-zola-backend is customizable."
  (should (custom-variable-p 'ox-zola-backend)))

(ert-deftest ox-zola-test-switch-backend ()
  "Test switching between backends."
  (let ((original ox-zola-backend))
    (unwind-protect
        (progn
          (ox-zola-switch-backend 'full)
          (should (eq ox-zola-backend 'full))
          (ox-zola-switch-backend 'lite)
          (should (eq ox-zola-backend 'lite)))
      (setq ox-zola-backend original))))

(ert-deftest ox-zola-test-ensure-backend-lite ()
  "Test that lite backend can be loaded."
  (let ((ox-zola-backend 'lite))
    (should (eq (ox-zola--ensure-backend) 'zola-lite))))

;;; ox-zola-lite tests

(ert-deftest ox-zola-lite-test-backend-exists ()
  "Test that the zola-lite backend is properly registered."
  (should (org-export-get-backend 'zola-lite)))

(ert-deftest ox-zola-lite-test-backend-derives-from-md ()
  "Test that zola-lite backend derives from md."
  (let ((backend (org-export-get-backend 'zola-lite)))
    (should (eq (org-export-backend-parent backend) 'md))))

(ert-deftest ox-zola-lite-test-build-frontmatter-basic ()
  "Test basic frontmatter generation in ox-zola-lite."
  (let ((info '(:title ("Test Title")
                :zola-title nil
                :author ("Test Author")
                :date nil
                :zola-date "2024-01-15"
                :zola-updated nil
                :zola-slug nil
                :zola-draft nil
                :zola-weight nil
                :zola-template nil
                :zola-description nil
                :zola-tags nil
                :zola-categories nil
                :zola-extra nil)))
    ;; Mock org-export-data to return the string directly
    (cl-letf (((symbol-function 'org-export-data)
               (lambda (data _info) (if (listp data) (car data) data))))
      (let ((result (ox-zola-lite--build-frontmatter info)))
        (should (string-prefix-p "+++" result))
        (should (string-match-p "title = \"Test Title\"" result))
        (should (string-match-p "date = 2024-01-15" result))
        (should (string-match-p "+++\n\n$" result))))))

(ert-deftest ox-zola-lite-test-build-frontmatter-taxonomies ()
  "Test that ox-zola-lite generates taxonomies section."
  (let ((info '(:title ("Test")
                :zola-title nil
                :author nil
                :date nil
                :zola-date nil
                :zola-updated nil
                :zola-slug nil
                :zola-draft nil
                :zola-weight nil
                :zola-template nil
                :zola-description nil
                :zola-tags ("emacs" "org-mode")
                :zola-categories ("tutorials")
                :zola-extra nil)))
    (cl-letf (((symbol-function 'org-export-data)
               (lambda (data _info) (if (listp data) (car data) data))))
      (let ((result (ox-zola-lite--build-frontmatter info)))
        (should (string-match-p "\\[taxonomies\\]" result))
        (should (string-match-p "tags = \\[\"emacs\", \"org-mode\"\\]" result))
        (should (string-match-p "categories = \\[\"tutorials\"\\]" result))))))

(ert-deftest ox-zola-lite-test-format-toml-value-string ()
  "Test TOML value formatting for strings."
  (should (equal "\"hello\"" (ox-zola-lite--format-toml-value "hello")))
  (should (equal "true" (ox-zola-lite--format-toml-value "true")))
  (should (equal "false" (ox-zola-lite--format-toml-value "false"))))

(ert-deftest ox-zola-lite-test-format-toml-value-list ()
  "Test TOML value formatting for lists."
  (should (equal "[\"a\", \"b\", \"c\"]"
                 (ox-zola-lite--format-toml-value '("a" "b" "c")))))

(ert-deftest ox-zola-lite-test-format-toml-value-nil ()
  "Test TOML value formatting for nil."
  (should (null (ox-zola-lite--format-toml-value nil))))

(ert-deftest ox-zola-lite-test-menu-entry-registered ()
  "Test that the Zola-lite menu entry is registered."
  (let ((backend (org-export-get-backend 'zola-lite)))
    (should (org-export-backend-menu backend))))

(ert-deftest ox-zola-lite-test-options-alist ()
  "Test that ZOLA_* keywords are recognized."
  (let ((backend (org-export-get-backend 'zola-lite)))
    (let ((options (org-export-backend-options backend)))
      ;; Check some expected options exist
      (should (assq :zola-base-dir options))
      (should (assq :zola-section options))
      (should (assq :zola-tags options))
      (should (assq :zola-categories options)))))

(ert-deftest ox-zola-lite-test-full-keywords-recognized ()
  "Test that HUGO_* aliases are recognized (for compatibility)."
  (let ((backend (org-export-get-backend 'zola-lite)))
    (let ((options (org-export-backend-options backend)))
      ;; HUGO_* keywords should map to the same plist keys
      (should (assq :zola-base-dir options))
      (should (assq :zola-section options)))))

;;; ox-zola-full tests (conditional on ox-hugo availability)

(ert-deftest ox-zola-full-test-backend-loading ()
  "Test that full backend loading is handled correctly."
  (let ((ox-zola-backend 'lite))
    (should (eq (ox-zola--ensure-backend) 'zola-lite))))

;; Only run full backend tests if ox-hugo is available
(when (require 'ox-hugo nil t)
  (require 'ox-zola-full)

  (ert-deftest ox-zola-full-test-gen-front-matter-toml-delimiters ()
    "Test that TOML frontmatter uses +++ delimiters."
    (let ((data '((title . "Test Post"))))
      (should (string-prefix-p "+++"
                                (ox-zola--gen-front-matter data "toml")))
      (should (string-match-p "\n\\+\\+\\+\n$"
                              (ox-zola--gen-front-matter data "toml")))))

  (ert-deftest ox-zola-full-test-gen-front-matter-taxonomies ()
    "Test that taxonomies are nested under [taxonomies] section."
    (let ((data '((title . "Test Post")
                  (tags . ("emacs" "org"))
                  (categories . ("tutorials")))))
      (let ((result (ox-zola--gen-front-matter data "toml")))
        (should (string-match-p "taxonomies" result))
        (should (string-match-p "emacs" result))
        (should (string-match-p "org" result))
        (should (string-match-p "tutorials" result)))))

  (ert-deftest ox-zola-full-test-special-block-properties-customizable ()
    "Test that special block properties variable is customizable."
    (should (custom-variable-p 'ox-zola-special-block-type-properties))))

;;; End-to-end tests with fixture data

(defvar ox-zola-test-data-dir
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test Org fixture files.")

(defun ox-zola-test--read-fixture (filename)
  "Read the contents of test fixture FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ox-zola-test-data-dir))
    (buffer-string)))

;;; Keyword recognition tests (lite backend)

(ert-deftest ox-zola-lite-test-reads-zola-base-dir ()
  "Test that #+ZOLA_BASE_DIR is read into :zola-base-dir."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "basic-post.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-base-dir) "/tmp/ox-zola-test"))
        (should (equal (plist-get info :zola-section) "posts"))))))

(ert-deftest ox-zola-lite-test-reads-tags-and-categories ()
  "Test that #+ZOLA_TAGS and #+ZOLA_CATEGORIES are read."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-tags) '("emacs" "org-mode")))
        (should (equal (plist-get info :zola-categories) '("tutorials")))))))

(ert-deftest ox-zola-lite-test-reads-all-metadata ()
  "Test that all ZOLA_* keywords are read correctly."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-base-dir) "/tmp/ox-zola-test"))
        (should (equal (plist-get info :zola-section) "articles"))
        (should (equal (plist-get info :zola-title) "Overridden Title"))
        (should (equal (plist-get info :zola-slug) "my-custom-slug"))
        (should (equal (plist-get info :zola-date) "2024-06-01"))
        (should (equal (plist-get info :zola-updated) "2024-06-15"))
        (should (equal (plist-get info :zola-draft) "true"))
        (should (equal (plist-get info :zola-weight) "10"))
        (should (equal (plist-get info :zola-template) "page.html"))
        (should (equal (plist-get info :zola-description) "A test description"))
        (should (equal (plist-get info :zola-tags) '("test")))
        (should (equal (plist-get info :zola-categories) '("dev")))))))

;;; HUGO_* keyword compatibility tests

(ert-deftest ox-zola-lite-test-reads-hugo-base-dir ()
  "Test that #+HUGO_BASE_DIR maps to :zola-base-dir."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-hugo-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-base-dir) "/tmp/hugo-site"))
        (should (equal (plist-get info :zola-section) "docs"))))))

(ert-deftest ox-zola-lite-test-hugo-keyword-takes-precedence ()
  "Test that HUGO_BASE_DIR overrides ox-zola-base-dir custom variable."
  (let ((org-inhibit-startup t)
        (ox-zola-base-dir "/tmp/custom-dir"))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-hugo-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        ;; File keyword overrides custom variable
        (should (equal (plist-get info :zola-base-dir) "/tmp/hugo-site"))))))

(ert-deftest ox-zola-lite-test-returns-defaults-without-keywords ()
  "Test that without ZOLA_* keywords, custom variable defaults are used."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "no-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        ;; base-dir defaults to nil (ox-zola-base-dir default)
        (should (null (plist-get info :zola-base-dir)))
        ;; section defaults to "posts" (ox-zola-section default)
        (should (equal (plist-get info :zola-section) "posts"))))))

;;; Output path tests

(ert-deftest ox-zola-lite-test-output-path-with-base-dir ()
  "Test output path computation when base-dir is set."
  (let ((info (list :zola-base-dir "/tmp/my-site" :zola-section "posts" :zola-slug "hello")))
    (should (equal (ox-zola-lite--output-path info)
                   "/tmp/my-site/content/posts/hello.md"))))

(ert-deftest ox-zola-lite-test-output-path-without-base-dir ()
  "Test output path returns just filename when base-dir is nil."
  (let ((info (list :zola-base-dir nil :zola-section nil :zola-slug "test")))
    (should (equal (ox-zola-lite--output-path info) "test.md"))))

(ert-deftest ox-zola-lite-test-output-path-default-section ()
  "Test output path falls back to 'posts section."
  (let ((info (list :zola-base-dir "/tmp/site" :zola-section nil :zola-slug "post")))
    (should (equal (ox-zola-lite--output-path info)
                   "/tmp/site/content/posts/post.md"))))

;;; End-to-end export to buffer tests

(ert-deftest ox-zola-lite-test-export-to-buffer-basic ()
  "Export basic-post.org to buffer and verify frontmatter."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-lite-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-prefix-p "+++" content))
                (should (string-match-p "title = \"My First Post\"" content))
                (should (string-match-p "date = 2024-01-15" content))
                (should (string-match-p "This is the content of my first post" content))
                (should (string-match-p "\\+\\+\\+\n\n" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-lite-test-export-to-buffer-taxonomies ()
  "Export with-taxonomies.org and verify [taxonomies] section."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let ((export-buf (ox-zola-lite-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "tags = \\[\"emacs\", \"org-mode\"\\]" content))
                (should (string-match-p "categories = \\[\"tutorials\"\\]" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-lite-test-export-to-buffer-all-metadata ()
  "Export with-all-metadata.org and verify all frontmatter fields."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-lite-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "title = \"Overridden Title\"" content))
                (should (string-match-p "date = 2024-06-01" content))
                (should (string-match-p "updated = 2024-06-15" content))
                (should (string-match-p "slug = \"my-custom-slug\"" content))
                (should (string-match-p "draft = true" content))
                (should (string-match-p "weight = 10" content))
                (should (string-match-p "template = \"page.html\"" content))
                (should (string-match-p "description = \"A test description\"" content))
                (should (string-match-p "\\[taxonomies\\]" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

  (ert-deftest ox-zola-lite-test-export-to-buffer-no-keywords ()
  "Export no-keywords.org — uses Org #+TITLE, no ZOLA metadata."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "no-keywords.org"))
      (org-mode)
      (let ((export-buf (ox-zola-lite-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "title = \"Defaults Post\"" content))
                (should-not (string-match-p "slug =" content))
                (should-not (string-match-p "draft =" content))
                (should (string-match-p "ZOLA" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

;;; ox-zola-lite custom variable tests
;; These test whether the ox-zola-base-dir and ox-zola-section custom
;; variables are respected as defaults when no keyword is in the file.

(ert-deftest ox-zola-lite-test-custom-var-base-dir-wired ()
  "Test that ox-zola-base-dir custom variable is used as default."
  (let ((org-inhibit-startup t)
        (ox-zola-base-dir "/tmp/custom-base-dir"))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "no-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-base-dir) "/tmp/custom-base-dir"))))))

(ert-deftest ox-zola-lite-test-custom-var-section-wired ()
  "Test that ox-zola-section custom variable is used as default."
  (let ((org-inhibit-startup t)
        (ox-zola-section "blog"))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "no-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-lite)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-lite))))
        (should (equal (plist-get info :zola-section) "blog"))))))

;;; End-to-end file export tests (lite backend)

(ert-deftest ox-zola-lite-test-export-to-file ()
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
          (let ((result (ox-zola-lite-export-to-md)))
            (should (file-exists-p result))
            (should (string-suffix-p ".md" result))
            (should (string-prefix-p temp-dir result))
            (with-temp-buffer
              (insert-file-contents result)
              (let ((content (buffer-string)))
                (should (string-match-p "title = \"My First Post\"" content))
                (should (string-match-p "date = 2024-01-15" content))
                (should (string-match-p "This is the content of my first post" content))))))
      (delete-directory temp-dir :recursive))))

(ert-deftest ox-zola-lite-test-export-to-file-taxonomies ()
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
          (let ((result (ox-zola-lite-export-to-md)))
            (should (file-exists-p result))
            (should (string-suffix-p ".md" result))
            (should (string-prefix-p temp-dir result))
            (with-temp-buffer
              (insert-file-contents result)
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "\"emacs\", \"org-mode\"" content))
                (should (string-match-p "\"tutorials\"" content))))))
      (delete-directory temp-dir :recursive))))

(ert-deftest ox-zola-lite-test-export-to-file-no-keywords ()
  "Export no-keywords.org — ZOLA_BASE_DIR missing, should still export to buffer."
  ;; Note: without base_dir, ox-zola-lite-export-to-md returns just filename
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-test--read-fixture "no-keywords.org"))
      (org-mode)
      (let ((result (ox-zola-lite-export-to-md)))
        ;; Without base_dir, output is just filename.md in default-directory
        (should (string-suffix-p ".md" result))
        (when (file-exists-p result)
          (delete-file result))))))

;;; Full backend end-to-end tests (conditional on ox-hugo)

(when (require 'ox-hugo nil t)
  (require 'ox-zola-full)

  (ert-deftest ox-zola-full-test-reads-zola-keywords ()
    "Test that ZOLA_* keywords are read via the pseudo hugo backend."
    (let ((org-inhibit-startup t)
          (orig-backend (org-export-get-backend 'hugo)))
      (with-temp-buffer
        (insert (ox-zola-test--read-fixture "basic-post.org"))
        (org-mode)
        (ox-zola--set-pseudohugo-backend)
        (unwind-protect
            (let* ((info (org-combine-plists
                          (org-export--get-export-attributes 'hugo)
                          (org-export--get-buffer-attributes)
                          (org-export-get-environment 'hugo))))
              (should (equal (plist-get info :hugo-base-dir) "/tmp/ox-zola-test"))
              (should (equal (plist-get info :hugo-section) "posts")))
          (org-export-register-backend orig-backend)))))

  (ert-deftest ox-zola-full-test-export-to-buffer ()
    "Export basic-post.org through full backend to buffer."
    (let ((org-inhibit-startup t))
      (with-temp-buffer
        (insert (ox-zola-test--read-fixture "basic-post.org"))
        (org-mode)
        (let ((export-buf (ox-zola-full-export-as-md)))
          (unwind-protect
              (with-current-buffer export-buf
                (let ((content (buffer-string)))
                  (should (string-prefix-p "+++" content))
                  (should (string-match-p "title = \"My First Post\"" content))))
            (when (buffer-live-p export-buf)
              (kill-buffer export-buf))))))))

(provide 'ox-zola-test)
;;; ox-zola-test.el ends here
