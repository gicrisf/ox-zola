;;; ox-zola-alt-test.el --- Tests for ox-zola-alt -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the alt Zola export backend (ox-zola-alt.el).
;; Tests are organized by phase following the incremental development plan.

;;; Code:

(require 'ert)
(require 'ox-zola-alt)

(defvar ox-zola-alt-test-data-dir
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test Org fixture files.")

(defun ox-zola-alt-test--read-fixture (filename)
  "Read the contents of test fixture FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ox-zola-alt-test-data-dir))
    (buffer-string)))

;;; Phase 0: Backend registration

(ert-deftest ox-zola-alt-test-backend-exists ()
  "Test that the zola-alt backend is properly registered."
  (should (org-export-get-backend 'zola-alt)))

(ert-deftest ox-zola-alt-test-backend-derives-from-hugo ()
  "Test that zola-alt backend derives from hugo."
  (let ((backend (org-export-get-backend 'zola-alt)))
    (should (eq (org-export-backend-parent backend) 'hugo))))

(ert-deftest ox-zola-alt-test-basic-export ()
  "Test that basic export works (Hugo-style or Zola-style FM)."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Should have frontmatter delimiters
                (should (string-match-p "^\\+\\+\\+" content))
                ;; Should have title
                (should (string-match-p "title" content))
                ;; Should have body content
                (should (string-match-p "This is the content" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

;;; Phase 1: Frontmatter transformation

(ert-deftest ox-zola-alt-test-taxonomies-section ()
  "Verify [taxonomies] section appears in output."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "emacs" content))
                (should (string-match-p "org-mode" content))
                (should (string-match-p "tutorials" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-alt-test-updated-field ()
  "Verify lastmod → updated rename."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Should have 'updated' not 'lastmod'
                (should (string-match-p "updated" content))
                (should (string-match-p "2024-06-15" content))
                (should-not (string-match-p "lastmod" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-alt-test-template-field ()
  "Verify layout → template rename."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Should have 'template' not 'layout'
                (should (string-match-p "template" content))
                (should (string-match-p "page.html" content))
                (should-not (string-match-p "\\blayout\\b" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-alt-test-hugo-unaffected ()
  "Verify regular ox-hugo exports still produce Hugo-style FM.
Loading ox-zola-alt should NOT change ox-hugo behavior."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n#+hugo_tags: foo bar\n\nBody.")
      (org-mode)
      ;; Export with plain hugo backend (not zola-alt)
      (let ((export-buf (org-export-to-buffer 'hugo "*Hugo Test*")))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Hugo uses tags at root level, not [taxonomies]
                (should (string-match-p "tags" content))
                ;; Hugo uses --- for YAML by default, or +++ for TOML
                ;; Just check it doesn't have [taxonomies]
                (should-not (string-match-p "\\[taxonomies\\]" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

;;; Phase 2: Keyword aliases

(ert-deftest ox-zola-alt-test-zola-keywords-recognized ()
  "Test that ZOLA_* keywords are read via zola-alt backend."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "basic-post.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-alt)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-alt))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/ox-zola-test"))
        (should (equal (plist-get info :hugo-section) "posts"))))))

(ert-deftest ox-zola-alt-test-hugo-keywords-recognized ()
  "Test that native HUGO_* keywords work through zola-alt."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-hugo-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-alt)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-alt))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/hugo-site"))
        (should (equal (plist-get info :hugo-section) "docs"))))))

(ert-deftest ox-zola-alt-test-hugo-base-dir-used-in-output-path ()
  "Test that HUGO_BASE_DIR is used for output path computation."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-alt-hugo-test-" t)))
    (unwind-protect
        (with-temp-buffer
          ;; Use HUGO_* keywords (not ZOLA_*)
          (insert (format "#+hugo_base_dir: %s\n" temp-dir))
          (insert "#+hugo_section: articles\n")
          (insert "#+title: Hugo Path Test\n\n")
          (insert "Content.\n")
          (org-mode)
          (let ((result (ox-zola-alt-export-to-md)))
            (should (file-exists-p result))
            (should (string-prefix-p temp-dir result))
            (should (string-match-p "/content/articles/" result))))
      (delete-directory temp-dir :recursive))))

;;; Phase 3: Single frontmatter block

(ert-deftest ox-zola-alt-test-single-frontmatter ()
  "Verify exactly one frontmatter block (2 +++ markers)."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string))
                    (fm-count 0)
                    (pos 0))
                (while (string-match "^\\+\\+\\+" content pos)
                  (setq fm-count (1+ fm-count))
                  (setq pos (match-end 0)))
                ;; Exactly 2: opening +++ and closing +++
                (should (= fm-count 2))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

;;; Phase 5: Edge cases

(ert-deftest ox-zola-alt-test-all-metadata ()
  "Test export with all metadata fields."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-alt-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "slug" content))
                (should (string-match-p "my-custom-slug" content))
                (should (string-match-p "draft" content))
                (should (string-match-p "weight" content))
                (should (string-match-p "\\[taxonomies\\]" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-alt-test-no-keywords ()
  "Test export with no ZOLA_/HUGO_ keywords at all.
This also tests nil :hugo-base-dir handling."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "no-keywords.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-alt-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Defaults Post" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-alt-test-nil-base-dir-no-crash ()
  "Test that nil :hugo-base-dir doesn't crash export.
Regression test for org-hugo--copy-ltximg-maybe crash."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      ;; Minimal org content with no base-dir
      (insert "#+title: No Base Dir\n\nContent.")
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-alt-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "No Base Dir" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export crashed with nil base-dir:" err)))))))

(ert-deftest ox-zola-alt-test-macro-properties ()
  "Test export with :PROPERTIES:, #+MACRO, custom front matter."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-macro-properties.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-alt-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Comparison Post" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-alt-test-closed-subtree ()
  "Test export with CLOSED, :PROPERTIES:, src blocks, id: links."
  (let ((org-inhibit-startup t)
        (org-export-with-broken-links t))
    (with-temp-buffer
      (insert (ox-zola-alt-test--read-fixture "with-closed-subtree.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-alt-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Github clone" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-alt-test-file-export ()
  "Test file export (ox-zola-alt-export-to-md)."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-alt-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert (ox-zola-alt-test--read-fixture "hugo-only.org"))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+hugo_base_dir:.*" nil t)
            (replace-match (format "#+hugo_base_dir: %s" temp-dir)))
          (org-mode)
          (let ((result (ox-zola-alt-export-to-md)))
            (should (file-exists-p result))
            (should (string-suffix-p ".md" result))
            (should (string-prefix-p temp-dir result))
            (with-temp-buffer
              (insert-file-contents result)
              (let ((content (buffer-string)))
                (should (string-prefix-p "+++" content))
                (should (string-match-p "title = \"Hugo Only Post\"" content))))))
      (delete-directory temp-dir :recursive))))

;;; End-to-end tests with persistent build directory

(defvar ox-zola-alt-test-repo-root
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root directory.")

(defvar ox-zola-alt-test-build-dir
  (expand-file-name "build/test-output" ox-zola-alt-test-repo-root)
  "Build directory for E2E test artifacts.")

(defun ox-zola-alt-test--clean-build-dir ()
  "Ensure the build directory exists."
  (unless (file-directory-p ox-zola-alt-test-build-dir)
    (make-directory ox-zola-alt-test-build-dir t)))

(ert-deftest ox-zola-alt-test-e2e-zola-keywords-output-path ()
  "E2E: ZOLA_* keywords produce correct output path."
  (let* ((org-inhibit-startup t)
         (fixture-file (expand-file-name "e2e-zola-keywords.org"
                                         ox-zola-alt-test-data-dir))
         (base-dir (expand-file-name "zola-keywords"
                                     ox-zola-alt-test-build-dir))
         (expected-dir (expand-file-name "content/posts" base-dir)))
    (ox-zola-alt-test--clean-build-dir)
    (with-current-buffer (find-file-noselect fixture-file)
      (unwind-protect
          (progn
            ;; Replace placeholder with actual path
            (goto-char (point-min))
            (while (search-forward "__ZOLA_BASE_DIR__" nil t)
              (replace-match base-dir t t))
            (let ((result (ox-zola-alt-export-to-md)))
              (should (file-exists-p result))
              (should (string-prefix-p expected-dir result))
              (should (string-suffix-p "e2e-zola-keywords.md" result))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(ert-deftest ox-zola-alt-test-e2e-hugo-keywords-output-path ()
  "E2E: HUGO_* keywords produce correct output path."
  (let* ((org-inhibit-startup t)
         (fixture-file (expand-file-name "e2e-hugo-keywords.org"
                                         ox-zola-alt-test-data-dir))
         (base-dir (expand-file-name "hugo-keywords"
                                     ox-zola-alt-test-build-dir))
         (expected-dir (expand-file-name "content/articles" base-dir)))
    (ox-zola-alt-test--clean-build-dir)
    (with-current-buffer (find-file-noselect fixture-file)
      (unwind-protect
          (progn
            ;; Replace placeholder with actual path
            (goto-char (point-min))
            (while (search-forward "__HUGO_BASE_DIR__" nil t)
              (replace-match base-dir t t))
            (let ((result (ox-zola-alt-export-to-md)))
              (should (file-exists-p result))
              (should (string-prefix-p expected-dir result))
              (should (string-suffix-p "e2e-hugo-keywords.md" result))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(provide 'ox-zola-alt-test)
;;; ox-zola-alt-test.el ends here
