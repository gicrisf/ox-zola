;;; ox-zola-full-test.el --- Tests for ox-zola-full -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the full Zola export backend (ox-zola-full.el).
;; Tests are organized by phase following the incremental development plan.

;;; Code:

(require 'ert)
(require 'ox-zola-full)

(defvar ox-zola-full-test-data-dir
  (expand-file-name "data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test Org fixture files.")

(defun ox-zola-full-test--read-fixture (filename)
  "Read the contents of test fixture FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ox-zola-full-test-data-dir))
    (buffer-string)))

;;; Phase 0: Backend registration

(ert-deftest ox-zola-full-test-backend-exists ()
  "Test that the zola-full backend is properly registered."
  (should (org-export-get-backend 'zola-full)))

(ert-deftest ox-zola-full-test-backend-derives-from-hugo ()
  "Test that zola-full backend derives from hugo."
  (let ((backend (org-export-get-backend 'zola-full)))
    (should (eq (org-export-backend-parent backend) 'hugo))))

(ert-deftest ox-zola-full-test-basic-export ()
  "Test that basic export works (Hugo-style or Zola-style FM)."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
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

(ert-deftest ox-zola-full-test-taxonomies-section ()
  "Verify [taxonomies] section appears in output."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-taxonomies.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "\\[taxonomies\\]" content))
                (should (string-match-p "emacs" content))
                (should (string-match-p "org-mode" content))
                (should (string-match-p "tutorials" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-updated-field ()
  "Verify lastmod → updated rename."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Should have 'updated' not 'lastmod'
                (should (string-match-p "updated" content))
                (should (string-match-p "2024-06-15" content))
                (should-not (string-match-p "lastmod" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-template-field ()
  "Verify layout → template rename."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                ;; Should have 'template' not 'layout'
                (should (string-match-p "template" content))
                (should (string-match-p "page.html" content))
                (should-not (string-match-p "\\blayout\\b" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-hugo-unaffected ()
  "Verify regular ox-hugo exports still produce Hugo-style FM.
Loading ox-zola-full should NOT change ox-hugo behavior."
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

(ert-deftest ox-zola-full-test-zola-keywords-recognized ()
  "Test that ZOLA_* keywords are read via zola-full backend."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "basic-post.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-full)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-full))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/ox-zola-test"))
        (should (equal (plist-get info :hugo-section) "posts"))))))

(ert-deftest ox-zola-full-test-hugo-keywords-recognized ()
  "Test that native HUGO_* keywords work through zola-alt."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-hugo-keywords.org"))
      (org-mode)
      (let* ((info (org-combine-plists
                    (org-export--get-export-attributes 'zola-full)
                    (org-export--get-buffer-attributes)
                    (org-export-get-environment 'zola-full))))
        (should (equal (plist-get info :hugo-base-dir) "/tmp/hugo-site"))
        (should (equal (plist-get info :hugo-section) "docs"))))))

(ert-deftest ox-zola-full-test-hugo-base-dir-used-in-output-path ()
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
          (let ((result (ox-zola-full-export-to-md)))
            (should (file-exists-p result))
            (should (string-prefix-p temp-dir result))
            (should (string-match-p "/content/articles/" result))))
      (delete-directory temp-dir :recursive))))

;;; Phase 3: Single frontmatter block

(ert-deftest ox-zola-full-test-single-frontmatter ()
  "Verify exactly one frontmatter block (2 +++ markers)."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "basic-post.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
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

(ert-deftest ox-zola-full-test-all-metadata ()
  "Test export with all metadata fields."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-all-metadata.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
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

(ert-deftest ox-zola-full-test-no-keywords ()
  "Test export with no ZOLA_/HUGO_ keywords at all.
This also tests nil :hugo-base-dir handling."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "no-keywords.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-full-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Defaults Post" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-full-test-nil-base-dir-no-crash ()
  "Test that nil :hugo-base-dir doesn't crash export.
Regression test for org-hugo--copy-ltximg-maybe crash."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      ;; Minimal org content with no base-dir
      (insert "#+title: No Base Dir\n\nContent.")
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-full-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "No Base Dir" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export crashed with nil base-dir:" err)))))))

(ert-deftest ox-zola-full-test-macro-properties ()
  "Test export with :PROPERTIES:, #+MACRO, custom front matter."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-macro-properties.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-full-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Comparison Post" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-full-test-closed-subtree ()
  "Test export with CLOSED, :PROPERTIES:, src blocks, id: links."
  (let ((org-inhibit-startup t)
        (org-export-with-broken-links t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-closed-subtree.org"))
      (org-mode)
      (condition-case err
          (let ((export-buf (ox-zola-full-export-as-md)))
            (unwind-protect
                (with-current-buffer export-buf
                  (let ((content (buffer-string)))
                    (should (string-prefix-p "+++" content))
                    (should (string-match-p "Github clone" content))))
              (when (buffer-live-p export-buf)
                (kill-buffer export-buf))))
        (error
         (ert-fail (list "Export failed with error:" err)))))))

(ert-deftest ox-zola-full-test-file-export ()
  "Test file export (ox-zola-full-export-to-md)."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-full-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (insert (ox-zola-full-test--read-fixture "hugo-only.org"))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+hugo_base_dir:.*" nil t)
            (replace-match (format "#+hugo_base_dir: %s" temp-dir)))
          (org-mode)
          (let ((result (ox-zola-full-export-to-md)))
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

(defvar ox-zola-full-test-repo-root
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root directory.")

(defvar ox-zola-full-test-build-dir
  (expand-file-name "build/test-output" ox-zola-full-test-repo-root)
  "Build directory for E2E test artifacts.")

(defun ox-zola-full-test--clean-build-dir ()
  "Ensure the build directory exists."
  (unless (file-directory-p ox-zola-full-test-build-dir)
    (make-directory ox-zola-full-test-build-dir t)))

(ert-deftest ox-zola-full-test-e2e-zola-keywords-output-path ()
  "E2E: ZOLA_* keywords produce correct output path."
  (let* ((org-inhibit-startup t)
         (fixture-file (expand-file-name "e2e-zola-keywords.org"
                                         ox-zola-full-test-data-dir))
         (base-dir (expand-file-name "zola-keywords"
                                     ox-zola-full-test-build-dir))
         (expected-dir (expand-file-name "content/posts" base-dir)))
    (ox-zola-full-test--clean-build-dir)
    (with-current-buffer (find-file-noselect fixture-file)
      (unwind-protect
          (progn
            ;; Replace placeholder with actual path
            (goto-char (point-min))
            (while (search-forward "__ZOLA_BASE_DIR__" nil t)
              (replace-match base-dir t t))
            (let ((result (ox-zola-full-export-to-md)))
              (should (file-exists-p result))
              (should (string-prefix-p expected-dir result))
              (should (string-suffix-p "e2e-zola-keywords.md" result))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(ert-deftest ox-zola-full-test-e2e-hugo-keywords-output-path ()
  "E2E: HUGO_* keywords produce correct output path."
  (let* ((org-inhibit-startup t)
         (fixture-file (expand-file-name "e2e-hugo-keywords.org"
                                         ox-zola-full-test-data-dir))
         (base-dir (expand-file-name "hugo-keywords"
                                     ox-zola-full-test-build-dir))
         (expected-dir (expand-file-name "content/articles" base-dir)))
    (ox-zola-full-test--clean-build-dir)
    (with-current-buffer (find-file-noselect fixture-file)
      (unwind-protect
          (progn
            ;; Replace placeholder with actual path
            (goto-char (point-min))
            (while (search-forward "__HUGO_BASE_DIR__" nil t)
              (replace-match base-dir t t))
            (let ((result (ox-zola-full-export-to-md)))
              (should (file-exists-p result))
              (should (string-prefix-p expected-dir result))
              (should (string-suffix-p "e2e-hugo-keywords.md" result))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

;;; Phase 6: Link handling (Zola-compatible, no relref)

(ert-deftest ox-zola-full-test-no-relref-in-output ()
  "Verify no relref appears in exported Markdown."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert (ox-zola-full-test--read-fixture "with-links.org"))
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-same-file-heading-link ()
  "Verify same-file heading links use Zola-compatible #anchor format."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n\n[[*Some Heading][Link]]\n\n* Some Heading\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))
                (should (string-match-p "\\[Link\\](#" content))
                (should (string-match-p "some-heading" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-custom-id-link ()
  "Verify custom-id links use Zola-compatible #anchor format."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n\n[[#custom-one][custom heading]]\n\n\
* Heading One\n:PROPERTIES:\n:CUSTOM_ID: custom-one\n:END:\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))
                (should (string-match-p "\\[custom heading\\](#custom-one)" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-heading-title-link ()
  "Verify links by heading title use Zola-compatible #anchor format."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n\n[[*Some Heading][link]]\n\n* Some Heading\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))
                (should (string-match-p "\\[link\\](#" content))
                (should (string-match-p "some-heading" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-external-url-unchanged ()
  "Verify external URLs pass through unchanged."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n\n[[https://example.com][Example]]\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should (string-match-p "\\[Example\\](https://example\\.com)" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-file-link-no-relref ()
  "Verify file: links do not use relref."
  (let ((org-inhibit-startup t))
    (with-temp-buffer
      (insert "#+title: Test\n\n[[file:other.org][other post]]\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))
                ;; Should use Zola @/ path syntax
                (should (string-match-p "\\[other post\\](@/" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-file-link-empty-section ()
  "Verify file: links with empty section produce @/post.md not @//post.md."
  (let ((org-inhibit-startup t)
        (org-hugo-section ""))  ; Override default to empty
    (with-temp-buffer
      (insert "#+title: Test\n\n[[file:other.org][other post]]\n")
      (org-mode)
      (let ((export-buf (ox-zola-full-export-as-md)))
        (unwind-protect
            (with-current-buffer export-buf
              (let ((content (buffer-string)))
                (should-not (string-match-p "relref" content))
                ;; Should NOT have double slash
                (should-not (string-match-p "@//" content))
                ;; Should have single slash path (no section)
                (should (string-match-p "\\[other post\\](@/other\\.md)" content))))
          (when (buffer-live-p export-buf)
            (kill-buffer export-buf)))))))

(ert-deftest ox-zola-full-test-file-link-with-anchor ()
  "Verify file: links with heading anchor use Zola @/ path + #anchor."
  (let* ((org-inhibit-startup t)
         (temp-dir (make-temp-file "ox-zola-link-test-" t))
         (base-dir (expand-file-name "zola-site" temp-dir))
         (content-dir (expand-file-name "content/posts" base-dir))
         (target-file (expand-file-name "other.org" content-dir)))
    (unwind-protect
        (progn
          (make-directory content-dir t)
          (with-temp-file target-file
            (insert "* Some Heading\n"))
          (with-temp-buffer
            (let ((buffer-file-name (expand-file-name "current.org" content-dir)))
              (insert (format "#+title: Test\n#+zola_base_dir: %s\n#+zola_section: posts\n"
                              base-dir))
              (insert (format "[[file:%s::*Some Heading][link]]\n" target-file))
              (org-mode)
              (let ((export-buf (ox-zola-full-export-as-md)))
                (unwind-protect
                    (with-current-buffer export-buf
                      (let ((content (buffer-string)))
                        (should-not (string-match-p "relref" content))
                        (should (string-match-p "\\[link\\](@/posts/other\\.md#" content))))
                  (when (buffer-live-p export-buf)
                    (kill-buffer export-buf)))))))
      (delete-directory temp-dir :recursive))))

(provide 'ox-zola-full-test)
;;; ox-zola-full-test.el ends here
