(defun gtest-list-tests ()
  "Run the gtest executable with --gtest_list_tests and return the parsed output."
  (let* ((output (shell-command-to-string (format "%s --gtest_list_tests" gtest-executable)))
         (lines (split-string output "\n" t)))
    (gtest-parse-test-list lines)))

(defun gtest-parse-test-list (lines)
  (let ((result nil))
    (dolist (line lines result)
      (if (string-suffix-p "." line)
          (setq current-suite (substring line 0 -1))
        (let ((test-case (substring line 2 nil)))
             (push (list current-suite test-case) result))))))

(define-derived-mode gtest-tree-mode tabulated-list-mode "GTest Tree"
  "Major mode for displaying gtest trees."
  (setq tabulated-list-format [("Test" 50 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun gtest-display-tests ()
  "Display the list of tests from the gtest executable in a buffer."
  (let ((tests (gtest-list-tests)))
    (with-current-buffer (get-buffer-create "*GTest Tests*")
      (gtest-tree-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (test)
                      (list test (vector test)))
                    tests))
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))



(defun gtest-run-test ()
  "Run the test at point."
  (interactive)
  (message "Run da test")
  (let* ((entry (tabulated-list-get-entry))
         (suite (aref entry 0))
         (test (aref entry 1))
         (command (format "%s --gtest_filter=%s.%s" gtest-executable suite test))))
  )

(define-key gtest-tree-mode-map (kbd "RET") 'gtest-run-test)

(require 'tree-widget)

(defun my-create-tree-widget ()
  "Do thing."
  (interactive)
  (with-current-buffer
      (get-buffer-create "*my-tree-widget*")
                (widget-create
                 'tree-widget
                 :open t
                 :tag "one"
                 :args
                 (list (widget-convert
                        'tree-widget
                        :tag "two")))
    (switch-to-buffer (current-buffer))))

