;; -*- lexical-binding: t;-*-

(require 'tree-widget)

;; Things to do
;;
;; Will need to re run my-tree-widget-icon-from-status
;; when you run a unit test or suite to enable the change
;; from red to green.
;;
;; If you run a single unit test, then you need to check if
;; all the unit tests in that suite have been run and update
;; test suite status if true.
;;
;; Create keybinding to refresh all unit tests and regenerate tree




;; TODO: Support mulitple executables. Sometimes you want to do this.
(defvar gtest-executable nil
  "The path to your gtest test executable")

(defun gtest-set-executable (gtest-executable-path)
  "Set the gtest binary executable. Absolute Path."
  (interactive "f")
  (setq gtest-executable gtest-executable-path))

(defun gtest-list-tests ()
  "Run the gtest executable with --gtest_list_tests and return the parsed output."
  (let* ((output (shell-command-to-string (format "%s --gtest_list_tests" gtest-executable)))
         (lines (split-string output "\n" t)))
    (gtest-parse-test-list lines)))

(defun gtest-parse-test-list (lines)
  (let ((suites (list)))
    (dolist (line lines suites)
      (if (string-suffix-p "." line)
          (push (list (substring line 0 -1)) suites)
        (push (substring line 2 nil) (car suites))))))

;; (defun my-tree-widget-get-icon-from-status (tree)
;;   (let ((status (widget-get tree :status)))
;;     (message (format "tree status: %s" status))
;;     (cond
;;      ((eq status nil) 'tree-widget-empty-icon)
;;      ((string= status "error") 'tree-widget-empty-icon)
;;      ((string= status "pass") 'tree-widget-open-icon)
;;      ((string= status "fail") 'tree-widget-close-icon))))

(defun gtest-get-test-case-result (test-suite test-case)
  "Returns 1 if successful"
  (let* ((stdout (shell-command-to-string (format "%s --gtest_filter=%s.%s" gtest-executable test-suite test-case)))
         (pos-end (string-match (regexp-quote "[----------] Global test environment tear-down") stdout))
         (pos-fail (string-match (regexp-quote "[  FAILED  ]") stdout)))
    ; (message "stdout: %s" stdout)
    (if pos-end
        (if pos-fail
            "fail"
          "pass")
      "error")))

(defun gtest-make-tree-node (text &optional parent)
  (message "text: %s" text)
  (let ((new-node (list :parent parent
                        :text text
                        :children nil
                        :status nil
                        :visible t
                        :indent "")))
    (when parent
      (plist-put new-node :indent (concat "    " (plist-get parent :indent)))
      (plist-put parent :children (push new-node (plist-get parent :children)))
      )
    new-node))

(defun gtest-add-test-cases (test-suite-node test-list)
  "test-list is a list like this (test-case1 test-case2 ... test-suite-name)"
  (dolist (test-case (butlast test-list))
    (gtest-make-tree-node test-case test-suite-node)))

(defun gtest-create-tree ()
  (let ((test-suite-nodes))
    (dolist (tests (gtest-list-tests) test-suite-nodes) 
      (push (gtest-make-tree-node (car (last tests))) test-suite-nodes)
      (gtest-add-test-cases (car test-suite-nodes) tests)
      (message "::")
      )))

(defun gtest-get-node-string (node)
  (let ((status (plist-get node :status)))
    (format "%s%s %s\n"
            (plist-get node :indent)
            (cond
             ((string= status "pass") "p")
             ((string= status "fail") "f")
             ((string= status "error") "e")
             ("n"))
            (plist-get node :text))))

(defun gtest-display-tree--recursive (tree)
  (insert (gtest-get-node-string tree))
  (let ((child-nodes (plist-get tree :children)))
    (dolist (child-node child-nodes)      
      (gtest-display-tree--recursive child-node))
    ))

(defun gtest-get-buffer-create ()
  (get-buffer-create "GTest Tree"))

(defun gtest-display-tree (root-nodes)
  (let ((gtest-buffer (gtest-get-buffer-create)))
    (pop-to-buffer
     gtest-buffer
     (with-current-buffer gtest-buffer
       (erase-buffer)
       (dolist (root-node root-nodes)
         (gtest-display-tree--recursive root-node))))))

(defvar gtest-highlight-overlay nil
  "Overlay used to highlight the current line.")

(defun gtest-create-highlight ()
  "Toggle highlighting on the current line."
  (let ((gtest-buffer (gtest-get-buffer-create)))
    (with-current-buffer gtest-buffer
      (beginning-of-buffer)
      (setq gtest-highlight-overlay (make-overlay (line-beginning-position) (line-end-position) gtest-buffer))
      (overlay-put gtest-highlight-overlay 'face 'highlight))))

(defun gtest-move (direction)
  (let ((gtest-buffer (gtest-get-buffer-create)))
    (with-current-buffer gtest-buffer
      (goto-char (overlay-start gtest-highlight-overlay))
      (forward-line direction)
      (move-overlay gtest-highlight-overlay (line-beginning-position 1) (line-end-position 1) gtest-buffer)
      )
    ))

(defun gtest-move-down ()
  (interactive)
  (gtest-move 1))

(defun gtest-move-up ()
  (interactive)
  (gtest-move -1))

(defun gtest-update-test-case (test-suite-node test-case-node)
  (plist-put test-case-node :status
             (gtest-get-test-case-result
              (plist-get test-suite-node :text)
              (plist-get test-case-node :text)))
  (plist-get test-case-node :status)
  )

(defun gtest-update-test-suite (test-suite-node)
  (let ((test-suite (plist-get test-suite-node :text))
        has-error
        has-fail
        test-case-result)

    (message "test-suite %s" test-suite)
    
    (dolist (child-node (plist-get test-suite-node :children))
      (setq test-case-result (gtest-update-test-case test-suite-node child-node))
      (setq has-error (or has-error (string= "error" test-case-result)))
      (setq has-fail (or has-fail (string= "fail" test-case-result))))
    
    (plist-put test-suite-node :status
               (cond
                (has-error "error")
                (has-fail "fail")
                ("pass")))
    (plist-get test-suite-node :status)))

(defun gtest-redraw-node--recursive (line-number node)
  "redraw node at line number"
  (when (plist-get node :visible)
    (let ((original-point (point))
          (gtest-buffer (gtest-get-buffer-create)))
      (goto-line line-number)
      (kill-whole-line)
      (insert (gtest-get-node-string node))
      (goto-char original-point))
    (let ((count 1))
      (dolist (child-node (plist-get node :children))
        (gtest-redraw-node--recursive (+ line-number count) child-node)
        (cl-incf count)
        ))))

(defun gtest-redraw-node (line-number node)
  (gtest-redraw-node--recursive line-number node)
  (goto-line line-number)
  (move-overlay gtest-highlight-overlay (line-beginning-position) (line-end-position) (gtest-get-buffer-create))
  )

(defun gtest-update-test ()
  (interactive)
  (let ((count 0)
        test-case-node)
    (dolist (test-suite-node test-suite-nodes)
      (cl-incf count)
      (cond
       ((= count (line-number-at-pos))
        (gtest-update-test-suite test-suite-node)
        (gtest-redraw-node (line-number-at-pos) test-suite-node))
       ((< count (line-number-at-pos))
        (setq child_idx (- (line-number-at-pos) count))
        (setq children (plist-get test-suite-node :children))
        (when (<= child_idx (length children))
          (setq test-case-node (nth (- child_idx 1) children))
          (gtest-update-test-case test-suite-node test-case-node)
          (gtest-redraw-node (line-number-at-pos) test-case-node))
        (setq count (+ count (length children)))))
      )))

(defvar gtest-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'gtest-move-down)
    (define-key map (kbd "p") 'gtest-move-up)
    (define-key map (kbd "r") 'gtest-update-test)
    map))

(define-minor-mode gtest-mode  
  "A minor mode for displaying gtests"
  nil
  :global nil
  :lighter "GTest"
  :keymap gtest-minor-mode-map
  )

(defun gtest-enable (executable)
  (interactive "f")
  (gtest-set-executable executable)
  (with-current-buffer (gtest-get-buffer-create)
    (gtest-mode))
  (gtest-display-tree test-suite-nodes)
  (gtest-create-highlight)
  (setq cursor-in-non-selected-windows nil))

(setq test-suite-nodes (gtest-create-tree))


