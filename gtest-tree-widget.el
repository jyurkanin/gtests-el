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

(defun set-gtest-executable (gtest-executable-path)
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

(defun my-tree-widget-get-icon-from-status (tree)
  (let ((status (widget-get tree :status)))
    (message (format "tree status: %s" status))
    (cond
     ((eq status nil) 'tree-widget-empty-icon)
     ((string= status "error") 'tree-widget-empty-icon)
     ((string= status "pass") 'tree-widget-open-icon)
     ((string= status "fail") 'tree-widget-close-icon))))

(defun my-tree-widget-value-create (tree)
  "Create the TREE tree-widget."
  (let* ((node   (tree-widget-node tree))
         (flags  (widget-get tree :tree-widget--guide-flags))
         (indent (widget-get tree :indent))
         ;; Setup widget's image support.  Looking up for images, and
         ;; setting widgets' :tag-glyph is done here, to allow us to
         ;; dynamically change the image theme.
         (widget-image-enable (tree-widget-use-image-p))
         children buttons)
    (and indent (not (widget-get tree :parent))
         (insert-char ?\  indent))
    (if (widget-get tree :open)
;;;; Expanded node.
        (let ((args     (widget-get tree :args))
              (guide    (widget-get tree :guide))
              (nohandle-guide (widget-get tree :nohandle-guide))
              (noguide  (widget-get tree :no-guide))
              (endguide (widget-get tree :end-guide))
              (handle   (widget-get tree :handle))
              (nohandle (widget-get tree :no-handle))
              (guidi    (tree-widget-find-image "guide"))
              (nohandle-guidi (tree-widget-find-image "nohandle-guide"))
              (noguidi  (tree-widget-find-image "no-guide"))
              (endguidi (tree-widget-find-image "end-guide"))
              (handli   (tree-widget-find-image "handle"))
              (nohandli (tree-widget-find-image "no-handle")))
          ;; Request children at run time, when requested.
          (when (and (widget-get tree :expander)
                     (widget-apply tree :expander-p))
            (setq args (mapcar #'widget-convert
                               (widget-apply tree :expander)))
            (widget-put tree :args args))
          ;; Defer the node widget creation after icon creation.
          (widget-put tree :node (widget-convert node))
          ;; Create the icon widget for the expanded tree.
          (push (widget-create-child-and-convert
                 tree (my-tree-widget-get-icon-from-status tree)
                 ;; Pass the node widget to child.
                 :node (widget-get tree :node))
                buttons)
          ;; Create the tree node widget.
          (push (widget-create-child tree (widget-get tree :node))
                children)
          ;; Update the icon :node with the created node widget.
          (widget-put (car buttons) :node (car children))
          ;; Create the tree children.
          (while args
            (setq node (car args)
                  args (cdr args))
            (and indent (insert-char ?\  indent))
            ;; Insert guide lines elements from previous levels.
            (dolist (f (reverse flags))
              (widget-create-child-and-convert
               tree (if f nohandle-guide noguide)
               :tag-glyph (if f nohandle-guidi noguidi))
              (widget-create-child-and-convert
               tree nohandle :tag-glyph nohandli))
            ;; Insert guide line element for this level.
            (widget-create-child-and-convert
             tree (if args guide endguide)
             :tag-glyph (if args guidi endguidi))
            ;; Insert the node handle line
            (widget-create-child-and-convert
             tree handle :tag-glyph handli)
            (if (tree-widget-p node)
                ;; Create a sub-tree node.
                (push (widget-create-child-and-convert
                       tree node :tree-widget--guide-flags
                       (cons (if args t) flags))
                      children)
              ;; Create the icon widget for a leaf node.
              (push (widget-create-child-and-convert
                     tree (widget-get tree :leaf-icon)
                     ;; At this point the node widget isn't yet created.
                     :node (setq node (widget-convert
                                       node :tree-widget--guide-flags
                                       (cons (if args t) flags)))
                     :tree-widget--leaf-flag t)
                    buttons)
              ;; Create the leaf node widget.
              (push (widget-create-child tree node) children)
              ;; Update the icon :node with the created node widget.
              (widget-put (car buttons) :node (car children)))))
;;;; Collapsed node.
      ;; Defer the node widget creation after icon creation.
      (widget-put tree :node (widget-convert node))
      ;; Create the icon widget for the collapsed tree.
      (push (widget-create-child-and-convert
             tree (my-tree-widget-get-icon-from-status tree)
             ;; Pass the node widget to child.
             :node (widget-get tree :node))
            buttons)
      ;; Create the tree node widget.
      (push (widget-create-child tree (widget-get tree :node))
            children)
      ;; Update the icon :node with the created node widget.
      (widget-put (car buttons) :node (car children))
      )
    ;; Save widget children and buttons.  The tree-widget :node child
    ;; is the first element in :children.
    (widget-put tree :children (nreverse children))
    (widget-put tree :buttons  buttons)
    ))

(defun my-tree-widget-update-icon (tree)
  "Update the icon of TREE to NEW-ICON."
  (message (format "tree type: %s" (widget-type tree)))
  (let ((buttons (widget-get tree :buttons)))
    (when buttons
      (let ((icon-widget (widget-convert (my-tree-widget-get-icon-from-status tree)))
            (old-icon-widget (car buttons)))
        ;; (widget-put icon-widget :node (widget-get old-icon-widget :node))
        (widget-put icon-widget :parent (widget-get old-icon-widget :parent))
        (widget-put icon-widget :indent (widget-get old-icon-widget :indent))
        (widget-put icon-widget :supress-face (widget-get old-icon-widget :supress-face))
        (widget-put icon-widget :button-overlay (widget-get old-icon-widget :button-overlay))
        (widget-put icon-widget :from (widget-get old-icon-widget :from))
        (widget-put icon-widget :to (widget-get old-icon-widget :to))
        (setcar buttons icon-widget)
        (widget-value-set icon-widget (widget-get icon-widget :value)))
        )
      ))

(defun gtest-get-test-case-result (test-suite test-case)
  "Returns 1 if successful"
  (let* ((stdout (shell-command-to-string  (format "%s --gtest_filter=%s.%s" gtest-executable test-suite test-case)))
         (pos-end (string-match (regexp-quote "[----------] Global test environment tear-down") stdout))
         (pos-fail (string-match (regexp-quote "[  FAILED  ]") stdout)))
    
    (if pos-end
        (if pos-fail
            "fail"
          "pass")
      "error"
      )))

(defun gtest-run-test-for-widget (test-case-node)
  (let* ((test-case (widget-get test-case-node :tag))
         (temp-node (widget-get test-case-node :parent))
         (test-suite-node (widget-get temp-node :parent))
         (test-suite (widget-get test-suite-node :tag)))

    (message (format "test-case-node: %s" (widget-type test-case-node)))
    (message (format "temp-node: %s" (widget-type temp-node)))
    (message (format "test-suite-node: %s" (widget-type test-suite-node)))
    
    (if test-suite
        (progn
          (widget-put temp-node :status (gtest-get-test-case-result test-suite test-case))
          (my-tree-widget-update-icon temp-node))
      (let ((child-nodes (widget-get temp-node :children))
            (did-fail nil)
            (did-error nil)
            (result nil))
        (setq child-nodes (cdr child-nodes))
        ;;(message (format "Num child nodes %d" (length child-nodes)))
        (while child-nodes
          (setq result (gtest-get-test-case-result test-case (widget-get (car child-nodes) :tag)))
          (setq did-fail (or did-fail (eq "fail" result)))
          (setq did-error (or did-error (eq "error" result)))
          (setq child-nodes (cdr child-nodes)))
        (if did-error
            (widget-put test-case-node :status "error")
          (if did-fail
              (widget-put test-case-node :status "fail")
            (widget-put test-case-node :status "pass")))))))



(defun my-tree-widget-run-test ()
  "TODO: Make this actually run an executable and parse the output. Then   update the state of the test-case-node widget. Check if all the
   test-cases in a suite are passing. If so, then mark the suite as
   passing."
  (interactive)
  (let ((tree (widget-at (point))))
    (when (gtest-run-test-for-widget (widget-get tree :node)))))

(defvar my-tree-widget-button-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km tree-widget-button-keymap)
    (keymap-set km "r" 'my-tree-widget-run-test)
    km))

(define-widget 'my-tree-widget 'tree-widget
  "My Tree Widget."
  :value-create 'my-tree-widget-value-create
  :status nil
  :keep (list :status)
  :keymap my-tree-widget-button-keymap
  )

(defun create-dynamic-tree ()
  "Create tree with variable number of child nodes"
  (setq all-tests (gtest-list-tests))
  
  (let ((buffer (get-buffer-create "Gtest Tests")))
    (with-current-buffer buffer
      (erase-buffer)
      (widget-insert "Expandable Tree Widget:\n\n")
      (dolist (suite-tests all-tests)
          (widget-create
           'my-tree-widget
           :tag (car (last suite-tests))
           :open nil
           :args
           (mapcar (lambda (n)
                     (widget-convert
                      'my-tree-widget
                      :tag n
                      :action                      
                      ))
                   (butlast suite-tests))))
      (use-local-map my-tree-widget-button-keymap)
      (widget-setup))
    (switch-to-buffer buffer)))

(create-dynamic-tree)

        
