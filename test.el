(require (quote json))
(require (quote compile))

(defun npm-run--find-package-json-files (project-root)
  (let ((default-directory project-root))
    (when (string-empty-p project-root)
      (error "No project root found"))
    (let ((output (shell-command-to-string
                   (format "%s -t f -E node_modules package.json %s"
                           npm-run-find-command
                           (shell-quote-argument (expand-file-name project-root))))))
      (when (string-empty-p output)
        (user-error "No package.json files found in project"))
      (mapcar (function string-trim)
              (split-string output "\n" t)))))

(provide (quote npm-run))
