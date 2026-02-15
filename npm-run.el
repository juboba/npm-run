;;; npm-run.el --- Run npm scripts from any package.json in your project -*- lexical-binding: t; -*-

;; Copyright (C) 2025  juboba
;; Author: juboba
;; Maintainer: juboba
;; URL: https://github.com/juboba/npm-run
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, tools, npm, scripts, project

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; npm-run provides an interactive command to run npm scripts from any
;; package.json file in your project.

;;; Code:

(require 'json)
(require 'compile)
(require 'project)

(defgroup npm-run nil
  "Run npm scripts from any package.json in your project."
  :group 'convenience)

(defcustom npm-run-find-command "fd"
  "Command to find package.json files."
  :type 'string
  :group 'npm-run)

(defcustom npm-run-run-command "npm run"
  "Command to run npm scripts.
Can be \='npm run\=' (default) or \='yarn\=' or \='pnpm run\=', etc."
  :type 'string
  :group 'npm-run)

(defcustom npm-run-show-ansi-colors t
  "Whether to show ANSI colors in output."
  :type 'boolean
  :group 'npm-run)

(defun npm-run--find-package-json-files (project-root)
  "Find all package.json files under PROJECT-ROOT."
  (let ((default-directory project-root))
    (when (string-empty-p project-root)
      (error "No project root found"))
    (let ((output (shell-command-to-string
                   (format "%s -t f -E node_modules package.json %s"
                           npm-run-find-command
                           (shell-quote-argument (expand-file-name project-root))))))
      (when (string-empty-p output)
        (user-error "No package.json files found in project"))
      (mapcar #'string-trim
              (split-string output "\n" t)))))

(defun npm-run--extract-package-dirs (package-json-files)
  "Extract unique directories from PACKAGE-JSON-FILES."
  (delete-dups
   (mapcar (lambda (file)
             (file-name-directory (string-trim file)))
           package-json-files)))

(defun npm-run--format-dir-candidate (dir project-root project-name)
  "Format DIR as a completion candidate.
PROJECT-ROOT and PROJECT-NAME are used for display formatting."
  (let* ((rel-name (file-relative-name dir project-root))
         (clean-name (directory-file-name rel-name))
         (display-name (cond
                        ((string= clean-name ".")
                         project-name)
                        ((string= clean-name "")
                         project-name)
                        (t
                         (replace-regexp-in-string "/" " -> " clean-name)))))
    (cons display-name dir)))

(defun npm-run--read-package-dir (package-dirs project-root project-name)
  "Prompt user to select a package directory from PACKAGE-DIRS.
PROJECT-ROOT and PROJECT-NAME are used for display formatting."
  (let* ((candidates (mapcar (lambda (dir)
                                (npm-run--format-dir-candidate dir project-root project-name))
                              package-dirs))
         (selected (completing-read "Select package: " candidates nil t)))
    (cdr (assoc selected candidates))))

(defun npm-run--parse-package-scripts (package-json-path)
  "Parse scripts from PACKAGE-JSON-PATH."
  (unless (file-exists-p package-json-path)
    (user-error "Package.json not found at %s" package-json-path))
  (let* ((json-object-type 'alist)
         (json-key-type 'string)
         (json (json-read-file package-json-path))
         (scripts-alist (cdr (assoc "scripts" json))))
    (unless scripts-alist
      (user-error "No scripts found in package.json"))
    (mapcar #'car scripts-alist)))

(defun npm-run--read-script (scripts package-dir project-name)
  "Prompt user to select a script from SCRIPTS.
PACKAGE-DIR and PROJECT-NAME are used in the prompt."
  (let* ((dir-name (file-name-nondirectory (directory-file-name package-dir)))
         (prompt (format "Run npm script in %s: "
                        (if (string= dir-name "")
                            project-name
                          dir-name))))
    (completing-read prompt scripts nil t)))

(defun npm-run--get-buffer-name (script package-dir project-root)
  "Get buffer name for running SCRIPT in PACKAGE-DIR.
PROJECT-ROOT is used for the root directory display."
  (let* ((dir-name (file-name-nondirectory (directory-file-name package-dir)))
         (display-name (if (string= dir-name "")
                           (file-name-nondirectory (directory-file-name project-root))
                         dir-name)))
    (format "*npm run %s [%s]*" script display-name)))

(defun npm-run--run-script (script package-dir project-root)
  "Run npm SCRIPT in PACKAGE-DIR.
PROJECT-ROOT is used for buffer naming."
  (let* ((compilation-buffer-name-function
          (lambda (_mode)
            (npm-run--get-buffer-name script package-dir project-root)))
         (command (format "cd %s && %s %s"
                         (shell-quote-argument package-dir)
                         npm-run-run-command
                         (shell-quote-argument script)))
         (compilation-buffer (compile command)))
    (when compilation-buffer
      (when npm-run-show-ansi-colors
        (with-current-buffer compilation-buffer
          (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter nil t)))
      (select-window (get-buffer-window compilation-buffer)))))

;;;###autoload
(defun npm-run ()
  "Run a script from a project npm package."
  (interactive)
  (let* ((project (project-current t))
         (project-root (if project
                           (project-root project)
                         default-directory))
         (project-name (if project
                           (file-name-nondirectory (directory-file-name project-root))
                         (file-name-nondirectory (directory-file-name default-directory))))
         (package-json-files (npm-run--find-package-json-files project-root))
         (package-dirs (npm-run--extract-package-dirs package-json-files)))
    (if (not package-dirs)
        (message "No package.json files found in project")
      (let* ((selected-dir (if (= (length package-dirs) 1)
                               (car package-dirs)
                             (npm-run--read-package-dir package-dirs project-root project-name)))
             (package-json-path (expand-file-name "package.json" selected-dir))
             (scripts (npm-run--parse-package-scripts package-json-path)))
        (if (not scripts)
            (message "No npm scripts found in %s" selected-dir)
          (let ((selected-script (npm-run--read-script scripts selected-dir project-name)))
            (when selected-script
              (npm-run--run-script selected-script selected-dir project-root))))))))

(provide 'npm-run)

;;; npm-run.el ends here
