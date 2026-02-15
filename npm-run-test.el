;;; npm-run-test.el --- Tests for npm-run -*- lexical-binding: t; -*-

;; Copyright (C) 2025  juboba

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

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'npm-run)

(ert-deftest npm-run-test--extract-package-dirs ()
  "Test extracting unique directories from package.json files."
  (let* ((temp-dir (make-temp-file "npm-run-test-" t))
         (pkg1 (concat temp-dir "/package.json"))
         (pkg2 (concat temp-dir "/packages/a/package.json"))
         (pkg3 (concat temp-dir "/packages/b/package.json"))
         (pkg4 (concat temp-dir "/packages/a/nested/package.json"))
         (files (list pkg1 pkg2 pkg3 pkg4))
         (expected-dirs (list (file-name-directory pkg1)
                              (file-name-directory pkg2)
                              (file-name-directory pkg3)
                              (file-name-directory pkg4))))
    (unwind-protect
        (progn
          ;; Create test directory structure
          (make-directory (file-name-directory pkg2) t)
          (make-directory (file-name-directory pkg3) t)
          (make-directory (file-name-directory pkg4) t)
          ;; Touch files
          (dolist (f files) (write-region "" nil f))
          ;; Test extraction
          (should (equal (npm-run--extract-package-dirs files)
                        (mapcar #'file-name-directory files))))
      (delete-directory temp-dir t))))

(ert-deftest npm-run-test--format-dir-candidate ()
  "Test formatting directory as completion candidate."
  (let* ((project-root "/path/to/project")
         ;; Root directory case
         (root-result (npm-run--format-dir-candidate
                       "/path/to/project" project-root "my-project"))
         ;; Subdirectory case
         (subdir-result (npm-run--format-dir-candidate
                         "/path/to/project/packages/utils"
                         project-root
                         "my-project"))
         ;; Deeply nested case
         (nested-result (npm-run--format-dir-candidate
                         "/path/to/project/packages/a/b/c"
                         project-root
                         "my-project")))
    ;; Root should use project name
    (should (string= (car root-result) "my-project"))
    ;; Subdirectory should use arrow notation
    (should (string= (car subdir-result) "packages -> utils"))
    ;; Nested should show full path
    (should (string= (car nested-result) "packages -> a -> b -> c"))))

(ert-deftest npm-run-test--parse-package-scripts ()
  "Test parsing scripts from package.json."
  (let* ((temp-file (make-temp-file "package" nil ".json"))
         (json-content "{
  \"name\": \"test-project\",
  \"scripts\": {
    \"dev\": \"vite\",
    \"build\": \"vite build\",
    \"test\": \"vitest\",
    \"lint\": \"eslint .\"
  }
}"))
    (unwind-protect
        (progn
          (write-region json-content nil temp-file)
          (let ((scripts (npm-run--parse-package-scripts temp-file)))
            (should (member "dev" scripts))
            (should (member "build" scripts))
            (should (member "test" scripts))
            (should (member "lint" scripts))
            (should (= 4 (length scripts)))))
      (delete-file temp-file))))

(ert-deftest npm-run-test--parse-package-scripts-no-scripts ()
  "Test parsing package.json with no scripts section."
  (let* ((temp-file (make-temp-file "package" nil ".json"))
         (json-content "{
  \"name\": \"test-project\",
  \"dependencies\": {
    \"foo\": \"^1.0.0\"
  }
}"))
    (unwind-protect
        (progn
          (write-region json-content nil temp-file)
          (should-error (npm-run--parse-package-scripts temp-file)
                       :type 'user-error))
      (delete-file temp-file))))

(ert-deftest npm-run-test--get-buffer-name ()
  "Test compilation buffer naming."
  (let* ((script "dev")
         (package-dir "/path/to/project/packages/utils")
         (project-root "/path/to/project"))
    (should (string= (npm-run--get-buffer-name script package-dir project-root)
                     "*npm run dev [utils]*"))))

(ert-deftest npm-run-test--get-buffer-name-root ()
  "Test compilation buffer naming for root package."
  (let* ((script "build")
         (package-dir "/path/to/project")
         (project-root "/path/to/project"))
    (should (string= (npm-run--get-buffer-name script package-dir project-root)
                     "*npm run build [project]*"))))

(ert-deftest npm-run-test-custom-commands ()
  "Test custom command configuration."
  ;; Test with pnpm
  (let ((npm-run-run-command "pnpm run"))
    (should (string= npm-run-run-command "pnpm run"))
    (setq npm-run-run-command "npm run"))
  
  ;; Test with yarn
  (let ((npm-run-run-command "yarn"))
    (should (string= npm-run-run-command "yarn"))
    (setq npm-run-run-command "npm run")))

(ert-deftest npm-run-test-ansi-colors-toggle ()
  "Test ANSI colors toggle."
  (let ((npm-run-show-ansi-colors t))
    (should npm-run-show-ansi-colors))
  (let ((npm-run-show-ansi-colors nil))
    (should-not npm-run-show-ansi-colors)))

;;; npm-run-test.el ends here
