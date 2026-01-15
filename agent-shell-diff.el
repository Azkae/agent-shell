;;; agent-shell-diff.el --- A quick way to query/display a diff. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'diff-mode)
(require 'xterm-color)

(defvar-local agent-shell-on-exit nil
  "Function to call when the diff buffer is killed.

This variable is automatically set by :on-exit from `agent-shell-diff'
and can be temporarily let-bound to nil to prevent the
on-exit callback from running when the buffer is killed.")

(defcustom agent-shell-delta-executable "delta"
  "The delta executable on your system to be used by Magit."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-default-light-theme "GitHub"
  "The default color theme when Emacs has a light background."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-default-dark-theme "Monokai Extended"
  "The default color theme when Emacs has a dark background."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-delta-args
  `("--max-line-distance" "0.6"
    "--true-color" ,(if xterm-color--support-truecolor "always" "never")
    "--color-only")
  "Delta command line arguments as a list of strings.

If the color theme is not specified using --theme, then it will
be chosen automatically according to whether the current Emacs
frame has a light or dark background. See `agent-shell-default-light-theme' and
`agent-shell-default-dark-theme'.

--color-only is required in order to use delta with magit; it
will be added if not present."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-max-size 50000
  "Don't use delta if length of the buffer is greater than specified.
This setting is needed to avoid situations when delta will freezes
the whole Emacs when trying to process extremely large diff"
  :type 'integer
  :group 'agent-shell)


(defun agent-shell--make-delta-args ()
  "Make final list of delta command-line arguments."
  (let ((args agent-shell-delta-args))
    (unless (seq-intersection '("--syntax-theme" "--light" "--dark") args)
      (setq args (nconc
                  (list "--syntax-theme"
                        (if (eq (frame-parameter nil 'background-mode) 'dark)
                            agent-shell-default-dark-theme
                              agent-shell-default-light-theme))
                       args)))
    (unless (member "--color-only" args)
      (setq args (cons "--color-only" args)))
    (setq args (nconc
                `("--plus-style"
                  ,(format "syntax \"%s\""
                           (face-attribute 'diff-added :background))
                  "--plus-emph-style"
                  ,(format "syntax \"%s\""
                           (face-attribute 'diff-refine-added :background))
                  "--minus-emph-style"
                  ,(format "syntax \"%s\""
                           (face-attribute 'diff-refine-removed :background))
                  "--minus-style"
                  ,(format "normal \"%s\""
                           (face-attribute 'diff-removed :background)))
                args))
    args))

(defun agent-shell-call-delta-and-convert-ansi-escape-sequences ()
  "Call delta on buffer contents and convert ANSI escape sequences to overlays.

The input buffer contents are expected to be raw git output."
  (unless (and agent-shell-max-size (> (point-max) agent-shell-max-size))
    (apply #'call-process-region
           (point-min) (point-max)
           agent-shell-delta-executable t t nil (agent-shell--make-delta-args))
    (let ((buffer-read-only nil))
      (xterm-color-colorize-buffer 'use-overlays))))

(cl-defun agent-shell-diff (&key old new on-exit title bindings file)
  "Display a diff between OLD and NEW strings in a buffer.

Creates a new buffer showing the differences between OLD and NEW
using `diff-mode'.  The buffer is read-only.

When the buffer is killed, calls ON-EXIT with no arguments.

Arguments:
  :OLD       - Original string content
  :NEW       - Modified string content
  :ON-EXIT   - Function called with no arguments when buffer is killed
  :TITLE     - Optional title to display in header line
  :BINDINGS  - List of alists defining key bindings, each with:
               :key         - Key string (e.g., \"n\")
               :description - Description for header line (e.g., \"next hunk\")
               :command     - Command function (e.g., `diff-hunk-next')
  :OLD-LABEL - Label for old content (default: \"before\")
  :NEW-LABEL - Label for new content (default: \"after\")"
  (let* ((diff-buffer (generate-new-buffer "*agent-shell-diff*"))
         (calling-window (selected-window))
         (calling-buffer (current-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n")
              (insert (agent-shell-diff--make-diff old new file))
              (agent-shell-call-delta-and-convert-ansi-escape-sequences)
              ;; Add overlays to hide scary text.
              (save-excursion
                (goto-char (point-min))
                ;; Hide --- and +++ lines
                (while (re-search-forward "^\\(---\\|\\+\\+\\+\\).*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put overlay 'category 'diff-header)
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'evaporate t)))
                ;; Replace @@ lines with "Changes"
                (goto-char (point-min))
                (while (re-search-forward "^@@.*@@.*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)))
                        (face 'diff-hunk-header))  ; or any face you prefer
                    (overlay-put overlay 'category 'diff-header)
                    ;; Intended display is:
                    ;; ╭─────────╮
                    ;; │ changes │
                    ;; ╰─────────╯
                    ;; Using before-string so diff-hunk-next
                    ;; lands on "│" instead of "╭".
                    (overlay-put overlay 'before-string
                                 (propertize "\n╭─────────╮\n" 'face face))
                    (overlay-put overlay 'display
                                 (propertize "│ changes │\n╰─────────╯\n\n" 'face face))
                    (overlay-put overlay 'evaporate t)))))
            (diff-mode)
            (when bindings
              (setq header-line-format
                    (concat
                     "  "
                     (when title
                       (concat (propertize title 'face 'mode-line-emphasis) " "))
                     (mapconcat
                      #'identity
                      (seq-filter
                       #'identity
                       (mapcar
                        (lambda (binding)
                          (when (map-elt binding :description)
                            (concat
                             (propertize (map-elt binding :key) 'face 'help-key-binding)
                             " "
                             (map-elt binding :description))))
                        bindings))
                      " "))))
            (goto-char (point-min))
            (ignore-errors (diff-hunk-next))
            (when on-exit
              (setq agent-shell-on-exit on-exit)
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (with-current-buffer diff-buffer
                            (when agent-shell-on-exit
                              (with-current-buffer calling-buffer
                                (funcall on-exit))))
                          (with-current-buffer calling-buffer
                            ;; Make sure give focus back to calling buffer on exit.
                            (if (window-live-p calling-window)
                                (if (eq (window-buffer calling-window) calling-buffer)
                                    ;; Calling buffer still on calling window, just select it.
                                    (select-window calling-window)
                                  ;; Calling buffer not on calling window, restore it.
                                  (progn
                                    (set-window-buffer calling-window calling-buffer)
                                    (select-window calling-window))))))
                        nil t))
            (setq buffer-read-only t)
            (when bindings
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map diff-mode-map)
                (dolist (binding bindings)
                  (define-key map (kbd (map-elt binding :key)) (map-elt binding :command)))
                (use-local-map map)))))
      (pop-to-buffer diff-buffer '((display-buffer-use-some-window
                                    display-buffer-same-window))))))

(defun agent-shell-diff--make-diff (old new file)
  "Create a unified diff between OLD and NEW strings.
Returns the diff output as a string."
  (let* ((suffix (format ".%s" (file-name-extension file)))
         (old-file (make-temp-file "old" nil suffix))
         (new-file (make-temp-file "new" nil suffix)))
    (with-temp-file old-file (insert old))
    (with-temp-file new-file (insert new))
    (with-temp-buffer
      (call-process "diff" nil t nil "-U3" old-file new-file)
      (buffer-string))))

(provide 'agent-shell-diff)

;;; agent-shell-diff.el ends here
