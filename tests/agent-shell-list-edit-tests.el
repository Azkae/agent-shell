;;; agent-shell-list-edit-tests.el --- Tests for agent-shell-list-edit -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run via:
;;
;;   `emacs -batch -l ert -l tests/agent-shell-list-edit-tests.el \
;;          -f ert-run-tests-batch-and-exit'

;;; Code:

(require 'ert)

(load-file (expand-file-name "../agent-shell-list-edit.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

(defun agent-shell-list-edit-tests--run (input fn)
  "Place INPUT into a temp buffer with \"|\" marking point, then call FN.

Return the resulting buffer string with \"|\" reinserted at the final
point location.  For example, INPUT \"- foo|\" places point after
\"foo\"."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (funcall fn)
    (insert "|")
    (buffer-string)))

(ert-deftest agent-shell-list-edit--at-item-recognizes-bullet ()
  (with-temp-buffer
    (insert "- hello")
    (goto-char (point-min))
    (should (equal (agent-shell-list-edit--at-item)
                   '((:type . bullet)
                     (:indent . "")
                     (:marker . "-")
                     (:content . "hello"))))))

(ert-deftest agent-shell-list-edit--at-item-recognizes-indented-bullet ()
  (with-temp-buffer
    (insert "  * nested")
    (goto-char (point-min))
    (should (equal (agent-shell-list-edit--at-item)
                   '((:type . bullet)
                     (:indent . "  ")
                     (:marker . "*")
                     (:content . "nested"))))))

(ert-deftest agent-shell-list-edit--at-item-recognizes-numbered ()
  (with-temp-buffer
    (insert "12. step")
    (goto-char (point-min))
    (should (equal (agent-shell-list-edit--at-item)
                   '((:type . numbered)
                     (:indent . "")
                     (:marker . "12")
                     (:content . "step"))))))

(ert-deftest agent-shell-list-edit--at-item-returns-nil-on-plain-line ()
  (with-temp-buffer
    (insert "just some prose")
    (goto-char (point-min))
    (should-not (agent-shell-list-edit--at-item))))

(ert-deftest agent-shell-list-edit--at-item-returns-nil-without-space-after-marker ()
  (with-temp-buffer
    (insert "-nope")
    (goto-char (point-min))
    (should-not (agent-shell-list-edit--at-item))))

(ert-deftest agent-shell-list-edit-newline-continues-bullet ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "- foo|"
            #'agent-shell-list-edit-newline)
           "- foo\n- |")))

(ert-deftest agent-shell-list-edit-newline-preserves-indent-and-marker ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "  + foo|"
            #'agent-shell-list-edit-newline)
           "  + foo\n  + |")))

(ert-deftest agent-shell-list-edit-newline-increments-numbered ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "1. one|"
            #'agent-shell-list-edit-newline)
           "1. one\n2. |")))

(ert-deftest agent-shell-list-edit-newline-empty-bullet-breaks-out ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "- foo\n- |"
            #'agent-shell-list-edit-newline)
           "- foo\n\n|")))

(ert-deftest agent-shell-list-edit-newline-empty-indented-bullet-breaks-out ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "  - |"
            #'agent-shell-list-edit-newline)
           "\n|")))

(ert-deftest agent-shell-list-edit-newline-plain-line-just-newlines ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "prose|"
            #'agent-shell-list-edit-newline)
           "prose\n|")))

(ert-deftest agent-shell-list-edit-indent-line-indents-bullet ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "- foo|"
            #'agent-shell-list-edit-indent-line)
           "  - foo|")))

(ert-deftest agent-shell-list-edit-indent-line-indents-numbered ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "1. step|"
            #'agent-shell-list-edit-indent-line)
           "  1. step|")))

(ert-deftest agent-shell-list-edit-dedent-line-dedents-bullet ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "    - foo|"
            #'agent-shell-list-edit-dedent-line)
           "  - foo|")))

(ert-deftest agent-shell-list-edit-dedent-line-no-op-without-enough-indent ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "- foo|"
            #'agent-shell-list-edit-dedent-line)
           "- foo|")))

(ert-deftest agent-shell-list-edit-dedent-line-no-op-on-plain-line ()
  (should (equal
           (agent-shell-list-edit-tests--run
            "    prose|"
            #'agent-shell-list-edit-dedent-line)
           "    prose|")))

(provide 'agent-shell-list-edit-tests)

;;; agent-shell-list-edit-tests.el ends here
