;;; agent-shell-chat-mode-tests.el --- Tests for agent-shell-chat-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-chat-mode)

;;; Code:

;; Declared special so tests can dynamically bind it (the prompt bar need
;; not be loaded); chat-mode reads it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

(defun agent-shell-chat-mode-tests--me-overlays ()
  "Return the `Me' label overlays in the current buffer, ordered by position."
  (sort (seq-filter (lambda (overlay)
                      (eq (overlay-get overlay 'category) 'agent-shell-chat-me))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun agent-shell-chat-mode-tests--agent-overlays ()
  "Return the agent label overlays in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'category) 'agent-shell-chat-agent))
              (overlays-in (point-min) (point-max))))

(defun agent-shell-chat-mode-tests--prompt (text)
  "Insert a shell prompt run displaying TEXT, as shell-maker fontifies it."
  (insert (propertize text 'font-lock-face
                      '(comint-highlight-prompt comint-highlight-prompt))))

(defun agent-shell-chat-mode-tests--marker ()
  "Insert the invisible `<shell-maker-end-of-prompt>' marker."
  (insert (propertize "<shell-maker-end-of-prompt>" 'invisible t)))

(defmacro agent-shell-chat-mode-tests--with-shell (&rest body)
  "Run BODY in a labeled shell buffer named \"Claude\"."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Claude"))))
                 agent-shell-chat--labeled t)
     ,@body))

(ert-deftest agent-shell-chat-prompt-face-p-test ()
  "Prompt runs are recognized whether the face is a symbol or a list."
  (should (agent-shell-chat--prompt-face-p 'comint-highlight-prompt))
  (should (agent-shell-chat--prompt-face-p '(comint-highlight-prompt comint-highlight-prompt)))
  (should-not (agent-shell-chat--prompt-face-p 'default))
  (should-not (agent-shell-chat--prompt-face-p nil)))

(ert-deftest agent-shell-chat-agent-name-test ()
  "The agent label uses `:mode-line-name', falling back to \"Agent\"."
  (agent-shell-chat-mode-tests--with-shell
    (should (equal "Claude" (agent-shell-chat--agent-name))))
  (with-temp-buffer
    (setq-local agent-shell--state nil)
    (should (equal "Agent" (agent-shell-chat--agent-name)))))

(ert-deftest agent-shell-chat-labels-submitted-turn-test ()
  "A submitted turn boxes the prompt as `Me' and the response as the agent."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi there\n")
    (agent-shell-chat--relabel)
    (let ((me (agent-shell-chat-mode-tests--me-overlays))
          (agent (agent-shell-chat-mode-tests--agent-overlays)))
      (should (= 1 (length me)))
      (should (string-match-p "Me" (overlay-get (car me) 'display)))
      (should (= 1 (length agent)))
      (should (string-match-p "Claude" (overlay-get (car agent) 'before-string))))))

(ert-deftest agent-shell-chat-live-prompt-hidden-with-bar-test ()
  "The empty live prompt shows `Me', or is hidden when the bar is on."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (should (string-match-p
               "Me" (overlay-get (car (agent-shell-chat-mode-tests--me-overlays)) 'display))))
    (let ((agent-shell-prompt-bar-mode t))
      (agent-shell-chat--relabel)
      (should (equal
               "" (overlay-get (car (agent-shell-chat-mode-tests--me-overlays)) 'display))))))

(ert-deftest agent-shell-chat-relabel-idempotent-test ()
  "Relabeling twice does not duplicate overlays."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi\n")
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--me-overlays))))
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-absorbs-leading-blank-lines-test ()
  "A submitted prompt's overlay swallows the input's leading blank lines."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((input-start (point)))
      (insert "\n\nPadded\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (should (= (overlay-end (car (agent-shell-chat-mode-tests--me-overlays)))
                 (save-excursion
                   (goto-char input-start)
                   (skip-chars-forward " \t\n")
                   (point)))))))

(provide 'agent-shell-chat-mode-tests)
;;; agent-shell-chat-mode-tests.el ends here
