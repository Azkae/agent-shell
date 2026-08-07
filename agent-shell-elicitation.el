;;; agent-shell-elicitation.el --- ACP elicitation support -*- lexical-binding: t; -*-

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
;; elicitation/create: the agent asks the user a structured question
;; mid-turn and waits for the answer before continuing.
;;
;; Only form mode is advertised at initialize time.  Form schemas are
;; flat objects of primitives and single-selects, so a schema asking for
;; choices renders as a button row (like a tool call permission), while
;; anything else is collected from the minibuffer.
;;
;; A single-select comes as either an "enum" of bare values or a "oneOf"
;; of titled options, and is often paired with an optional free text
;; property for answering in one's own words.  Both spellings render as
;; buttons, the latter with a trailing "Custom reply".
;;
;; A schema asking several questions at once is answered one question at
;; a time: the dialog renders the first question's buttons and, once
;; answered, re-renders itself on the next, sending every answer together
;; after the last one.
;;
;; See https://agentclientprotocol.com/protocol/elicitation.

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile
  (require 'cl-lib))

(declare-function acp-make-elicitation-create-response "acp")
(declare-function acp-send-response "acp")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--cancel-idle-timer "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell--start-idle-timer "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell-jump-to-latest-permission-button-row "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")

(defun agent-shell-elicitation--block-id (request-id)
  "Return the fragment block id rendering elicitation REQUEST-ID.

For example, with REQUEST-ID 43, returns \"elicitation-43\"."
  (format "elicitation-%s" request-id))

(cl-defun agent-shell-elicitation--on-create-request (&key state acp-request)
  "Handle an incoming \"elicitation/create\" ACP-REQUEST with STATE.

Renders the agent's question as buttons, one per choice (see
`agent-shell-elicitation--make-text' for the described variant):

   ╭─

       Agent question

       How should I approach this refactoring?

       [ conservative (1) ] [ balanced (2) ] [ Decline (d) ]

   ╰─

A schema asking several questions renders the first one and re-renders
itself on the next as each is answered (see
`agent-shell-elicitation--advance').

Only form mode is advertised to the agent, so requests in any other mode
\(e.g. url) are declined rather than errored: \"decline\" is an outcome
the agent must already handle, while a JSON-RPC error is not."
  (agent-shell-elicitation--track state (map-elt acp-request 'id))
  (if (not (equal (map-nested-elt acp-request '(params mode)) "form"))
      (agent-shell-elicitation--send-response
       :state state
       :request-id (map-elt acp-request 'id)
       :action "decline")
    (agent-shell-elicitation--render
     :dialog (agent-shell-elicitation--make-dialog
              :state state
              :request-id (map-elt acp-request 'id)
              :message (map-nested-elt acp-request '(params message))
              :schema (map-nested-elt acp-request '(params requestedSchema)))
     :above-last-prompt (not (agent-shell--active-requests-p state)))
    (let ((data (list (cons :request-id (map-elt acp-request 'id))
                      (cons :message (map-nested-elt acp-request '(params message))))))
      (agent-shell--emit-event :event 'elicitation-request :data data)
      (agent-shell--start-idle-timer :event 'elicitation-request :data data))
    (map-put! state :last-entry-type "elicitation/create")))

(cl-defun agent-shell-elicitation--make-dialog (&key state request-id message schema)
  "Bundle STATE, REQUEST-ID, MESSAGE and SCHEMA into an elicitation dialog.

Carries what answering takes from one question to the next, alongside
SCHEMA's questions as button forms (see
`agent-shell-elicitation--button-forms'):

  ((:state . STATE)
   (:request-id . 8)
   (:message . \"Please answer the following questions.\")
   (:schema . SCHEMA)
   (:forms . (((:name . question_0) ...) ((:name . question_1) ...))))"
  (list (cons :state state)
        (cons :request-id request-id)
        (cons :message message)
        (cons :schema schema)
        (cons :forms (agent-shell-elicitation--button-forms schema))))

(cl-defun agent-shell-elicitation--render (&key dialog (index 0) answers above-last-prompt)
  "Render DIALOG's question at INDEX, with ANSWERS collected so far.

ABOVE-LAST-PROMPT lands a newly created fragment above the active
prompt.  Re-rendering on the next question updates the same block in
place, so only the first render has any use for it."
  (agent-shell--update-fragment
   :state (map-elt dialog :state)
   ;; block-id must match the one used in
   ;; `agent-shell-elicitation--send-response' to delete the fragment.
   :block-id (agent-shell-elicitation--block-id (map-elt dialog :request-id))
   :body (with-current-buffer (map-nested-elt dialog '(:state :buffer))
           (agent-shell-elicitation--make-text
            :message (map-elt dialog :message)
            :progress (agent-shell-elicitation--make-progress
                       :dialog dialog :index index :answers answers)
            :actions (agent-shell-elicitation--make-actions
                      :dialog dialog :index index :answers answers)))
   :expanded t
   :navigation 'never
   :above-last-prompt above-last-prompt)
  (agent-shell-jump-to-latest-permission-button-row)
  (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                :shell-buffer (map-nested-elt dialog '(:state :buffer))
                                :existing-only t)))
    (with-current-buffer viewport-buffer
      (agent-shell-jump-to-latest-permission-button-row))))

(cl-defun agent-shell-elicitation--advance (&key dialog index answers)
  "Move DIALOG on to the question at INDEX, carrying ANSWERS.

Re-renders the dialog while questions remain, so a schema asking
several is answered with the same buttons as one asking a single
question, one question at a time.  Once INDEX is past the last
question, ANSWERS become the elicitation's content.

For example, picking \"Keep it as is\" on the first of three questions
advances to INDEX 1 with ANSWERS

  ((question_0 . \"Keep it as is\"))

while picking on the last sends all three at once."
  (if (< index (length (map-elt dialog :forms)))
      (agent-shell-elicitation--render
       :dialog dialog :index index :answers answers)
    (agent-shell-elicitation--send-response
     :state (map-elt dialog :state)
     :request-id (map-elt dialog :request-id)
     :action "accept"
     :content answers)))

(defun agent-shell-elicitation--track (state request-id)
  "Track elicitation REQUEST-ID as awaiting a response in STATE."
  (unless (assq :elicitations state)
    (nconc state (list (cons :elicitations nil))))
  (map-put! state :elicitations
            (cons request-id (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--send-response (&key state request-id action content)
  "Respond to elicitation REQUEST-ID with ACTION and CONTENT using STATE.

ACTION is one of \"accept\", \"decline\" or \"cancel\".  CONTENT is the
collected alist and only applies to \"accept\".

Responding twice to the same request is a no-op, so a button clicked
after the request was already answered (or cancelled on interrupt)
cannot desynchronize the agent."
  (when (member request-id (map-elt state :elicitations))
    (map-put! state :elicitations
              (seq-remove (lambda (pending)
                            (equal pending request-id))
                          (map-elt state :elicitations)))
    (acp-send-response
     :client (map-elt state :client)
     :response (acp-make-elicitation-create-response
                :request-id request-id
                :action action
                :content content))
    (with-current-buffer (map-elt state :buffer)
      ;; block-id must match the one used as
      ;; `agent-shell--update-fragment' param by "elicitation/create".
      (agent-shell--delete-fragment
       :state state
       :block-id (agent-shell-elicitation--block-id request-id))
      (agent-shell--cancel-idle-timer)
      (agent-shell--emit-event
       :event 'elicitation-response
       :data (list (cons :request-id request-id)
                   (cons :action action)
                   (cons :content content)))
      (or (agent-shell-jump-to-latest-permission-button-row)
          (goto-char (point-max))))))

(cl-defun agent-shell-elicitation-cancel-pending (&key state)
  "Cancel all elicitations awaiting a response in STATE.

Invoked on interrupt so the agent is not left waiting on a question the
user has moved on from."
  (dolist (request-id (map-elt state :elicitations))
    (agent-shell-elicitation--send-response
     :state state
     :request-id request-id
     :action "cancel")))

(cl-defun agent-shell-elicitation--make-actions (&key dialog (index 0) answers)
  "Make button actions answering DIALOG's question at INDEX, after ANSWERS.

Returns a list of alists in the shape
`agent-shell--make-permission-button' renders, always ending in a
decline action.  For a schema fitting
`agent-shell-elicitation--button-forms', each of the question's choices
becomes its own button:

  (((:label . \"conservative (1)\") (:option . \"conservative\")
    (:char . \"1\") (:description . nil) (:action . accept-conservative))
   ((:label . \"balanced (2)\") (:option . \"balanced\")
    (:char . \"2\") (:description . nil) (:action . accept-balanced))
   ((:label . \"Decline (d)\") (:option . \"decline\")
    (:char . \"d\") (:description . nil) (:action . decline)))

where each :action is a function of no arguments, answering the question
and moving on to the next one (see
`agent-shell-elicitation--advance').

When the question also carries optional extra properties, a \"Custom
reply\" action collects them from the minibuffer, so the user can answer
in their own words instead of picking one of the offered choices.  One
question among several that the schema does not require also gets a
\"Skip\" action; a lone question does not, since declining already
answers nothing.  Declining declines the whole elicitation, however many
questions are left.

A schema with no properties yields a lone confirmation action, and any
richer schema yields one action prompting through the minibuffer."
  (append
   (if-let* ((forms (map-elt dialog :forms))
             (form (seq-elt forms index)))
       (append
        (agent-shell-elicitation--make-choice-actions
         :dialog dialog :index index :answers answers)
        (when (map-elt form :extras)
          (list (agent-shell-elicitation--make-custom-reply-action
                 :dialog dialog :index index :answers answers)))
        (when (and (> (length forms) 1)
                   (not (map-elt form :required)))
          (list (agent-shell-elicitation--make-skip-action
                 :dialog dialog :index index :answers answers))))
     (list (if (seq-empty-p (map-nested-elt dialog '(:schema properties)))
               (agent-shell-elicitation--make-action
                :label "OK (y)" :option "accept" :char "y"
                :action (lambda ()
                          (agent-shell-elicitation--send-response
                           :state (map-elt dialog :state)
                           :request-id (map-elt dialog :request-id)
                           :action "accept")))
             (agent-shell-elicitation--make-action
              :label "Answer (y)" :option "answer" :char "y"
              :action (lambda ()
                        (condition-case nil
                            (agent-shell-elicitation--send-response
                             :state (map-elt dialog :state)
                             :request-id (map-elt dialog :request-id)
                             :action "accept"
                             :content (agent-shell-elicitation--read-content
                                       :schema (map-elt dialog :schema)))
                          (quit
                           (agent-shell-elicitation--send-response
                            :state (map-elt dialog :state)
                            :request-id (map-elt dialog :request-id)
                            :action "cancel"))))))))
   (list (agent-shell-elicitation--make-action
          :label "Decline (d)" :option "decline" :char "d"
          :action (lambda ()
                    (agent-shell-elicitation--send-response
                     :state (map-elt dialog :state)
                     :request-id (map-elt dialog :request-id)
                     :action "decline"))))))

(cl-defun agent-shell-elicitation--make-choice-actions (&key dialog index answers)
  "Make one action per choice of DIALOG's question at INDEX, after ANSWERS.

Each choice gets its position as a keyboard shortcut, so a question
offering \"conservative\" and \"balanced\" yields the
\"conservative (1)\" and \"balanced (2)\" actions of
`agent-shell-elicitation--make-actions'.  Picking one records the
choice's raw value under the question's property name and moves on."
  (let ((form (seq-elt (map-elt dialog :forms) index)))
    (seq-map-indexed
     (lambda (choice position)
       (agent-shell-elicitation--make-action
        :label (format "%s (%s)" (map-elt choice :title) (1+ position))
        :option (map-elt choice :title)
        :char (number-to-string (1+ position))
        :description (map-elt choice :description)
        :action (lambda ()
                  (agent-shell-elicitation--advance
                   :dialog dialog
                   :index (1+ index)
                   ;; The question's extras are optional, so the picked
                   ;; choice is the whole answer to it.
                   :answers (append answers
                                    (list (cons (map-elt form :name)
                                                (map-elt choice :value))))))))
     (map-elt form :choices))))

(cl-defun agent-shell-elicitation--make-custom-reply-action (&key dialog index answers)
  "Read DIALOG's question at INDEX from the minibuffer, after ANSWERS.

Reads the question's optional non-choice properties (see
`agent-shell-elicitation--button-forms'), typically a lone free text
field, so this is how the user answers in their own words instead of
picking one of the offered choices.

Backing out of the prompt returns to the question rather than ending
the elicitation: quitting or leaving the answer blank both mean the user
has not answered yet, so the offered choices are rendered again, with
whatever ANSWERS earlier questions already collected.  To move past a
question without answering it, use \"Skip\" instead."
  (let ((extras (map-elt (seq-elt (map-elt dialog :forms) index) :extras)))
    (agent-shell-elicitation--make-action
     :label "Custom reply (c)" :option "reply" :char "c"
     :description (when (= (length extras) 1)
                    (map-elt (cdr (seq-first extras)) 'description))
     :action (lambda ()
               (if-let* ((content (condition-case nil
                                      (agent-shell-elicitation--read-content
                                       ;; No required: an extra qualifying for
                                       ;; this button is optional by definition.
                                       :schema (list (cons 'properties extras)))
                                    (quit nil))))
                   (agent-shell-elicitation--advance
                    :dialog dialog
                    :index (1+ index)
                    :answers (append answers content))
                 (agent-shell-elicitation--render
                  :dialog dialog :index index :answers answers))))))

(cl-defun agent-shell-elicitation--make-skip-action (&key dialog index answers)
  "Make an action leaving DIALOG's question at INDEX unanswered, after ANSWERS.

Offered only for one question among several that the schema does not
require, so a schema asking a handful can be walked to the end without
inventing an answer to each.  The question's property is simply left out
of the content."
  (agent-shell-elicitation--make-action
   :label "Skip (s)" :option "skip" :char "s"
   :action (lambda ()
             (agent-shell-elicitation--advance
              :dialog dialog :index (1+ index) :answers answers))))

(cl-defun agent-shell-elicitation--make-action (&key label option char description action)
  "Make an action alist from LABEL, OPTION, CHAR, DESCRIPTION and ACTION.

LABEL is the button text, CHAR its keyboard shortcut, and OPTION the
name used in the cursor sensor message (\"Press RET or d to decline\").
DESCRIPTION, when non-nil, is the agent's longer copy for the choice,
rendered under the button by `agent-shell-elicitation--make-text'."
  (list (cons :label label)
        (cons :option option)
        (cons :char char)
        (cons :description (agent-shell-elicitation--text description))
        (cons :action action)))

(defun agent-shell-elicitation--choices (property)
  "Return PROPERTY's single-select choices as a list of alists, or nil.

The schema spells a single-select two ways, and agents use both.  An
untitled enum lists bare values

  ((type . \"string\") (enum . [\"conservative\" \"balanced\"]))

while a titled one lists options carrying their own copy:

  ((type . \"string\")
   (oneOf . [((const . \"tour\") (title . \"Tour the codebase\")
              (description . \"I explore src/lib, then summarize.\"))]))

Both normalize to

  (((:value . \"tour\") (:title . \"Tour the codebase\")
    (:description . \"I explore src/lib, then summarize.\")))

where :value is the raw JSON value to send back (so a numeric choice
round-trips as a number) and :title is what the button shows.  Returns
nil for a property that is not a single-select."
  (if-let* ((options (or (map-elt property 'oneOf)
                         (map-elt property 'anyOf))))
      ;; An option without a const names no value to send back, so it
      ;; cannot become a button.
      (seq-remove #'null
                  (seq-map (lambda (option)
                             (when-let* ((value (map-elt option 'const)))
                               (agent-shell-elicitation--make-choice
                                :value value
                                :title (map-elt option 'title)
                                :description (map-elt option 'description))))
                           options))
    (when-let* ((enum (map-elt property 'enum)))
      ;; enumNames is optional, so bind it outside the guard: a bare
      ;; enum must still yield choices.
      (let ((names (map-elt property 'enumNames)))
        (seq-map-indexed (lambda (value index)
                           (agent-shell-elicitation--make-choice
                            :value value
                            :title (when (< index (length names))
                                     (seq-elt names index))))
                         (append enum nil))))))

(cl-defun agent-shell-elicitation--make-choice (&key value title description)
  "Make a choice alist from VALUE, TITLE and DESCRIPTION.

TITLE falls back to VALUE's printed form, and a blank DESCRIPTION is
normalized to nil so callers can test it directly."
  (list (cons :value value)
        (cons :title (or (agent-shell-elicitation--text title)
                         (format "%s" value)))
        (cons :description (agent-shell-elicitation--text description))))

(defun agent-shell-elicitation--text (value)
  "Return VALUE if it is a non-empty string, nil otherwise."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun agent-shell-elicitation--button-forms (schema)
  "Return SCHEMA's questions as button forms, or nil for a minibuffer schema.

A schema qualifies when every single-select property offers at most 9
choices (so each choice gets a single digit shortcut) and every other
property is optional.  Agents routinely pair a choice with an optional
free text field and ask several questions at once by numbering them, as
in

  ((type . \"object\")
   (properties . ((question_0 . ((title . \"Next task\")
                                 (oneOf . [((const . \"tour\") ...)])))
                  (question_0_custom . ((type . \"string\")
                                        (title . \"Other\")))
                  (question_1 . ((title . \"Watcher\")
                                 (oneOf . [((const . \"drop\") ...)]))))))

which returns one form per question, in the order asked:

  (((:name . question_0) (:title . \"Next task\") (:description . nil)
    (:required . nil) (:choices . (((:value . \"tour\") ...)))
    (:extras . ((question_0_custom . ((type . \"string\")
                                      (title . \"Other\"))))))
   ((:name . question_1) (:title . \"Watcher\") (:description . nil)
    (:required . nil) (:choices . (((:value . \"drop\") ...)))
    (:extras . nil)))

A question's choices become one button each and its extras a single
\"Custom reply\" button, because picking a choice is allowed to leave
them unset.  A required extra cannot be skipped that way, so such a
schema is collected from the minibuffer instead."
  (when-let* ((names (seq-map #'car
                              (seq-filter (lambda (property)
                                            (agent-shell-elicitation--choices (cdr property)))
                                          (map-elt schema 'properties))))
              ((seq-every-p (lambda (name)
                              (<= (length (agent-shell-elicitation--choices
                                           (map-nested-elt schema (list 'properties name))))
                                  9))
                            names))
              ((seq-every-p (lambda (property)
                              (or (seq-contains-p names (car property))
                                  (not (seq-contains-p (map-elt schema 'required)
                                                       (symbol-name (car property))))))
                            (map-elt schema 'properties))))
    (seq-map (lambda (name)
               (agent-shell-elicitation--make-form :schema schema :names names :name name))
             names)))

(cl-defun agent-shell-elicitation--make-form (&key schema names name)
  "Make SCHEMA's question NAME into a button form, among questions NAMES.

TITLE falls back to the property name, so a question the agent left
untitled still has something to head it with.  See
`agent-shell-elicitation--button-forms' for the returned shape."
  (list (cons :name name)
        (cons :title (or (agent-shell-elicitation--text
                          (map-nested-elt schema (list 'properties name 'title)))
                         (symbol-name name)))
        (cons :description (agent-shell-elicitation--text
                            (map-nested-elt schema (list 'properties name 'description))))
        (cons :required (and (seq-contains-p (map-elt schema 'required)
                                             (symbol-name name))
                             t))
        (cons :choices (agent-shell-elicitation--choices
                        (map-nested-elt schema (list 'properties name))))
        (cons :extras (agent-shell-elicitation--extras
                       :schema schema :names names :name name))))

(cl-defun agent-shell-elicitation--extras (&key schema names name)
  "Return SCHEMA's optional properties belonging to the question NAME.

NAMES are every question SCHEMA asks.  A lone question owns every
non-question property, so with properties `strategy' and `summary',
NAMES `(strategy)' and NAME `strategy', returns

  ((summary . ((type . \"string\"))))

Agents asking several questions name the pairing instead, so each
remaining property goes to the longest question name it starts with (see
`agent-shell-elicitation--extra-owner').  With properties `question_0',
`question_0_custom', `question_1' and `question_1_custom', NAMES
`(question_0 question_1)' and NAME `question_0', returns

  ((question_0_custom . ((type . \"string\") (title . \"Other\"))))

A property starting with no question name is left out: it is optional by
definition, so omitting it still answers the schema."
  (seq-filter (lambda (property)
                (and (not (seq-contains-p names (car property)))
                     (eq name (if (= (length names) 1)
                                  (seq-first names)
                                (agent-shell-elicitation--extra-owner (car property) names)))))
              (map-elt schema 'properties)))

(defun agent-shell-elicitation--extra-owner (extra names)
  "Return the question in NAMES that property EXTRA belongs to, or nil.

Agents pair a question with its free text field by name, so EXTRA goes
to the longest question name it starts with: `question_0_custom' among
`(question_0 question_1)' returns `question_0', while among
`(question_1 question_10)' `question_10_custom' returns `question_10'
rather than `question_1'."
  (seq-first (seq-sort-by (lambda (name)
                            (length (symbol-name name)))
                          #'>
                          (seq-filter (lambda (name)
                                        (string-prefix-p (symbol-name name)
                                                         (symbol-name extra)))
                                      names))))

(cl-defun agent-shell-elicitation--make-progress (&key dialog (index 0) answers)
  "Render DIALOG's answered questions and the heading for the one at INDEX.

Returns nil when DIALOG asks a single question, whose message already is
the question.  When it asks several, recaps what ANSWERS holds so far
and heads the current question with its position:

  CI on PRs - Keep it as is
  Watcher - Drop it

  (3/3) Side fixes
  I also changed things outside this chain: ...

Faces aside, this is what `agent-shell-elicitation--make-text' puts
between the message and the buttons."
  (when-let* ((forms (map-elt dialog :forms))
              ((> (length forms) 1)))
    (string-join
     (seq-remove
      #'null
      (list (when (> index 0)
              (mapconcat (lambda (form)
                           (propertize
                            (format "%s - %s"
                                    (map-elt form :title)
                                    (agent-shell-elicitation--answer-summary form answers))
                            'font-lock-face 'agent-shell-secondary))
                         (seq-take forms index)
                         "\n"))
            ;; Heading and copy read as one question, so a single newline.
            (string-join
             (seq-remove #'null
                         (list (propertize (format "(%d/%d) %s"
                                                   (1+ index)
                                                   (length forms)
                                                   (map-elt (seq-elt forms index) :title))
                                           'font-lock-face 'agent-shell-input)
                               (when-let* ((description (map-elt (seq-elt forms index)
                                                                 :description)))
                                 (propertize description
                                             'font-lock-face 'agent-shell-secondary))))
             "\n")))
     "\n\n")))

(defun agent-shell-elicitation--answer-summary (form answers)
  "Return how FORM's question was answered in ANSWERS, for the recap.

Resolves a picked choice back to the title on its button, shortens a
custom reply to fit one line, and reports a question moved past as
skipped.  For example, with FORM naming `question_0', offering a choice
valued \"tour\" and titled \"Tour the codebase\", and ANSWERS

  ((question_0 . \"tour\"))

returns \"Tour the codebase\"."
  (if-let* ((value (map-elt answers (map-elt form :name))))
      (or (map-elt (seq-find (lambda (choice)
                               (equal (map-elt choice :value) value))
                             (map-elt form :choices))
                   :title)
          (format "%s" value))
    (if-let* ((custom (seq-some (lambda (extra)
                                  (map-elt answers (car extra)))
                                (map-elt form :extras))))
        (truncate-string-to-width (format "%s" custom) 60 nil nil t)
      "Skipped")))

(cl-defun agent-shell-elicitation--make-text (&key message progress actions)
  "Render an elicitation dialog for MESSAGE with PROGRESS and ACTIONS.

PROGRESS, when non-nil, is the recap and question heading
`agent-shell-elicitation--make-progress' renders for a schema asking
several questions, and goes between MESSAGE and the buttons.

ACTIONS is a list of alists as returned by
`agent-shell-elicitation--make-actions'.  Actions without a description
share one row, so a plain enum stays compact:

   ╭─

       Agent question

       How should I approach this refactoring?

       [ conservative (1) ] [ balanced (2) ] [ Decline (d) ]

   ╰─

while a described action takes a line of its own, with the agent's copy
underneath, so the choices can be told apart without hovering them:

   ╭─

       Agent question

       What would you like to work on?

       [ Tour the codebase (1) ]
           I explore src/lib, then summarize how the app fits together.

       [ Custom reply (c) ]
           Type your own answer instead of choosing an option above.

       [ Decline (d) ]

   ╰─"
  (let* ((keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (when-let* ((char (map-elt action :char)))
                       (define-key map (kbd char)
                                   (lambda ()
                                     (interactive)
                                     (funcall (map-elt action :action))))))
                   map))
         (button (lambda (action)
                   (agent-shell--make-permission-button
                    :text (map-elt action :label)
                    :help (or (map-elt action :description)
                              (map-elt action :label))
                    :action (lambda ()
                              (interactive)
                              (funcall (map-elt action :action)))
                    :keymap keymap
                    :char (map-elt action :char)
                    :option (map-elt action :option)
                    :navigatable t)))
         (described (seq-filter (lambda (action)
                                  (map-elt action :description))
                                actions))
         (plain (seq-remove (lambda (action)
                              (map-elt action :description))
                            actions)))
    (format "╭─

%s


%s


╰─"
            (agent-shell-elicitation--indent
             (string-join
              (seq-remove #'null
                          (list (propertize "Agent question"
                                            'font-lock-face 'agent-shell-permission-title)
                                (propertize (or message "")
                                            'font-lock-face 'agent-shell-input)
                                progress))
              "\n\n")
             4)
            (string-join
             (append
              (seq-map (lambda (action)
                         (concat "    " (funcall button action) "\n"
                                 (agent-shell-elicitation--indent
                                  (propertize (map-elt action :description)
                                              'font-lock-face 'agent-shell-secondary))))
                       described)
              (when plain
                (list (concat "    " (mapconcat button plain " ")))))
             "\n\n"))))

(defun agent-shell-elicitation--indent (text &optional columns)
  "Indent every line of TEXT by COLUMNS, 8 (under a dialog button) by default.

Blank lines are left alone rather than padded into trailing whitespace.

Indents with literal spaces rather than a `line-prefix' display
property: the fragment renderer sets its own `line-prefix' over the
whole body (see `agent-shell-ui--indent-text'), which would override
one set here."
  (mapconcat (lambda (line)
               (if (string-empty-p line)
                   line
                 (concat (make-string (or columns 8) ?\s) line)))
             (split-string text "\n")
             "\n"))

(cl-defun agent-shell-elicitation--read-content (&key schema)
  "Read SCHEMA's properties from the minibuffer.

Returns the alist to send back as the elicitation content.  Optional
properties left blank are omitted.  For example, a schema asking for a
required \"summary\" string and an optional \"issue\" integer may return

  ((summary . \"Rename the parser\") (issue . 42))"
  (seq-remove #'null
              (seq-map (lambda (property)
                         (when-let* ((value (agent-shell-elicitation--read-property
                                             :name (car property)
                                             :property (cdr property)
                                             :required (seq-contains-p
                                                        (map-elt schema 'required)
                                                        (symbol-name (car property))))))
                           (cons (car property) value)))
                       (map-elt schema 'properties))))

(cl-defun agent-shell-elicitation--read-property (&key name property required)
  "Read PROPERTY named NAME from the minibuffer.

REQUIRED reflects whether the schema lists NAME as required.  Returns
nil when an optional property is left blank so the caller can omit it.
Booleans come back as t or :false to serialize as JSON booleans.

For example, reading

  (strategy . ((type . \"string\") (enum . [\"conservative\" \"balanced\"])))

prompts with completion over both choices and returns \"balanced\"."
  (let ((prompt (format "%s%s: "
                        (or (map-elt property 'title)
                            (symbol-name name))
                        (if required "" " (optional)")))
        (choices (agent-shell-elicitation--choices property)))
    (cond
     (choices
      ;; Complete over the titles the agent wrote, then map the pick back
      ;; to the value to send: a titled choice's title and value differ.
      (let* ((titles (seq-map (lambda (choice) (map-elt choice :title)) choices))
             (title (completing-read prompt titles nil required)))
        (unless (string-empty-p title)
          (map-elt (seq-find (lambda (choice)
                               (equal (map-elt choice :title) title))
                             choices)
                   :value))))
     ((equal (map-elt property 'type) "boolean")
      (if (y-or-n-p prompt) t :false))
     ((equal (map-elt property 'type) "integer")
      (truncate (read-number prompt)))
     ((equal (map-elt property 'type) "number")
      (read-number prompt))
     (t
      (let ((value (read-string prompt)))
        (unless (string-empty-p value)
          value))))))

(provide 'agent-shell-elicitation)

;;; agent-shell-elicitation.el ends here
