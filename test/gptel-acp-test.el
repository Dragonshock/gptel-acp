;;; gptel-acp-test.el --- ERT for the ACP backend -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Drives gptel-acp with acp-fakes.  No network and no grok process.

;;; Code:

(require 'ert)
(require 'gptel)
(require 'gptel-acp)
(require 'acp-fakes)

(defvar gptel-acp-test--responses nil)
(defvar gptel-acp-test--client nil)
(defvar gptel-acp-test--wire-responses nil)
(defvar gptel-acp-test--seq 0)

(defun gptel-acp-test--message (direction kind object)
  `((:direction . ,direction)
    (:kind . ,kind)
    (:object . ,object)))

(defun gptel-acp-test--handshake ()
  "initialize, authenticate, and session/new for session sess-1."
  (list
   (gptel-acp-test--message
    'outgoing 'request
    '((jsonrpc . "2.0") (id . 1) (method . "initialize")
      (params (protocolVersion . 1))))
   (gptel-acp-test--message
    'incoming 'response
    '((jsonrpc . "2.0") (id . 1)
      (result (protocolVersion . 1)
              (agentCapabilities (loadSession . :false))
              (_meta (steering (supported . :false))))))
   (gptel-acp-test--message
    'outgoing 'request
    '((jsonrpc . "2.0") (id . 2) (method . "authenticate")))
   (gptel-acp-test--message
    'incoming 'response
    '((jsonrpc . "2.0") (id . 2) (result)))
   (gptel-acp-test--message
    'outgoing 'request
    '((jsonrpc . "2.0") (id . 3) (method . "session/new")))
   (gptel-acp-test--message
    'incoming 'response
    '((jsonrpc . "2.0") (id . 3)
      (result (sessionId . "sess-1"))))))

(defun gptel-acp-test--prompt (id)
  (gptel-acp-test--message
   'outgoing 'request
   `((jsonrpc . "2.0") (id . ,id) (method . "session/prompt"))))

(defun gptel-acp-test--result (id &optional reason)
  (gptel-acp-test--message
   'incoming 'response
   `((jsonrpc . "2.0") (id . ,id)
     (result (stopReason . ,(or reason "end_turn"))))))

(defun gptel-acp-test--update (kind text &optional session-id)
  (gptel-acp-test--message
   'incoming 'notification
   `((jsonrpc . "2.0")
     (method . "session/update")
     (params (sessionId . ,(or session-id "sess-1"))
             (update (sessionUpdate . ,kind)
                     (content (type . "text") (text . ,text)))))))

(defun gptel-acp-test--permission (id)
  "A permission request and the outgoing response that answers it."
  (list
   (gptel-acp-test--message
    'incoming 'request
    `((jsonrpc . "2.0")
      (id . ,id)
      (method . "session/request_permission")
      (params (toolCall (toolCallId . "call-1")
                        (title . "read_file")
                        (rawInput (path . "notes.txt")))
              (options . [((optionId . "allow-1") (kind . "allow_once"))
                          ((optionId . "reject-1") (kind . "reject_once"))]))))
   (gptel-acp-test--message
    'outgoing 'response
    `((jsonrpc . "2.0") (id . ,id)
      (result (outcome (outcome . "selected") (optionId . "allow-1")))))))

(defun gptel-acp-test--cancel ()
  (gptel-acp-test--message
   'outgoing 'notification
   '((jsonrpc . "2.0") (method . "session/cancel")
     (params (sessionId . "sess-1")))))

(defun gptel-acp-test--since (since)
  "Recorded requests pushed after the cons SINCE, oldest first."
  (nreverse
   (cl-loop for cell on gptel-acp--recording
            until (eq cell since)
            collect (car cell))))

(defun gptel-acp-test--methods (since)
  (mapcar (lambda (entry) (plist-get entry :method))
          (gptel-acp-test--since since)))

(defun gptel-acp-test--prompts (since)
  "Text of each session/prompt recorded after SINCE, oldest first."
  (cl-loop for entry in (gptel-acp-test--since since)
           when (equal (plist-get entry :method) "session/prompt")
           collect
           (cl-loop for block in (append (map-elt (plist-get entry :params) 'prompt) nil)
                    collect (map-elt block 'text))))

(defun gptel-acp-test--option-ids ()
  (cl-loop for args in (reverse gptel-acp-test--wire-responses)
           for response = (plist-get args :response)
           for id = (map-nested-elt response '(:result outcome optionId))
           when id collect id))

(defun gptel-acp-test--context (fsm)
  (format "state=%s error=%S status=%S responses=%S replay=%S turns=%S methods=%S prompts=%S"
          (and fsm (gptel-fsm-state fsm))
          (and fsm (plist-get (gptel-fsm-info fsm) :error))
          (and fsm (plist-get (gptel-fsm-info fsm) :status))
          (reverse gptel-acp-test--responses)
          (and fsm (plist-get (plist-get (gptel-fsm-info fsm) :data) :replay))
          (and fsm (plist-get (plist-get (gptel-fsm-info fsm) :data) :turns))
          (gptel-acp-test--methods (last gptel-acp--recording))
          (gptel-acp-test--prompts (last gptel-acp--recording))))

(defun gptel-acp-test--expect (fsm state)
  (unless (eq (gptel-fsm-state fsm) state)
    (ert-fail (gptel-acp-test--context fsm))))

(defun gptel-acp-test--callback (response _info)
  (push response gptel-acp-test--responses))

(defun gptel-acp-test--with-client (messages fn)
  "Run FN in a buffer whose ACP backend replays MESSAGES.
FN is called with no arguments in that buffer.  Dynamic bindings:
`gptel-acp-test--client', `gptel-acp-test--responses', and
`gptel-acp--recording'."
  (let* ((gptel-acp-test--seq (1+ gptel-acp-test--seq))
         (name (format "acp-test-%s" gptel-acp-test--seq))
         (backend (gptel-make-acp name
                    :command "cat"
                    :command-params nil
                    :auth 'cached-token
                    :profile 'chat
                    :stream t
                    :models '(grok)))
         (proc-buf (generate-new-buffer " *gptel-acp-cat*"))
         (proc (make-process
                :name (format "gptel-acp-cat-%s" gptel-acp-test--seq)
                :buffer proc-buf
                :command '("cat")
                :connection-type 'pipe
                :noquery t))
         (gptel-acp-test--client nil)
         (gptel-acp-test--responses nil)
         (gptel-acp--recording (list :sentinel))
         (gptel-acp--client-maker
          (lambda (&rest _)
            (setq gptel-acp-test--client (acp-fakes-make-client messages))
            (gptel-acp--put gptel-acp-test--client :process proc)
            (gptel-acp--put gptel-acp-test--client :gptel-owned-buffer
                            (generate-new-buffer " *gptel-acp-owned*"))
            gptel-acp-test--client))
         (buf (generate-new-buffer " *gptel-acp-test*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local gptel-backend backend)
          (setq-local gptel-model 'grok)
          (setq-local gptel-use-curl nil)
          (setq-local gptel-stream t)
          (setq-local gptel-acp-profile 'chat)
          (setq-local gptel-tools nil)
          (setq-local gptel-use-tools nil)
          (setq-local gptel-use-context nil)
          (setq-local gptel-context nil)
          (setq-local gptel-track-response t)
          (funcall fn))
      (ignore-errors (gptel-acp--drop-client backend gptel-acp-test--client))
      (when (processp proc)
        (setf (alist-get proc gptel--request-alist nil 'remove) nil)
        (when (process-live-p proc) (delete-process proc)))
      (when (buffer-live-p proc-buf) (kill-buffer proc-buf))
      (when (buffer-live-p buf) (kill-buffer buf))
      (setf (alist-get name gptel--known-backends nil 'remove #'equal) nil))))

(defun gptel-acp-test--send ()
  "Send the current buffer through ACP and return the FSM."
  (gptel-request nil
    :stream t
    :system nil
    :callback #'gptel-acp-test--callback))

(ert-deftest gptel-acp-grok-preset ()
  (let ((backend (gptel-get-backend "Grok")))
    (should (gptel-acp-p backend))
    (should (eq (type-of backend) 'gptel-acp))
    (should (equal (gptel-acp-command backend) "grok"))
    (should (equal (gptel-acp-command-params backend)
                   '("--no-auto-update" "agent" "stdio")))
    (should (eq (gptel-acp-auth backend) 'auto))
    (should (eq (gptel-acp-profile backend) 'chat))
    (should (eq (gptel-backend-stream backend) t))
    (should (equal (gptel-backend-models backend) '(grok-4.7-build-fast)))
    (should (equal (get 'grok-4.7-build-fast :acp-model-id) "grok-4.7-build-fast"))
    (should (equal (get 'grok-4.7-build-fast :reasoning-effort) "high"))
    (should (equal (get 'grok-4.7-build-fast :description) "Grok 4.7 Fast")))
  (let ((caps (map-nested-elt (gptel-acp--initialize-request 'chat)
                              '(:params clientCapabilities))))
    (should (eq (map-nested-elt caps '(fs readTextFile)) t))
    (should-not (eq (map-nested-elt caps '(fs writeTextFile)) t))
    (should-not (eq (map-elt caps 'terminal) t)))
  (let ((caps (map-nested-elt (gptel-acp--initialize-request 'agent)
                              '(:params clientCapabilities))))
    (should (eq (map-nested-elt caps '(fs writeTextFile)) t))
    (should (eq (map-elt caps 'terminal) t))))

(ert-deftest gptel-acp-session-config ()
  "A model id and effort are set before the first prompt."
  (gptel-acp-test--with-client
   (append (gptel-acp-test--handshake)
           (list
            (gptel-acp-test--message
             'outgoing 'request
             '((jsonrpc . "2.0") (id . 4) (method . "session/set_config_option")))
            (gptel-acp-test--message
             'incoming 'response
             '((jsonrpc . "2.0") (id . 4) (result)))
            (gptel-acp-test--message
             'outgoing 'request
             '((jsonrpc . "2.0") (id . 5) (method . "session/set_config_option")))
            (gptel-acp-test--message
             'incoming 'response
             '((jsonrpc . "2.0") (id . 5) (result)))
            (gptel-acp-test--prompt 6)
            (gptel-acp-test--update "agent_message_chunk" "pong")
            (gptel-acp-test--result 6)))
   (lambda ()
     (put 'grok-4.7-build-fast :acp-model-id "grok-4.7-build-fast")
     (setq-local gptel-model 'grok-4.7-build-fast)
     (setf (gptel-backend-models gptel-backend) '(grok-4.7-build-fast))
     (setq gptel-acp-reasoning-effort "high")
     (insert "hello")
     (let ((fsm (unwind-protect
                    (gptel-acp-test--send)
                  (setq gptel-acp-reasoning-effort nil)))
           (configs nil))
       (gptel-acp-test--expect fsm 'DONE)
       (dolist (entry (gptel-acp-test--since (last gptel-acp--recording)))
         (when (equal (plist-get entry :method) "session/set_config_option")
           (push (cons (map-elt (plist-get entry :params) 'configId)
                       (map-elt (plist-get entry :params) 'value))
                 configs)))
       (setq configs (nreverse configs))
       (should (equal configs '(("model" . "grok-4.7-build-fast")
                                ("reasoning_effort" . "high"))))
       (should (equal (reverse gptel-acp-test--responses) '("pong" t)))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))))))

(ert-deftest gptel-acp-text-stream ()
  (gptel-acp-test--with-client
   (append (gptel-acp-test--handshake)
           (list (gptel-acp-test--prompt 4)
                 (gptel-acp-test--update "agent_message_chunk" "pon")
                 (gptel-acp-test--update "agent_message_chunk" "g")
                 (gptel-acp-test--result 4)))
   (lambda ()
     (insert "hello")
     (let ((fsm (gptel-acp-test--send)))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (reverse gptel-acp-test--responses) '("pon" "g" t)))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))
       (let ((params (map-elt gptel-acp-test--client :gptel-initialize)))
         (should-not (eq (map-nested-elt params '(clientCapabilities fs writeTextFile)) t))
         (should-not (eq (map-nested-elt params '(clientCapabilities terminal)) t)))))))

(ert-deftest gptel-acp-reasoning-chunk ()
  (gptel-acp-test--with-client
   (append (gptel-acp-test--handshake)
           (list (gptel-acp-test--prompt 4)
                 (gptel-acp-test--update "agent_thought_chunk" "think")
                 (gptel-acp-test--update "agent_message_chunk" "ans")
                 (gptel-acp-test--result 4)))
   (lambda ()
     (insert "hello")
     (let ((fsm (gptel-acp-test--send)))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (reverse gptel-acp-test--responses)
                      '((reasoning . "think") (reasoning . t) "ans" t)))))))

(defun gptel-acp-test--answer (allow)
  "Callback that answers an ACP permission prompt."
  (lambda (response _info)
    (push response gptel-acp-test--responses)
    (when (eq (car-safe response) 'tool-call)
      (if allow
          (dolist (call (cdr response))
            (funcall (nth 2 call) 'allow))
        (gptel--reject-tool-calls (cdr response))))))

(defun gptel-acp-test--permission-script ()
  (append (gptel-acp-test--handshake)
          (list (gptel-acp-test--prompt 4))
          (gptel-acp-test--permission 10)
          (list (gptel-acp-test--update "agent_message_chunk" "ok")
                (gptel-acp-test--result 4))))

(defun gptel-acp-test--run-permission (allow)
  (let ((gptel-acp-test--wire-responses nil)
        (orig (symbol-function 'acp-send-response)))
    (cl-letf (((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (push args gptel-acp-test--wire-responses)
                 (apply orig args))))
      (gptel-acp-test--with-client
       (gptel-acp-test--permission-script)
       (lambda ()
         (insert "hello")
         (let ((fsm (gptel-request nil
                      :stream t
                      :system nil
                      :callback (gptel-acp-test--answer allow))))
           (gptel-acp-test--expect fsm 'DONE)
           (should (equal (gptel-acp-test--option-ids)
                          (list (if allow "allow-1" "reject-1"))))
           (let ((seen (reverse gptel-acp-test--responses)))
             (should (eq (car-safe (car seen)) 'tool-call))
             (should (equal (cdr seen) '("ok" t))))
           (should (acp-fakes-exhausted-p gptel-acp-test--client))))))))

(ert-deftest gptel-acp-permission-allow ()
  (gptel-acp-test--run-permission t))

(ert-deftest gptel-acp-permission-reject ()
  (gptel-acp-test--run-permission nil))

(ert-deftest gptel-acp-session-cancel ()
  (gptel-acp-test--with-client
   (append (gptel-acp-test--handshake)
           (list (gptel-acp-test--prompt 4)
                 (gptel-acp-test--cancel)
                 (gptel-acp-test--result 4 "cancelled")))
   (lambda ()
     (insert "hello")
     (let ((fsm (gptel-acp-test--send)))
       (gptel-acp-test--expect fsm 'ASTR)
       (should-not (member t gptel-acp-test--responses))
       (gptel-abort (current-buffer))
       (gptel-acp-test--expect fsm 'ABRT)
       (should (memq 'abort gptel-acp-test--responses))
       (should-not (member t gptel-acp-test--responses))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))))))

(ert-deftest gptel-acp-replay-after-hash-change ()
  (gptel-acp-test--with-client
   (append
    (gptel-acp-test--handshake)
    (list (gptel-acp-test--prompt 4)
          (gptel-acp-test--update "agent_message_chunk" "beta-two")
          (gptel-acp-test--result 4)
          (gptel-acp-test--prompt 5)
          (gptel-acp-test--update "agent_message_chunk" "gamma-ack" "sess-1")
          (gptel-acp-test--result 5)
          (gptel-acp-test--message
           'outgoing 'request
           '((jsonrpc . "2.0") (id . 6) (method . "session/new")))
          (gptel-acp-test--message
           'incoming 'response
           '((jsonrpc . "2.0") (id . 6)
             (result (sessionId . "sess-2"))))
          (gptel-acp-test--prompt 7)
          (gptel-acp-test--update "agent_message_chunk" "delta" "sess-2")
          (gptel-acp-test--result 7)))
   (lambda ()
     (insert "alpha-one")
     (let* ((mark gptel-acp--recording)
            (fsm (gptel-acp-test--send)))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (gptel-acp-test--methods mark)
                      '("initialize" "authenticate" "session/new" "session/prompt")))
       (should (equal (gptel-acp-test--prompts mark) '(("alpha-one")))))
     (setq gptel-acp-test--responses nil)
     (goto-char (point-max))
     (insert "\n\n" (propertize "beta-two" 'gptel 'response) "\n\ngamma-three")
     (let* ((mark gptel-acp--recording)
            (fsm (gptel-acp-test--send)))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (gptel-acp-test--methods mark) '("session/prompt")))
       (should (equal (gptel-acp-test--prompts mark) '(("gamma-three")))))
     (setq gptel-acp-test--responses nil)
     (goto-char (point-min))
     (search-forward "alpha-one")
     (replace-match "alpha-one-edited")
     (goto-char (point-max))
     (let* ((mark gptel-acp--recording)
            (fsm (gptel-acp-test--send))
            (texts (mapconcat (lambda (blocks) (mapconcat #'identity blocks "\n"))
                              (gptel-acp-test--prompts mark)
                              "\n")))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (gptel-acp-test--methods mark)
                      '("session/new" "session/prompt")))
       (should (string-search "alpha-one-edited" texts))
       (should (string-search "beta-two" texts))
       (should (string-search "gamma-three" texts))
       (should (string-search "Assistant" texts))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))))))

(ert-deftest gptel-acp-system-override ()
  "A system prompt is session _meta, not advisory prompt text."
  (gptel-acp-test--with-client
   (append (gptel-acp-test--handshake)
           (list (gptel-acp-test--prompt 4)
                 (gptel-acp-test--update "agent_message_chunk" "ok")
                 (gptel-acp-test--result 4)))
   (lambda ()
     (insert "hello")
     (let* ((mark gptel-acp--recording)
            (fsm (gptel-request nil
                   :stream t
                   :system "be brief"
                   :callback #'gptel-acp-test--callback))
            (new (cl-find-if
                  (lambda (entry)
                    (equal (plist-get entry :method) "session/new"))
                  (gptel-acp-test--since mark)))
            (texts (mapconcat
                    (lambda (blocks) (mapconcat #'identity blocks "\n"))
                    (gptel-acp-test--prompts mark)
                    "\n")))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (map-nested-elt (plist-get new :params)
                                      '(_meta systemPromptOverride))
                      "be brief"))
       (should-not (eq (map-nested-elt (plist-get new :params)
                                       '(_meta yoloMode))
                       t))
       (should-not (string-search "System instructions:" texts))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))))))

(ert-deftest gptel-acp-stale-file-hash-replays ()
  "A saved session id with a different hash opens a new session."
  (gptel-acp-test--with-client
   (append
    (list
     (gptel-acp-test--message
      'outgoing 'request
      '((jsonrpc . "2.0") (id . 1) (method . "initialize")
        (params (protocolVersion . 1))))
     (gptel-acp-test--message
      'incoming 'response
      '((jsonrpc . "2.0") (id . 1)
        (result (protocolVersion . 1)
                (agentCapabilities (loadSession . t))
                (_meta (steering (supported . :false))))))
     (gptel-acp-test--message
      'outgoing 'request
      '((jsonrpc . "2.0") (id . 2) (method . "authenticate")))
     (gptel-acp-test--message
      'incoming 'response
      '((jsonrpc . "2.0") (id . 2) (result)))
     (gptel-acp-test--message
      'outgoing 'request
      '((jsonrpc . "2.0") (id . 3) (method . "session/new")))
     (gptel-acp-test--message
      'incoming 'response
      '((jsonrpc . "2.0") (id . 3)
        (result (sessionId . "sess-2")))))
    (list (gptel-acp-test--prompt 4)
          (gptel-acp-test--update "agent_message_chunk" "delta" "sess-2")
          (gptel-acp-test--result 4)))
   (lambda ()
     (setq-local gptel-acp-session-id "old-sess")
     (setq-local gptel-acp-history-hash "stale-history")
     (setq-local gptel-acp-system-hash "stale-system")
     (insert "alpha-edited")
     (insert "\n\n" (propertize "beta-old" 'gptel 'response) "\n\ngamma-now")
     (let* ((mark gptel-acp--recording)
            (fsm (gptel-acp-test--send))
            (texts (mapconcat
                    (lambda (blocks) (mapconcat #'identity blocks "\n"))
                    (gptel-acp-test--prompts mark)
                    "\n")))
       (gptel-acp-test--expect fsm 'DONE)
       (should (equal (gptel-acp-test--methods mark)
                      '("initialize" "authenticate" "session/new" "session/prompt")))
       (should-not (member "session/load" (gptel-acp-test--methods mark)))
       (should (string-search "alpha-edited" texts))
       (should (string-search "beta-old" texts))
       (should (string-search "gamma-now" texts))
       (should (string-search "Assistant" texts))
       (should (acp-fakes-exhausted-p gptel-acp-test--client))))))

(provide 'gptel-acp-test)
;;; gptel-acp-test.el ends here
