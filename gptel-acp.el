;;; gptel-acp.el --- Agent Client Protocol backend for gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (gptel "0.9.9.6"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
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

;; Separate package.  gptel does not load this file.  See README.md
;; for install and Emacs configuration.  One long-lived agent process
;; per backend speaks JSON-RPC through acp.el.  HTTP backends are
;; unchanged.  Sending goes through `gptel-backend-send'.
;;
;; acp.el is loaded only when an ACP backend is used.  Phase 4 (MCP
;; and Elisp tools inside session/new) is intentionally absent:
;; mcpServers is always an empty vector.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'gptel-request)

(declare-function acp-make-client "acp")
(declare-function acp-send-request "acp")
(declare-function acp-send-notification "acp")
(declare-function acp-send-response "acp")
(declare-function acp-shutdown "acp")
(declare-function acp-subscribe-to-notifications "acp")
(declare-function acp-subscribe-to-requests "acp")
(declare-function acp-make-initialize-request "acp")
(declare-function acp-make-authenticate-request "acp")
(declare-function acp-make-session-new-request "acp")
(declare-function acp-make-session-prompt-request "acp")
(declare-function acp-make-session-load-request "acp")
(declare-function acp-make-session-cancel-notification "acp")
(declare-function acp-make-session-request-permission-response "acp")
(declare-function acp-make-fs-read-text-file-response "acp")
(declare-function acp-make-fs-write-text-file-response "acp")
(declare-function acp-make-error "acp")
(declare-function acp--client-started-p "acp")

(declare-function gptel--handle-pre-insert "gptel")
(declare-function gptel--update-status "gptel")
(declare-function gptel--update-tool-call "gptel")
(declare-function gptel--fsm-live-p "gptel")
(declare-function gptel-context--collect-media "gptel-context")
(declare-function gptel-curl--stream-insert-response "gptel")
(declare-function gptel--insert-response "gptel")
(declare-function gptel--stream-convert-markdown->org "gptel")
(declare-function gptel-org--get-topic-start "gptel-org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-up-heading-safe "org")

(defvar gptel-acp--request-buffer nil
  "Original chat buffer while a prompt is being built.")
(defvar gptel-acp--request-position nil
  "Position in `gptel-acp--request-buffer' used for the session key.")
(defvar gptel-acp--request-rewrite nil
  "Non-nil when the prompt being built is a rewrite.")

(defvar gptel-acp--client-maker nil
  "Function of (backend profile environment) that returns an acp client.
Tests bind this to a fake client.  Nil uses `gptel-acp--make-acp-client'.")

(defvar gptel-acp--recording nil
  "When non-nil, a list receiving outgoing ACP requests.
Each entry is (:method METHOD :params PARAMS).")

(defvar gptel-acp--in-wait nil
  "Non-nil while `gptel-backend-send' is still inside the WAIT handler.")

(defvar gptel-acp-reasoning-effort nil
  "Reasoning effort id for an ACP session, or nil.
When non-nil, gptel sends it with `session/set_config_option' as
`reasoning_effort' after the session exists.  Typical ids are
\"low\", \"medium\", \"high\", and \"xhigh\".  Nil uses the
`:reasoning-effort' property of `gptel-model', if that is a string.")

(defvar-local gptel-acp-profile nil
  "Buffer-local ACP profile, `chat' or `agent'.
Nil uses the profile stored on the backend.")

(defvar-local gptel-acp-session-id nil
  "ACP session id for this buffer.
Written as a file-local variable so a later Emacs can `session/load' it.")

(defvar-local gptel-acp-history-hash nil
  "Hash of the turns already accepted for `gptel-acp-session-id'.
Written as a file-local variable.  A later Emacs loads that
session only when this still matches the buffer.")

(defvar-local gptel-acp-system-hash nil
  "Hash of the advisory session inputs for `gptel-acp-session-id'.
Written as a file-local variable next to `gptel-acp-history-hash'.")

(put 'gptel-acp-session-id 'safe-local-variable #'stringp)
(put 'gptel-acp-history-hash 'safe-local-variable #'stringp)
(put 'gptel-acp-system-hash 'safe-local-variable #'stringp)
(put 'gptel-acp-profile 'safe-local-variable
     (lambda (v) (memq v '(nil chat agent))))

(defconst gptel-acp--permission-action
  (lambda (&rest _) 'allow)
  "Tool function for an ACP permission prompt.
Returning the symbol `allow' tells the permission callback to
select the agent's allow option.  Any other result rejects.")

(defun gptel-acp--permission-answered-p (tool)
  "Return non-nil when TOOL is an ACP permission prompt."
  (and (gptel-tool-p tool)
       (eq (gptel-tool-function tool) gptel-acp--permission-action)))

;;;; Backend

(cl-defstruct (gptel-acp (:include gptel-backend)
                         (:constructor gptel--make-acp)
                         (:copier nil))
  "An ACP backend.

COMMAND and COMMAND-PARAMS start the agent.  ENVIRONMENT-VARIABLES
are strings \"VAR=value\" passed to that process.  AUTH is `auto',
`api-key', or `cached-token'.  PROFILE is `chat' or `agent' and is
the default for buffers that do not set `gptel-acp-profile'.
CLIENT is the live acp.el client.  SESSIONS maps a session key to
the id and the hashes of history already sent."
  (command "grok")
  (command-params '("agent" "stdio"))
  environment-variables
  (auth 'auto)
  (profile 'chat)
  client
  sessions)

;;;###autoload
(cl-defun gptel-make-acp
    (name &key
          (command "grok")
          (command-params '("agent" "stdio"))
          environment-variables
          (auth 'auto)
          (profile 'chat)
          (stream t)
          (models '(grok))
          (host "acp")
          (endpoint "")
          (protocol "acp")
          key header curl-args request-params)
  "Create an ACP backend named NAME and register it.

COMMAND and COMMAND-PARAMS start the agent.  AUTH is `auto',
`api-key', or `cached-token'.  PROFILE is `chat' or `agent'.
The remaining keys fill the usual `gptel-backend' slots.  KEY
is optional; `auto' auth reads it only when sending."
  (let ((backend
         (gptel--make-acp
          :name name
          :host host
          :endpoint endpoint
          :protocol protocol
          :header header
          :key key
          :models models
          :stream stream
          :curl-args curl-args
          :request-params request-params
          :command command
          :command-params command-params
          :environment-variables environment-variables
          :auth auth
          :profile profile)))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    backend))

(put 'grok-4.7-build-fast :capabilities '(media json reasoning))
(put 'grok-4.7-build-fast :mime-types
     '("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf"))
(put 'grok-4.7-build-fast :description "Grok 4.7 Fast")
(put 'grok-4.7-build-fast :acp-model-id "grok-4.7-build-fast")
(put 'grok-4.7-build-fast :reasoning-effort "high")
(put 'grok-4.7-build-fast :context-window 256)

(unless (alist-get "Grok" gptel--known-backends nil nil #'equal)
  (gptel-make-acp "Grok"
    :command "grok"
    :command-params '("--no-auto-update" "agent" "stdio")
    :auth 'auto
    :profile 'chat
    :stream t
    :models '(grok-4.7-build-fast)))

;;;; Auth and library

(defun gptel-acp--ensure-acp ()
  "Load acp.el, or signal if it is missing.
Loading `gptel-acp' itself does not load acp.el."
  (unless (and (fboundp 'acp-make-client) (fboundp 'acp-send-request))
    (condition-case err
        (require 'acp)
      (error (error "acp.el is required to use an ACP backend: %s"
                    (error-message-string err)))))
  (unless (fboundp 'acp-send-request)
    (error "acp.el is required to use an ACP backend")))

(defun gptel-acp--coerce-key (key)
  "Return KEY as a non-empty string, or nil.  Never signals."
  (condition-case nil
      (cond
       ((null key) nil)
       ((stringp key)
        (let ((trimmed (string-trim key)))
          (unless (string-empty-p trimmed) trimmed)))
       ((functionp key) (gptel-acp--coerce-key (funcall key)))
       ((and (symbolp key) (boundp key))
        (gptel-acp--coerce-key (symbol-value key)))
       (t nil))
    (error nil)))

(defun gptel-acp--resolve-key (backend)
  "Return an API key string for BACKEND, or nil.
Order is the backend key, `gptel-api-key', then `XAI_API_KEY'.
This does not log the key."
  (or (gptel-acp--coerce-key (gptel-backend-key backend))
      (gptel-acp--coerce-key (and (boundp 'gptel-api-key) gptel-api-key))
      (let ((env (getenv "XAI_API_KEY")))
        (and (stringp env) (not (string-empty-p (string-trim env)))
             (string-trim env)))))

(defun gptel-acp--cached-token-available-p ()
  "Return non-nil when a grok login cache exists."
  (file-readable-p (expand-file-name "~/.grok/auth.json")))

(defun gptel-acp--choose-auth (backend)
  "Return (METHOD-ID . KEY) for BACKEND.
KEY is nil when the method is `cached_token'.  METHOD-ID is a string."
  (pcase (gptel-acp-auth backend)
    ('cached-token (cons "cached_token" nil))
    ('api-key (cons "xai.api_key" (gptel-acp--resolve-key backend)))
    (_ (if-let* ((key (gptel-acp--resolve-key backend)))
           (cons "xai.api_key" key)
         (cons "cached_token" nil)))))

(defun gptel-acp--effective-profile (backend)
  "Return `chat' or `agent' for BACKEND.
Read the buffer-local profile from the chat buffer captured for
this request.  The prompt copy does not carry that local."
  (let* ((source (or (and (buffer-live-p gptel-acp--request-buffer)
                          gptel-acp--request-buffer)
                     (current-buffer)))
         (profile (or (buffer-local-value 'gptel-acp-profile source)
                      (gptel-acp-profile backend)
                      'chat)))
    (if (eq profile 'agent) 'agent 'chat)))

;;;; Client lifecycle

(defun gptel-acp--client-live-p (client)
  "Return non-nil when CLIENT has a live process."
  (and client (fboundp 'acp--client-started-p) (acp--client-started-p client)))

(defun gptel-acp--put (alist key value)
  "Set KEY to VALUE in ALIST without replacing ALIST's first cons.
Emacs 31 `map-put!' signals `map-not-inplace' when the key is new,
because adding it would change which cons existing holders see."
  (when alist
    (if-let* ((cell (assoc key alist)))
        (setcdr cell value)
      (setcdr alist (cons (cons key value) (cdr alist)))))
  alist)

(defun gptel-acp--make-acp-client (backend _profile environment)
  "Create a real acp client for BACKEND and ENVIRONMENT."
  (let ((buf (generate-new-buffer " *gptel-acp*")))
    (let ((client
           (acp-make-client
            :command (gptel-acp-command backend)
            :command-params (mapcar (lambda (arg) (format "%s" arg))
                                    (gptel-acp-command-params backend))
            :environment-variables environment
            :context-buffer buf)))
      (gptel-acp--put client :gptel-owned-buffer buf)
      client)))

(defun gptel-acp--drop-client (backend &optional client)
  "Drop CLIENT, or BACKEND's client, and forget every session.
A dead process is discarded without assuming shutdown is safe."
  (let ((client (or client (gptel-acp-client backend))))
    (when client
      (when (and (fboundp 'acp-shutdown)
                 (ignore-errors (acp--client-started-p client)))
        (ignore-errors (acp-shutdown :client client)))
      (when (and (fboundp 'acp-shutdown)
                 (map-elt client :process))
        (ignore-errors (acp-shutdown :client client)))
      (when-let* ((buf (map-elt client :gptel-owned-buffer)))
        (when (buffer-live-p buf) (kill-buffer buf))))
    (when (and backend (eq client (gptel-acp-client backend)))
      (setf (gptel-acp-client backend) nil)
      (setf (gptel-acp-sessions backend) nil))))

(defun gptel-acp--shutdown-all ()
  "Shut down every ACP client.  Used from `kill-emacs-hook'."
  (dolist (cell gptel--known-backends)
    (when (gptel-acp-p (cdr cell))
      (ignore-errors (gptel-acp--drop-client (cdr cell))))))

(add-hook 'kill-emacs-hook #'gptel-acp--shutdown-all)

(defun gptel-acp--subscribe (client)
  "Subscribe CLIENT once to agent notifications and requests."
  (unless (map-elt client :gptel-subscribed)
    (acp-subscribe-to-notifications
     :client client
     :on-notification (lambda (note) (gptel-acp--on-notification client note)))
    (acp-subscribe-to-requests
     :client client
     :on-request (lambda (req) (gptel-acp--on-request client req)))
    (gptel-acp--put client :gptel-subscribed t)))

(defun gptel-acp--new-client (backend profile environment)
  "Create, subscribe, and return a client for BACKEND at PROFILE."
  (let ((client (funcall (or gptel-acp--client-maker
                             #'gptel-acp--make-acp-client)
                         backend profile environment)))
    (gptel-acp--put client :gptel-profile profile)
    (gptel-acp--put client :gptel-terminals (make-hash-table :test 'equal))
    (gptel-acp--subscribe client)
    client))

(defun gptel-acp--environment (backend method key)
  "Environment strings for BACKEND when authenticating with METHOD and KEY.
`cached_token' does not invent an API key.  An existing
`XAI_API_KEY' is left untouched."
  (let ((env (copy-sequence (gptel-acp-environment-variables backend))))
    (when (and (equal method "xai.api_key")
               (stringp key)
               (not (string-empty-p key))
               (or (null (getenv "XAI_API_KEY"))
                   (string-empty-p (getenv "XAI_API_KEY"))))
      (push (concat "XAI_API_KEY=" key) env))
    env))

(defun gptel-acp--callback-buffer (client)
  "Return a live buffer for CLIENT callbacks, or nil."
  (let ((buf (or (map-elt client :gptel-owned-buffer)
                 (map-elt client :context-buffer))))
    (and (bufferp buf) (buffer-live-p buf) buf)))

(defun gptel-acp--rpc (client request on-success on-failure)
  "Send REQUEST on CLIENT.  Record it when `gptel-acp--recording' is set."
  (when gptel-acp--recording
    (push (list :method (map-elt request :method)
                :params (copy-tree (map-elt request :params) t))
          gptel-acp--recording))
  (acp-send-request
   :client client
   :request request
   :buffer (gptel-acp--callback-buffer client)
   :on-success on-success
   :on-failure on-failure))

;;;; Capabilities and handshake

(defun gptel-acp--initialize-request (profile)
  "Build an initialize request for PROFILE.
Chat does not advertise writeTextFile or terminal."
  (let* ((agent (eq profile 'agent))
         (req (acp-make-initialize-request
               :protocol-version 1
               :client-info `((name . "gptel")
                              (title . "gptel")
                              (version . ,(if (boundp 'gptel-version)
                                              gptel-version
                                            "0.9.9.6")))
               :read-text-file-capability t
               :write-text-file-capability agent))
         (params (map-elt req :params))
         (caps (map-elt params 'clientCapabilities)))
    (gptel-acp--put params 'clientCapabilities
              (append caps `((terminal . ,(if agent t :false)))))
    req))

(defun gptel-acp--note-initialize (client profile request result)
  "Remember PROFILE, REQUEST, and initialize RESULT on CLIENT."
  (gptel-acp--put client :gptel-profile profile)
  (gptel-acp--put client :gptel-initialize (map-elt request :params))
  (gptel-acp--put client :gptel-supports-steering
            (eq (map-nested-elt result '(_meta steering supported)) t))
  (gptel-acp--put client :gptel-supports-load
            (eq (map-nested-elt result '(agentCapabilities loadSession)) t))
  (gptel-acp--put client :gptel-ready nil))

(defun gptel-acp--ensure-client (backend profile info method key)
  "Return a client for BACKEND at PROFILE, creating it if needed.
INFO is the request plist.  A profile change or a dead process
drops the old client and every session.  Rewrite requests that
need chat while the shared client is an agent get a separate
client stored on INFO."
  (let* ((rewrite (or (plist-get info :gptel-acp-rewrite)
                      gptel-acp--request-rewrite
                      (eq (plist-get info :callback) 'gptel--rewrite-callback)))
         (shared (gptel-acp-client backend))
         (oneshot (and rewrite
                       (or (null (gptel-acp--client-live-p shared))
                           (not (eq (map-elt shared :gptel-profile) 'chat))))))
    (plist-put info :gptel-acp-rewrite rewrite)
    (if oneshot
        (or (plist-get info :gptel-acp-oneshot)
            (let ((client (gptel-acp--new-client
                           backend 'chat
                           (gptel-acp--environment backend method key))))
              (plist-put info :gptel-acp-oneshot client)
              client))
      (cond
       ((and (gptel-acp--client-live-p shared)
             (eq (map-elt shared :gptel-profile) profile))
        shared)
       (t
        (gptel-acp--drop-client backend shared)
        (let ((client (gptel-acp--new-client
                       backend profile
                       (gptel-acp--environment backend method key))))
          (setf (gptel-acp-client backend) client)
          client))))))

(defun gptel-acp--ready (client cont fail)
  "Run handshake on CLIENT, then call CONT.  FAIL receives an error."
  (if (map-elt client :gptel-ready)
      (funcall cont)
    (let* ((profile (map-elt client :gptel-profile))
           (request (gptel-acp--initialize-request profile)))
      (gptel-acp--put client :gptel-initialize (map-elt request :params))
      (gptel-acp--rpc
       client request
       (lambda (result)
         (gptel-acp--note-initialize client profile request result)
         (gptel-acp--rpc
          client
          (acp-make-authenticate-request
           :method-id (or (map-elt client :gptel-auth-method) "cached_token"))
          (lambda (&rest _)
            (gptel-acp--put client :gptel-ready t)
            (funcall cont))
          fail))
       fail))))

;;;; Session identity

(defun gptel-acp--org-branch ()
  "Return the Org heading lineage at point, or an empty string."
  (if (and (derived-mode-p 'org-mode) (fboundp 'org-get-heading))
      (save-excursion
        (let ((heads nil))
          (condition-case nil
              (progn
                (unless (org-at-heading-p) (org-back-to-heading t))
                (push (org-get-heading t t t t) heads)
                (while (org-up-heading-safe)
                  (push (org-get-heading t t t t) heads)))
            (error nil))
          (mapconcat #'identity heads "/")))
    ""))

(defun gptel-acp--session-key (buffer position)
  "Return the session key for BUFFER at POSITION.
The key is the buffer, the Org topic, and the heading lineage."
  (with-current-buffer buffer
    (save-excursion
      (when (and position (marker-buffer position))
        (goto-char position))
      (let ((topic "")
            (branch ""))
        (when (derived-mode-p 'org-mode)
          (require 'gptel-org)
          (when-let* ((start (gptel-org--get-topic-start)))
            (setq topic (or (org-entry-get start "GPTEL_TOPIC" t) "")))
          (setq branch (or (gptel-acp--org-branch) "")))
        (format "%s\n%s\n%s"
                (or (buffer-file-name buffer) (buffer-name buffer))
                topic branch)))))

(defun gptel-acp--turn-fingerprint (turn)
  "Stable rendering of TURN for hashing."
  (list (plist-get turn :role)
        (mapcar (lambda (part)
                  (list (plist-get part :text)
                        (plist-get part :media)
                        (plist-get part :mime)
                        (plist-get part :textfile)
                        (plist-get part :url)))
                (plist-get turn :parts))))

(defun gptel-acp--hash-turns (turns)
  "Hash TURNS with `secure-hash'."
  (secure-hash 'sha256 (prin1-to-string
                        (mapcar #'gptel-acp--turn-fingerprint turns))))

(defun gptel-acp--system-text ()
  "Return the current system prompt as a string, or nil."
  (cond
   ((stringp gptel-system-prompt) gptel-system-prompt)
   ((listp gptel-system-prompt)
    (mapconcat (lambda (part)
                 (if (stringp part) part (prin1-to-string part)))
               (delq nil gptel-system-prompt)
               "\n"))
   (t nil)))

(defun gptel-acp--config-string (value)
  "Return VALUE when it is a non-empty string, otherwise nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun gptel-acp--model-id ()
  "ACP model id for `gptel-model', or nil when the model has none."
  (gptel-acp--config-string (get gptel-model :acp-model-id)))

(defun gptel-acp--reasoning-effort ()
  "ACP reasoning effort id for this request, or nil."
  (gptel-acp--config-string
   (or (and (boundp 'gptel-acp-reasoning-effort) gptel-acp-reasoning-effort)
       (get gptel-model :reasoning-effort))))

(defun gptel-acp--system-hash (profile)
  "Hash the advisory inputs that force a new ACP session."
  (secure-hash
   'sha256
   (prin1-to-string
    (list profile
          (gptel-acp--model-id)
          (gptel-acp--reasoning-effort)
          (gptel-acp--system-text)
          gptel-temperature
          gptel-max-tokens
          gptel--schema
          (and (boundp 'gptel-tools)
               (mapcar #'gptel-tool-name gptel-tools))
          gptel-confirm-tool-calls))))

(defun gptel-acp--nonempty-local (buffer symbol)
  "Return BUFFER's local SYMBOL when it is a non-empty string."
  (when (buffer-live-p buffer)
    (let ((value (buffer-local-value symbol buffer)))
      (and (stringp value) (not (string-empty-p value)) value))))

(defun gptel-acp--saved-hashes-allow-load-p
    (saved-history saved-system history-hash system-hash)
  "Return non-nil when a file-saved session may be loaded.
Both hashes absent is a legacy file and may load.  Both present
and equal to HISTORY-HASH and SYSTEM-HASH may load.  Any other
combination must open a new session and replay."
  (cond
   ((and (null saved-history) (null saved-system)) t)
   ((and saved-history saved-system
         (equal saved-history history-hash)
         (equal saved-system system-hash))
    t)
   (t nil)))

(defun gptel-acp--advisory (profile)
  "Advisory text for the first prompt of a session at PROFILE.
Temperature, max tokens, and schema stay buffer-local.  They are
described here and are not protocol fields.  The system prompt is
sent as `_meta.systemPromptOverride'."
  (let ((bits nil))
    (when (numberp gptel-temperature)
      (push (format "Advisory temperature: %s.  This is not an ACP protocol field and was not accepted as one."
                    gptel-temperature)
            bits))
    (when gptel-max-tokens
      (push (format "Advisory max tokens: %s.  This is not an ACP protocol field and was not accepted as one."
                    gptel-max-tokens)
            bits))
    (when gptel--schema
      (push (format "Advisory JSON schema, not an ACP protocol field:\n%s"
                    (if (stringp gptel--schema)
                        gptel--schema
                      (prin1-to-string gptel--schema)))
            bits))
    (when (and (eq profile 'chat) gptel-tools)
      (push (format "Emacs tools configured here run only when an HTTP backend is selected, not on this ACP chat session: %s."
                    (mapconcat #'gptel-tool-name gptel-tools ", "))
            bits))
    (when bits (mapconcat #'identity (nreverse bits) "\n\n"))))

;;;; Prompt blocks

(defun gptel-acp--text-block (text)
  "An ACP text content block for TEXT."
  `((type . "text") (text . ,(or text ""))))

(defun gptel-acp--part-blocks (part)
  "Convert a gptel prompt PART into ACP content blocks."
  (cond
   ((plist-get part :text)
    (list (gptel-acp--text-block (plist-get part :text))))
   ((plist-get part :media)
    (let* ((path (plist-get part :media))
           (mime (or (plist-get part :mime) "application/octet-stream"))
           (uri (concat "file://" (expand-file-name path)))
           (data (gptel--base64-encode path)))
      (list
       (if (string-prefix-p "image/" mime)
           `((type . "image") (mimeType . ,mime) (data . ,data) (uri . ,uri))
         `((type . "resource")
           (resource . ((uri . ,uri) (mimeType . ,mime) (blob . ,data))))))))
   ((plist-get part :textfile)
    (list (gptel-acp--text-block
           (with-temp-buffer
             (gptel--insert-file-string (plist-get part :textfile))
             (buffer-string)))))
   ((plist-get part :url)
    (list `((type . "resource_link")
            (uri . ,(plist-get part :url))
            (name . ,(plist-get part :url))
            (mimeType . ,(or (plist-get part :mime) "text/html")))))
   (t nil)))

(defun gptel-acp--turn-blocks (turn)
  "ACP blocks for TURN."
  (cl-loop for part in (plist-get turn :parts)
           append (gptel-acp--part-blocks part)))

(defun gptel-acp--turn-text (turn)
  "Plain text of TURN, for a replay transcript."
  (mapconcat
   (lambda (part)
     (or (plist-get part :text)
         (when-let* ((path (plist-get part :textfile)))
           (format "[file:%s]" path))
         (when-let* ((path (plist-get part :media)))
           (format "[media:%s]" path))
         (when-let* ((url (plist-get part :url)))
           (format "[url:%s]" url))
         ""))
   (plist-get turn :parts)
   ""))

(defun gptel-acp--ensure-blocks (blocks)
  "Return BLOCKS, or one empty text block."
  (or blocks (list (gptel-acp--text-block ""))))

(defun gptel-acp--replay-blocks (prompts advisory)
  "Blocks that replay PROMPTS, with ADVISORY text on a new session.
The last user turn stays the question.  Earlier turns are transcript."
  (let* ((last (car (last prompts)))
         (earlier (butlast prompts))
         (blocks nil))
    (when advisory
      (push (gptel-acp--text-block advisory) blocks))
    (dolist (turn earlier)
      (push (gptel-acp--text-block
             (format "%s:\n%s"
                     (if (equal (plist-get turn :role) "assistant")
                         "Assistant"
                       "User")
                     (gptel-acp--turn-text turn)))
            blocks))
    (when earlier
      (push (gptel-acp--text-block
             "The last user message below is the question to answer.  Earlier turns are the conversation so far.")
            blocks))
    (gptel-acp--ensure-blocks
     (append (nreverse blocks)
             (if last
                 (gptel-acp--turn-blocks last)
               (list (gptel-acp--text-block "")))))))

(defun gptel-acp--incremental-blocks (prompts)
  "Blocks for the last turn in PROMPTS only."
  (gptel-acp--ensure-blocks
   (gptel-acp--turn-blocks (car (last prompts)))))

(defun gptel-acp--request-buffer ()
  "Buffer the ACP session key is computed from."
  (or (and (buffer-live-p gptel-acp--request-buffer) gptel-acp--request-buffer)
      (current-buffer)))

;;;; Parsers

(defun gptel-acp--parts-from-region (beg end)
  "Prompt parts between BEG and END in the current buffer."
  (if gptel-track-media
      (mapcar (lambda (part)
                ;; `gptel--parse-media-links' returns plists already.
                part)
              (gptel--parse-media-links major-mode beg end))
    (when-let* ((text (gptel--trim-prefixes
                       (buffer-substring-no-properties beg end))))
      (list (list :text text)))))

(cl-defmethod gptel--parse-buffer ((_backend gptel-acp) &optional max-entries)
  "Parse the current buffer into ACP turns for BACKEND.
Include up to MAX-ENTRIES user/assistant exchanges."
  (let ((prompts nil)
        (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((text (gptel--trim-prefixes
                                (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :parts (list (list :text text)))
                     prompts)))
            ('ignore)
            ((pred (lambda (prop) (eq (car-safe prop) 'tool))))
            ('nil
             (and max-entries (cl-decf max-entries))
             (when-let* ((parts (gptel-acp--parts-from-region (point) prev-pt)))
               (push (list :role "user" :parts parts) prompts))))
          (setq prev-pt (point)))
      (when-let* ((text (gptel--trim-prefixes
                         (buffer-substring-no-properties
                          (point-min) (point)))))
        (push (list :role "user" :parts (list (list :text text))) prompts)))
    prompts))

(cl-defmethod gptel--parse-list ((_backend gptel-acp) prompt-list)
  "Parse PROMPT-LIST into ACP turns."
  (if (consp (car-safe prompt-list))
      (mapcar
       (lambda (entry)
         (pcase entry
           (`(prompt . ,msg)
            (list :role "user"
                  :parts (list (list :text (or (car-safe msg) msg)))))
           (`(response . ,msg)
            (list :role "assistant"
                  :parts (list (list :text (or (car-safe msg) msg)))))
           (_
            (list :role "user"
                  :parts (list (list :text (format "%s" entry)))))))
       prompt-list)
    (cl-loop for text in prompt-list
             for user = t then (not user)
             when (and text (not (string-empty-p text)))
             collect (list :role (if user "user" "assistant")
                           :parts (list (list :text text))))))

(cl-defmethod gptel--inject-media ((_backend gptel-acp) prompts)
  "Prepend `gptel-context' media onto the first user turn in PROMPTS."
  (when-let* ((media (gptel-context--collect-media))
              (first (car prompts))
              ((equal (plist-get first :role) "user")))
    (plist-put first :parts
               (append media (plist-get first :parts)))))

(cl-defmethod gptel--request-data ((backend gptel-acp) prompts)
  "Build the ACP prompt description for BACKEND and PROMPTS.
This does not start a process.  The plist is what a dry run shows.
It contains no API key."
  (let* ((buffer (gptel-acp--request-buffer))
         (profile (if gptel-acp--request-rewrite
                      'chat
                    (gptel-acp--effective-profile backend)))
         (key (gptel-acp--session-key buffer gptel-acp--request-position))
         (record (cdr (assoc key (gptel-acp-sessions backend))))
         (history (butlast prompts))
         (history-hash (gptel-acp--hash-turns history))
         (confirm (buffer-local-value 'gptel-confirm-tool-calls buffer))
         (system-hash
          (let ((gptel-confirm-tool-calls confirm))
            (gptel-acp--system-hash profile)))
         (matched (and (not gptel-acp--request-rewrite)
                       record
                       (equal (plist-get record :history-hash) history-hash)
                       (equal (plist-get record :system-hash) system-hash)
                       (eq (plist-get record :profile) profile)
                       (plist-get record :id)))
         (advisory (gptel-acp--advisory profile))
         (replay-blocks (gptel-acp--replay-blocks prompts advisory))
         (incremental (gptel-acp--incremental-blocks prompts))
         (saved (buffer-local-value 'gptel-acp-session-id buffer))
         (saved-id (and (stringp saved) (not (string-empty-p saved)) saved))
         (saved-history (gptel-acp--nonempty-local buffer 'gptel-acp-history-hash))
         (saved-system (gptel-acp--nonempty-local buffer 'gptel-acp-system-hash))
         (system-text (gptel-acp--system-text)))
    (when (and (eq profile 'chat) gptel-tools (not matched))
      (message "ACP chat: %s run only on an HTTP backend"
               (mapconcat #'gptel-tool-name gptel-tools ", ")))
    ;; Load only when this Emacs has no memory of the session and the
    ;; file-saved hashes still match.  A mismatch, in memory or in the
    ;; file, replays into a new session.  A saved id with no hashes is
    ;; a legacy file and may still load.
    (list :acp t
          :profile profile
          :acp-model-id (gptel-acp--model-id)
          :acp-effort (gptel-acp--reasoning-effort)
          :acp-system system-text
          :acp-yolo (and (null confirm) t)
          :replay (not matched)
          :load (and (not matched)
                     (not record)
                     (not gptel-acp--request-rewrite)
                     saved-id
                     (gptel-acp--saved-hashes-allow-load-p
                      saved-history saved-system history-hash system-hash))
          :oneshot (and gptel-acp--request-rewrite t)
          :session-key key
          :session-id matched
          :saved-session-id saved-id
          :history-hash history-hash
          :system-hash system-hash
          :turns prompts
          :advisory advisory
          :prompt (if matched incremental replay-blocks)
          :incremental-prompt incremental
          :replay-prompt replay-blocks)))

;;;; FSM

(defun gptel-acp--permission-p (info)
  "Return non-nil when INFO is waiting on an ACP permission prompt."
  (plist-get info :gptel-acp-permission))

(defun gptel-acp--install-fsm (fsm)
  "Install the ACP transition table on FSM for this request only.
DONE, ERRS, and ABRT keep whatever handlers the request already had."
  (let ((handlers (gptel-fsm-handlers fsm)))
    (setf (gptel-fsm-table fsm)
          `((WAIT . ((,#'gptel--error-p . ERRS)
                     (t . ASTR)))
            (ASTR . ((,#'gptel--error-p . ERRS)
                     (,#'gptel-acp--permission-p . APERM)
                     (t . DONE)))
            (APERM . ((t . ASTR)))))
    (setf (gptel-fsm-handlers fsm)
          (append
           `((ASTR ,#'gptel-acp--handle-astr)
             (APERM ,#'gptel-acp--handle-aperm))
           (cl-remove-if (lambda (entry)
                           (memq (car entry) '(ASTR APERM)))
                         handlers)))))

(defun gptel-acp--handle-astr (fsm)
  "Prepare the buffer when FSM enters ASTR."
  (let ((info (gptel-fsm-info fsm)))
    (unless (plist-get info :gptel-acp-preinsert)
      (plist-put info :gptel-acp-preinsert t)
      (plist-put info :http-status "200")
      (when (fboundp 'gptel--handle-pre-insert)
        (gptel--handle-pre-insert fsm)))))

(defun gptel-acp--handle-aperm (fsm)
  "Show the pending ACP permission prompt for FSM.
The callback receives the usual tool-call shape.  This state does
not enter `gptel--handle-tool-use'."
  (let* ((info (gptel-fsm-info fsm))
         (calls (plist-get info :gptel-acp-permission))
         (buf (plist-get info :buffer)))
    (when (and (buffer-live-p buf) (fboundp 'gptel--update-status))
      (with-current-buffer buf
        (when gptel-mode
          (gptel--update-status " Run tools?" 'mode-line-emphasis))))
    (gptel-acp--deliver info `(tool-call . ,calls))))

(defun gptel-acp--deliver (info response)
  "Call INFO's callback with RESPONSE, choosing the standard inserter."
  (let ((callback
         (or (plist-get info :callback)
             (and (plist-get info :stream)
                  (fboundp 'gptel-curl--stream-insert-response)
                  #'gptel-curl--stream-insert-response)
             (and (fboundp 'gptel--insert-response)
                  #'gptel--insert-response))))
    (when (functionp callback)
      (funcall callback response info))))

(defun gptel-acp--error-plist (err)
  "Convert an ACP error ERR into a plist `gptel--handle-error' understands."
  (cond
   ((stringp err) (list :message err))
   ((and (listp err) (keywordp (car err))) err)
   ((listp err)
    (list :type (or (map-elt err 'code) (map-elt err :code))
          :message (or (map-elt err 'message)
                       (map-elt err :message)
                       "ACP request failed")))
   (t (list :message (prin1-to-string err)))))

(defun gptel-acp--process-died-p (err)
  "Return non-nil when ERR says the agent process exited."
  (let ((msg (or (plist-get (gptel-acp--error-plist err) :message) "")))
    (string-match-p "Agent process ended" msg)))

(defun gptel-acp--defer (fsm state)
  "Move FSM to STATE, waiting until WAIT's handler returns if needed."
  (let ((info (gptel-fsm-info fsm)))
    (if (and gptel-acp--in-wait (eq (gptel-fsm-state fsm) 'WAIT))
        (plist-put info :gptel-acp-defer-state state)
      (unless (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))
        (gptel--fsm-transition fsm state)))))

(defun gptel-acp--clear-alist (client)
  "Remove CLIENT's process from `gptel--request-alist'."
  (when-let* ((proc (and client (map-elt client :process))))
    (setf (alist-get proc gptel--request-alist nil 'remove) nil)))

(defun gptel-acp--fail (fsm err)
  "Move FSM to ERRS because of ERR."
  (let ((info (gptel-fsm-info fsm)))
    (unless (or (plist-get info :gptel-acp-cancelled)
                (memq (gptel-fsm-state fsm) '(ERRS ABRT DONE)))
      (plist-put info :error (gptel-acp--error-plist err))
      (plist-put info :status "ACP error")
      (gptel-acp--deliver info nil)
      (gptel-acp--clear-alist (plist-get info :gptel-acp-client))
      (when-let* ((oneshot (plist-get info :gptel-acp-oneshot)))
        (ignore-errors (acp-shutdown :client oneshot))
        (plist-put info :gptel-acp-oneshot nil))
      (gptel-acp--defer fsm 'ERRS))))

(defun gptel-acp--remember-session (backend info text)
  "Store the session hash after a successful turn whose reply is TEXT."
  (unless (or (plist-get info :oneshot)
              (plist-get (plist-get info :data) :oneshot))
    (let* ((data (plist-get info :data))
           (key (plist-get data :session-key))
           (sid (plist-get info :gptel-acp-session-id))
           (turns (append (plist-get data :turns)
                          (list (list :role "assistant"
                                      :parts (list (list :text (or text "")))))))
           (buf (plist-get info :buffer)))
      (when (and key sid)
        (setf (alist-get key (gptel-acp-sessions backend) nil nil #'equal)
              (list :id sid
                    :history-hash (gptel-acp--hash-turns turns)
                    :system-hash (plist-get data :system-hash)
                    :profile (plist-get data :profile)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local gptel-acp-session-id sid)
            (setq-local gptel-acp-history-hash (gptel-acp--hash-turns turns))
            (setq-local gptel-acp-system-hash (plist-get data :system-hash))
            (setq-local gptel-acp-profile (plist-get data :profile))))))))

(defun gptel-acp--end-marker (info)
  "Deliver the streaming end marker, or one full string."
  (let ((text (or (plist-get info :gptel-acp-text) ""))
        (reasoning (plist-get info :gptel-acp-reasoning)))
    (if (plist-get info :stream)
        (progn
          (when (plist-get info :gptel-acp-reasoning-open)
            (gptel-acp--deliver info '(reasoning . t))
            (plist-put info :gptel-acp-reasoning-open nil))
          (gptel-acp--deliver info t))
      (when (and reasoning (not (string-empty-p reasoning)))
        (gptel-acp--deliver info (cons 'reasoning reasoning)))
      (gptel-acp--deliver info text))))

(defun gptel-acp--finish (fsm)
  "Finish FSM after the prompt result, unless a steer follow-up is queued."
  (let ((info (gptel-fsm-info fsm)))
    (cond
     ((plist-get info :gptel-acp-cancelled) nil)
     ((eq (gptel-fsm-state fsm) 'APERM)
      (plist-put info :gptel-acp-pending-result t))
     ((and (not (plist-get info :gptel-acp-followed))
           (gptel-acp--take-steer info))
      (plist-put info :gptel-acp-followed t)
      (gptel-acp--send-prompt
       (plist-get info :gptel-acp-client)
       fsm
       (list (gptel-acp--text-block (plist-get info :gptel-acp-steer-text)))))
     (t (gptel-acp--finish-now fsm)))))

(defun gptel-acp--finish-now (fsm)
  "Deliver the end of FSM and move to DONE."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (unless (or (plist-get info :gptel-acp-cancelled)
                (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
      (plist-put info :tool-use nil)
      (plist-put info :http-status "200")
      (gptel-acp--end-marker info)
      (when (gptel-acp-p backend)
        (gptel-acp--remember-session
         backend info (plist-get info :gptel-acp-text)))
      (gptel-acp--clear-alist (plist-get info :gptel-acp-client))
      (when-let* ((oneshot (plist-get info :gptel-acp-oneshot)))
        (ignore-errors (acp-shutdown :client oneshot))
        (plist-put info :gptel-acp-oneshot nil))
      (gptel-acp--put (plist-get info :gptel-acp-client) :gptel-fsm nil)
      (gptel-acp--defer fsm 'DONE))))

(defun gptel-acp--register-abort (client fsm session-id)
  "Register CLIENT so `gptel-abort' can cancel SESSION-ID."
  (let* ((info (gptel-fsm-info fsm))
         (proc (map-elt client :process))
         (abort
          (lambda ()
            (plist-put info :gptel-acp-cancelled t)
            (when (and session-id (gptel-acp--client-live-p client))
              (ignore-errors
                (acp-send-notification
                 :client client
                 :notification
                 (acp-make-session-cancel-notification
                  :session-id session-id
                  :reason "gptel-abort")))))))
    (when (processp proc)
      (setf (alist-get proc gptel--request-alist) (cons fsm abort)))))

(defun gptel-acp--apply-config (client fsm sid then)
  "Set the session model and effort, then call THEN.
SID is the ACP session id.  Options with no value are skipped.
Model is applied before reasoning effort, because effort refers
to the model that is current after the model change."
  (let* ((data (plist-get (gptel-fsm-info fsm) :data))
         (steps (delq nil
                      (list
                       (when-let* ((id (plist-get data :acp-model-id)))
                         (cons "model" id))
                       (when-let* ((effort (plist-get data :acp-effort)))
                         (cons "reasoning_effort" effort))))))
    (gptel-acp--apply-config-steps client fsm sid steps then)))

(defun gptel-acp--apply-config-steps (client fsm sid steps then)
  "Send STEPS of (config-id . value) for SID, then call THEN."
  (if (null steps)
      (funcall then)
    (let ((step (car steps)))
      (gptel-acp--rpc
       client
       (acp-make-session-set-config-option-request
        :session-id sid
        :config-id (car step)
        :value (cdr step))
       (lambda (&rest _)
         (gptel-acp--apply-config-steps client fsm sid (cdr steps) then))
       (lambda (err) (gptel-acp--fail fsm err))))))

(defun gptel-acp--send-prompt (client fsm blocks)
  "Send session/prompt BLOCKS on CLIENT for FSM."
  (let* ((info (gptel-fsm-info fsm))
         (sid (plist-get info :gptel-acp-session-id)))
    (gptel-acp--register-abort client fsm sid)
    (gptel-acp--rpc
     client
     (acp-make-session-prompt-request :session-id sid :prompt blocks)
     (lambda (result)
       (when-let* ((usage (map-elt result 'usage)))
         (plist-put info :tokens
                    (list (or (map-elt usage 'inputTokens)
                              (map-elt usage 'input_tokens))
                          (or (map-elt usage 'outputTokens)
                              (map-elt usage 'output_tokens)))))
       (gptel-acp--finish fsm))
     (lambda (err)
       (if (gptel-acp--process-died-p err)
           (progn
             (gptel-acp--drop-client (plist-get info :backend) client)
             (gptel-acp--fail fsm err))
         (gptel-acp--fail fsm err))))
    (gptel-acp--entered-stream fsm)))

(defun gptel-acp--entered-stream (fsm)
  "Enter ASTR now that session/prompt has been sent."
  (let ((info (gptel-fsm-info fsm)))
    (unless (or (plist-get info :gptel-acp-defer-state)
                (plist-get info :gptel-acp-cancelled)
                (not (eq (gptel-fsm-state fsm) 'WAIT)))
      (if gptel-acp--in-wait
          (plist-put info :gptel-acp-defer-state 'ASTR)
        (gptel--fsm-transition fsm 'ASTR)))))

(defun gptel-acp--session-meta (data)
  "Return session/new _meta for DATA, or nil.
`systemPromptOverride' is the system text captured when the
request was built.  `yoloMode' is set only when tool confirmation
was off at that time.  Both are read from DATA so an ACP callback
buffer cannot substitute its own values."
  (let ((meta nil)
        (system (plist-get data :acp-system)))
    (when (and (stringp system) (not (string-empty-p system)))
      (push `(systemPromptOverride . ,system) meta))
    (when (plist-get data :acp-yolo)
      (push '(yoloMode . t) meta))
    (nreverse meta)))

(defun gptel-acp--cwd (info)
  "Working directory for the request in INFO."
  (let ((buf (plist-get info :buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (or (and buffer-file-name (file-name-directory buffer-file-name))
              default-directory))
      default-directory)))

(defun gptel-acp--open-session (backend client fsm replay)
  "Create or load a session, then prompt.
REPLAY non-nil sends the full transcript.  A saved session id is
loaded when the agent supports it; load failure replays the buffer."
  (let* ((info (gptel-fsm-info fsm))
         (data (plist-get info :data))
         (cwd (gptel-acp--cwd info))
         (meta (gptel-acp--session-meta data))
         (saved (plist-get data :saved-session-id))
         (existing (plist-get data :session-id)))
    (cond
     ((and (not replay) existing)
      (plist-put info :gptel-acp-session-id existing)
      (gptel-acp--send-prompt client fsm (plist-get data :prompt)))
     ((and replay (plist-get data :load) (not (plist-get data :oneshot))
           (eq (map-elt client :gptel-supports-load) t)
           (not (plist-get info :gptel-acp-load-failed)))
      (plist-put info :gptel-acp-loading t)
      (plist-put info :gptel-acp-session-id saved)
      (gptel-acp--rpc
       client
       (acp-make-session-load-request
        :session-id saved :cwd cwd :mcp-servers [] :meta meta)
       (lambda (&rest _)
         (plist-put info :gptel-acp-loading nil)
         (gptel-acp--apply-config
          client fsm saved
          (lambda ()
            (gptel-acp--send-prompt
             client fsm (plist-get data :incremental-prompt)))))
       (lambda (&rest _)
         (plist-put info :gptel-acp-loading nil)
         (plist-put info :gptel-acp-load-failed t)
         (gptel-acp--open-session backend client fsm t))))
     (t
      (gptel-acp--rpc
       client
       (acp-make-session-new-request :cwd cwd :mcp-servers [] :meta meta)
       (lambda (result)
         (let ((sid (map-elt result 'sessionId)))
           (plist-put info :gptel-acp-session-id sid)
           (unless (plist-get data :oneshot)
             (when (buffer-live-p (plist-get info :buffer))
               (with-current-buffer (plist-get info :buffer)
                 (setq-local gptel-acp-session-id sid))))
           (gptel-acp--apply-config
            client fsm sid
            (lambda ()
              (gptel-acp--send-prompt
               client fsm
               (if replay
                   (plist-get data :replay-prompt)
                 (plist-get data :prompt)))))))
       (lambda (err) (gptel-acp--fail fsm err)))))))

(defun gptel-acp--begin (backend fsm)
  "Start or continue the ACP turn described by FSM on BACKEND."
  (let* ((info (gptel-fsm-info fsm))
         (data (plist-get info :data))
         (profile (plist-get data :profile))
         (forced (plist-get info :gptel-acp-force-auth))
         (choice (if forced
                     (cons forced nil)
                   (gptel-acp--choose-auth backend)))
         (method (car choice))
         (key (cdr choice))
         (client (gptel-acp--ensure-client backend profile info method key)))
    (plist-put info :gptel-acp-client client)
    (plist-put info :http-status "200")
    (unless (plist-get info :callback)
      (plist-put info :callback
                 (if (plist-get info :stream)
                     #'gptel-curl--stream-insert-response
                   #'gptel--insert-response)))
    (unless (plist-get info :transformer)
      (when (and (buffer-live-p (plist-get info :buffer))
                 (with-current-buffer (plist-get info :buffer)
                   (and (derived-mode-p 'org-mode)
                        gptel-org-convert-response))
                 (fboundp 'gptel--stream-convert-markdown->org))
        (plist-put info :transformer
                   (gptel--stream-convert-markdown->org
                    (plist-get info :position)))))
    (gptel-acp--put client :gptel-auth-method method)
    (let ((busy (map-elt client :gptel-fsm)))
      (when (and busy (not (eq busy fsm)) (gptel--fsm-live-p busy))
        (gptel-acp--fail fsm "ACP client is busy with another request")))
    (unless (plist-get info :error)
      (gptel-acp--put client :gptel-fsm fsm)
      (gptel-acp--ready
       client
       (lambda ()
         (unless (plist-get info :gptel-acp-cancelled)
           (gptel-acp--open-session
            backend client fsm (plist-get data :replay))))
       (lambda (err)
         (cond
          ((and (not forced)
                (equal method "xai.api_key")
                (eq (gptel-acp-auth backend) 'auto)
                (gptel-acp--cached-token-available-p)
                (not (plist-get info :gptel-acp-auth-retried)))
           (plist-put info :gptel-acp-auth-retried t)
           (plist-put info :gptel-acp-force-auth "cached_token")
           ;; A failed one-shot client is stored on INFO, not on the backend.
           (plist-put info :gptel-acp-oneshot nil)
           (gptel-acp--drop-client backend client)
           (gptel-acp--begin backend fsm))
          (t
           (when (gptel-acp--process-died-p err)
             (gptel-acp--drop-client backend client))
           (gptel-acp--fail fsm err))))))))

(cl-defmethod gptel-backend-send ((backend gptel-acp) fsm)
  "Send FSM through the ACP agent in BACKEND."
  (gptel-acp--ensure-acp)
  (gptel-acp--install-fsm fsm)
  (let ((gptel-acp--in-wait t))
    (condition-case err
        (gptel-acp--begin backend fsm)
      (error (gptel-acp--fail fsm (error-message-string err))))
    (let* ((info (gptel-fsm-info fsm))
           (state (plist-get info :gptel-acp-defer-state)))
      (when state
        (plist-put info :gptel-acp-defer-state nil)
        (unless (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT APERM))
          (gptel--fsm-transition fsm state))))))

;;;; Notifications

(defun gptel-acp--content-text (content)
  "Return the text inside an ACP CONTENT block."
  (cond
   ((null content) "")
   ((stringp content) content)
   ((vectorp content)
    (mapconcat #'gptel-acp--content-text content ""))
   ((listp content)
    (concat (or (map-elt content 'text) "")
            (gptel-acp--content-text (map-elt content 'content))))
   (t "")))

(defun gptel-acp--on-notification (client notification)
  "Handle a session/update NOTIFICATION for CLIENT."
  (let* ((fsm (map-elt client :gptel-fsm))
         (info (and fsm (gptel-fsm-info fsm)))
         (update (map-nested-elt notification '(params update)))
         (kind (and update (map-elt update 'sessionUpdate))))
    (when (and fsm info (not (plist-get info :gptel-acp-loading))
               (not (plist-get info :gptel-acp-cancelled)))
      (cond
       ((equal kind "agent_message_chunk")
        (let ((text (gptel-acp--content-text (map-elt update 'content))))
          (unless (string-empty-p text)
            (when (plist-get info :gptel-acp-reasoning-open)
              (when (plist-get info :stream)
                (gptel-acp--deliver info '(reasoning . t)))
              (plist-put info :gptel-acp-reasoning-open nil))
            (plist-put info :gptel-acp-text
                       (concat (or (plist-get info :gptel-acp-text) "") text))
            (when (plist-get info :stream)
              (gptel-acp--deliver info text)))))
       ((equal kind "agent_thought_chunk")
        (let ((text (gptel-acp--content-text (map-elt update 'content))))
          (unless (string-empty-p text)
            (plist-put info :gptel-acp-reasoning
                       (concat (or (plist-get info :gptel-acp-reasoning) "") text))
            (plist-put info :gptel-acp-reasoning-open t)
            (when (plist-get info :stream)
              (gptel-acp--deliver info (cons 'reasoning text))))))
       ((member kind '("tool_call" "tool_call_update"))
        (gptel-acp--note-tool fsm update))))))

(defun gptel-acp--note-tool (fsm update)
  "Show a tool_call UPDATE on FSM's status line."
  (let ((info (gptel-fsm-info fsm))
        (title (or (map-elt update 'title)
                   (map-elt update 'toolCallId)
                   "tool")))
    (plist-put info :tool-use (list (list :name (format "%s" title))))
    (when (and (buffer-live-p (plist-get info :buffer))
               (fboundp 'gptel--update-tool-call))
      (with-current-buffer (plist-get info :buffer)
        (when gptel-mode
          (gptel--update-tool-call fsm))))))

;;;; Permission and fs/terminal requests

(defun gptel-acp--permission-options (options)
  "Return (ALLOW-ID . REJECT-ID) from permission OPTIONS."
  (let ((allow nil) (always nil) (first nil) (reject nil))
    (dolist (opt (append options nil))
      (let ((id (map-elt opt 'optionId))
            (kind (map-elt opt 'kind)))
        (unless first (setq first id))
        (cond
         ((equal kind "allow_once") (setq allow id))
         ((equal kind "allow_always") (setq always id))
         ((member kind '("reject_once" "reject_always"))
          (unless reject (setq reject id))))))
    (cons (or allow always first) reject)))

(defun gptel-acp--answer-permission (client request-id allow)
  "Answer CLIENT permission REQUEST-ID.
ALLOW non-nil selects the allow option.  Otherwise select
reject_once, or cancel when the agent offered no reject option."
  (let* ((choice (map-elt client :gptel-permission-choice))
         (allow-id (car choice))
         (reject-id (cdr choice)))
    (acp-send-response
     :client client
     :response
     (cond
      (allow
       (acp-make-session-request-permission-response
        :request-id request-id :option-id allow-id))
      (reject-id
       (acp-make-session-request-permission-response
        :request-id request-id :option-id reject-id))
      (t
       (acp-make-session-request-permission-response
        :request-id request-id :cancelled t))))))

(defun gptel-acp--permission-callback (client fsm request-id)
  "Return the callback that answers REQUEST-ID and resumes FSM."
  (lambda (result)
    (let ((info (gptel-fsm-info fsm)))
      (gptel-acp--answer-permission client request-id (eq result 'allow))
      (plist-put info :gptel-acp-permission nil)
      (unless (memq (gptel-fsm-state fsm) '(ERRS ABRT DONE))
        (gptel--fsm-transition fsm 'ASTR))
      (when (plist-get info :gptel-acp-pending-result)
        (plist-put info :gptel-acp-pending-result nil)
        (gptel-acp--finish fsm)))))

(defun gptel-acp--on-permission (client fsm request)
  "Turn an ACP permission REQUEST into gptel's tool-call callback."
  (let* ((info (gptel-fsm-info fsm))
         (tool-call (map-nested-elt request '(params toolCall)))
         (title (or (map-elt tool-call 'title)
                    (map-elt tool-call 'toolCallId)
                    "acp_permission"))
         (choice (gptel-acp--permission-options
                  (map-nested-elt request '(params options))))
         (tool (gptel--make-tool
                :name (format "%s" title)
                :description "ACP permission request"
                :function gptel-acp--permission-action
                :confirm t))
         (args (list :title title
                     :input (map-elt tool-call 'rawInput)))
         (cb (gptel-acp--permission-callback
              client fsm (map-elt request 'id))))
    (gptel-acp--put client :gptel-permission-choice choice)
    ;; Use the yolo flag captured with the request.  This callback runs
    ;; in the ACP client buffer, which does not have the chat buffer's
    ;; `gptel-confirm-tool-calls'.
    (if (plist-get (plist-get info :data) :acp-yolo)
        (funcall cb 'allow)
      (plist-put info :gptel-acp-permission
                 (list (list tool args cb)))
      (if (eq (gptel-fsm-state fsm) 'WAIT)
          (progn
            (plist-put info :gptel-acp-defer-state nil)
            (gptel--fsm-transition fsm 'APERM))
        (unless (eq (gptel-fsm-state fsm) 'APERM)
          (gptel--fsm-transition fsm 'APERM))))))

(defun gptel-acp--read-file (path line limit)
  "Return text from PATH starting at LINE for LIMIT lines."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (when (and line (> line 1))
      (forward-line (1- line)))
    (let ((start (point)))
      (if limit
          (forward-line limit)
        (goto-char (point-max)))
      (buffer-substring-no-properties start (point)))))

(defun gptel-acp--respond-error (client request code message)
  "Send a JSON-RPC error response for REQUEST."
  (acp-send-response
   :client client
   :response `((:request-id . ,(map-elt request 'id))
               (:error . ,(acp-make-error :code code :message message)))))

(defun gptel-acp--on-request (client request)
  "Handle an agent request, or answer -32601."
  (let* ((method (map-elt request 'method))
         (fsm (map-elt client :gptel-fsm))
         (profile (map-elt client :gptel-profile))
         (params (map-elt request 'params)))
    (cond
     ((equal method "session/request_permission")
      (if fsm
          (gptel-acp--on-permission client fsm request)
        (gptel-acp--respond-error client request -32603
                                  "No gptel request is waiting for permission")))
     ((equal method "fs/read_text_file")
      (condition-case err
          (acp-send-response
           :client client
           :response (acp-make-fs-read-text-file-response
                      :request-id (map-elt request 'id)
                      :content (gptel-acp--read-file
                                (map-elt params 'path)
                                (map-elt params 'line)
                                (map-elt params 'limit))))
        (error (acp-send-response
                :client client
                :response (acp-make-fs-read-text-file-response
                           :request-id (map-elt request 'id)
                           :error (acp-make-error
                                   :code (if (eq (car err) 'file-missing)
                                             -32002
                                           -32603)
                                   :message (error-message-string err)))))))
     ((equal method "fs/write_text_file")
      (if (not (eq profile 'agent))
          (acp-send-response
           :client client
           :response (acp-make-fs-write-text-file-response
                      :request-id (map-elt request 'id)
                      :error (acp-make-error
                              :code -32601
                              :message "Chat profile does not write files")))
        (condition-case err
            (let ((path (expand-file-name (map-elt params 'path))))
              (make-directory (file-name-directory path) t)
              (write-region (or (map-elt params 'content) "") nil path nil 'silent)
              (acp-send-response
               :client client
               :response (acp-make-fs-write-text-file-response
                          :request-id (map-elt request 'id))))
          (error (acp-send-response
                  :client client
                  :response (acp-make-fs-write-text-file-response
                             :request-id (map-elt request 'id)
                             :error (acp-make-error
                                     :code -32603
                                     :message (error-message-string err))))))))
     ((and (stringp method) (string-prefix-p "terminal/" method))
      (if (not (eq profile 'agent))
          (gptel-acp--respond-error
           client request -32601 "Chat profile does not provide a terminal")
        (gptel-acp--on-terminal client request)))
     (t
      (gptel-acp--respond-error
       client request -32601
       (format "Method not found: %s" method))))))

(defun gptel-acp--on-terminal (client request)
  "Handle a terminal/* REQUEST for an agent-profile CLIENT."
  (let* ((method (map-elt request 'method))
         (params (map-elt request 'params))
         (id (map-elt request 'id))
         (tid (map-elt params 'terminalId))
         (table (progn
                  (unless (map-elt client :gptel-terminals)
                    (gptel-acp--put client :gptel-terminals
                                    (make-hash-table :test 'equal)))
                  (map-elt client :gptel-terminals))))
    (cond
     ((equal method "terminal/create")
      (let* ((tid (format "term-%s" id))
             (command (map-elt params 'command))
             (args (append (map-elt params 'args) nil))
             (buf (generate-new-buffer " *gptel-acp-term*"))
             (proc (make-process
                    :name tid
                    :buffer buf
                    :command (cons command args)
                    :connection-type 'pipe
                    :noquery t
                    :file-handler t)))
        (puthash tid (list :process proc :buffer buf :output "") table)
        (acp-send-response
         :client client
         :response `((:request-id . ,id)
                     (:result . ((terminalId . ,tid)))))))
     ((equal method "terminal/output")
      (let* ((rec (gethash tid table))
             (buf (plist-get rec :buffer))
             (limit (or (map-elt params 'outputByteLimit) 65536))
             (output (if (and buf (buffer-live-p buf))
                         (with-current-buffer buf
                           (buffer-substring-no-properties (point-min) (point-max)))
                       ""))
             (truncated (> (length output) limit)))
        (when truncated (setq output (substring output 0 limit)))
        (acp-send-response
         :client client
         :response `((:request-id . ,id)
                     (:result . ((output . ,output)
                                 (truncated . ,(if truncated t :false))))))))
     ((equal method "terminal/wait_for_exit")
      (let* ((rec (gethash tid table))
             (proc (plist-get rec :process)))
        (when (and proc (process-live-p proc))
          (while (process-live-p proc)
            (accept-process-output proc 0.1)))
        (acp-send-response
         :client client
         :response `((:request-id . ,id)
                     (:result . ((exitCode . ,(or (and proc (process-exit-status proc))
                                                  0))))))))
     ((equal method "terminal/kill")
      (when-let* ((rec (gethash tid table))
                  (proc (plist-get rec :process)))
        (when (process-live-p proc) (delete-process proc)))
      (acp-send-response
       :client client
       :response `((:request-id . ,id) (:result . nil))))
     ((equal method "terminal/release")
      (when-let* ((rec (gethash tid table)))
        (when-let* ((proc (plist-get rec :process)))
          (when (process-live-p proc) (delete-process proc)))
        (when-let* ((buf (plist-get rec :buffer)))
          (when (buffer-live-p buf) (kill-buffer buf)))
        (remhash tid table))
      (acp-send-response
       :client client
       :response `((:request-id . ,id) (:result . nil))))
     (t (gptel-acp--respond-error
         client request -32601
         (format "Method not found: %s" method))))))

;;;; Steering

(defun gptel-acp--take-steer (info)
  "Remove queued steer overlays and store their text on INFO.
Return non-nil when there was text to send."
  (let ((text
         (mapconcat
          (lambda (ov)
            (if (and (overlayp ov) (overlay-buffer ov))
                (let ((chunk (string-trim
                              (buffer-substring-no-properties
                               (overlay-start ov) (overlay-end ov)))))
                  (with-current-buffer (overlay-buffer ov)
                    (delete-region (overlay-start ov) (overlay-end ov)))
                  (delete-overlay ov)
                  chunk)
              ""))
          (plist-get info :gptel-acp-steer-overlays)
          "\n")))
    (plist-put info :gptel-acp-steer-overlays nil)
    (setq text (string-trim text))
    (unless (string-empty-p text)
      (plist-put info :gptel-acp-steer-text text)
      text)))

(defun gptel-acp--menu-steer (info msg)
  "Steer the ACP request in INFO with MSG from the gptel menu.
Insert MSG at the response marker and hand that region to
`gptel-acp-steer'.  A blank MSG cancels steering queued on INFO."
  (if (string-blank-p msg)
      (progn
        (dolist (ov (plist-get info :gptel-acp-steer-overlays))
          (when (and (overlayp ov) (overlay-buffer ov))
            (with-current-buffer (overlay-buffer ov)
              (let ((inhibit-read-only t))
                (delete-region (overlay-start ov) (overlay-end ov))))
            (delete-overlay ov)))
        (plist-put info :gptel-acp-steer-overlays nil)
        (message "Steering message canceled"))
    (let* ((tm (or (plist-get info :tracking-marker)
                   (plist-get info :position)))
           (tbuf (and tm (marker-buffer tm))))
      (unless (and tbuf (buffer-live-p tbuf))
        (user-error "No ACP request buffer to steer"))
      (with-current-buffer tbuf
        (save-excursion
          (goto-char tm)
          (let ((beg (point))
                (inhibit-read-only t))
            (insert (string-trim msg))
            (gptel-acp-steer info (cons beg (point)))))))))

(defun gptel-acp-steer (info bounds)
  "Steer the ACP request in INFO with the text in BOUNDS.
Agent sessions that advertise `_session/steering' send it now.
Chat sessions, and agents without that extension, queue the text
as the next user message after the current turn.  BOUNDS is a
cons of buffer positions."
  (let* ((backend (plist-get info :backend))
         (client (and (gptel-acp-p backend) (gptel-acp-client backend)))
         (profile (or (plist-get (plist-get info :data) :profile)
                      (and (gptel-acp-p backend)
                           (gptel-acp--effective-profile backend))))
         (text (string-trim
                (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (sid (or (plist-get info :gptel-acp-session-id)
                  gptel-acp-session-id)))
    (if (and (eq profile 'agent)
             client sid
             (eq (map-elt client :gptel-supports-steering) t))
        (progn
          (gptel-acp--rpc
           client
           `((:method . "_session/steering")
             (:params . ((sessionId . ,sid)
                         (prompt . ,(vector (gptel-acp--text-block text)))
                         (_meta . ((steering
                                    . ((idleBehavior . "promptRequired"))))))))
           #'ignore
           (lambda (err)
             (message "ACP steering failed: %s"
                      (plist-get (gptel-acp--error-plist err) :message))))
          (delete-region (car bounds) (cdr bounds))
          (message "Steering sent"))
      (let ((ov (make-overlay (car bounds) (cdr bounds) (current-buffer) t t)))
        (overlay-put ov 'gptel 'steer)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'face 'warning)
        (overlay-put
         ov 'before-string
         (concat (propertize "QUEUED" 'face '(:inherit shadow :box -1))
                 (propertize ": " 'face 'shadow)))
        (plist-put info :gptel-acp-steer-overlays
                   (cons ov (plist-get info :gptel-acp-steer-overlays)))
        (message "Steering queued for the next ACP turn")))))

;;;; Persistence

(defun gptel-acp--save-session-id ()
  "Save the ACP session id, hashes, and profile as file-local variables."
  (when buffer-file-name
    (when (and (stringp gptel-acp-session-id)
               (not (string-empty-p gptel-acp-session-id)))
      (add-file-local-variable 'gptel-acp-session-id gptel-acp-session-id))
    (when (and (stringp gptel-acp-history-hash)
               (not (string-empty-p gptel-acp-history-hash)))
      (add-file-local-variable 'gptel-acp-history-hash gptel-acp-history-hash))
    (when (and (stringp gptel-acp-system-hash)
               (not (string-empty-p gptel-acp-system-hash)))
      (add-file-local-variable 'gptel-acp-system-hash gptel-acp-system-hash))
    (when (memq gptel-acp-profile '(chat agent))
      (add-file-local-variable 'gptel-acp-profile gptel-acp-profile))))

(defun gptel-acp--install-save-hook ()
  "Save the ACP session id alongside the backend name."
  (add-hook 'gptel-save-state-hook #'gptel-acp--save-session-id))

(defvar gptel-save-state-hook)
(if (boundp 'gptel-save-state-hook)
    (gptel-acp--install-save-hook)
  (with-eval-after-load 'gptel
    (gptel-acp--install-save-hook)))

;;;; Hooks into gptel

(defun gptel-acp--around-realize-query (orig fsm)
  "Show ORIG the chat buffer and allow ACP streaming without curl.
ORIG is `gptel--realize-query'.  FSM is the request state machine."
  (let* ((info (gptel-fsm-info fsm))
         (backend (or (plist-get info :backend) gptel-backend))
         (gptel-acp--request-buffer (plist-get info :buffer))
         (gptel-acp--request-position (plist-get info :position))
         (gptel-acp--request-rewrite
          (eq (plist-get info :callback) 'gptel--rewrite-callback))
         (gptel-use-curl (or gptel-use-curl (gptel-acp-p backend))))
    (funcall orig fsm)))

(defun gptel-acp--around-send-steer (orig)
  "Send steering through ACP, or call ORIG for an HTTP backend."
  (let* ((info (and (boundp 'gptel--fsm-last) gptel--fsm-last
                    (gptel-fsm-info gptel--fsm-last)))
         (backend (or (and info (plist-get info :backend)) gptel-backend)))
    (if (not (gptel-acp-p backend))
        (funcall orig)
      (unless (gptel--fsm-live-p)
        (user-error "No active gptel request in this buffer; nothing to steer"))
      (if-let* (((eq (get-pos-property (point) 'gptel) 'steer))
                (ov (or (cdr (get-char-property-and-overlay (point) 'gptel))
                        (cdr (get-char-property-and-overlay (1- (point)) 'gptel)))))
          (progn
            (plist-put info :gptel-acp-steer-overlays
                       (delq ov (plist-get info :gptel-acp-steer-overlays)))
            (delete-overlay ov)
            (message "Steering message canceled"))
        (let* ((sm (plist-get info :position))
               (tracking-marker (plist-get info :tracking-marker))
               (bounds
                (cond
                 ((use-region-p) (deactivate-mark) (car-safe (region-bounds)))
                 ((and tracking-marker (> (point) tracking-marker))
                  (cons (save-excursion
                          (goto-char tracking-marker)
                          (skip-chars-forward " \t\n")
                          (point))
                        (point)))
                 ((and sm (>= (point) sm)
                       (not (get-text-property (point) 'gptel)))
                  (cons (save-excursion
                          (goto-char
                           (max (previous-single-property-change
                                 (point) 'gptel nil (or sm (point-min)))
                                (previous-single-property-change
                                 (point) 'read-only nil (or sm (point-min)))))
                          (skip-chars-forward " \r\t\n")
                          (point))
                        (point))))))
          (unless (and bounds (> (cdr bounds) (car bounds)))
            (user-error "No steering message at point"))
          (gptel-acp-steer info bounds))))))

(defun gptel-acp--around-update-wait (orig fsm)
  "Keep an ACP Typing status, or call ORIG.
ORIG is `gptel--update-wait'.  ACP can leave WAIT before that
handler returns."
  (let* ((info (gptel-fsm-info fsm))
         (backend (or (plist-get info :backend) gptel-backend)))
    (if (and (gptel-acp-p backend)
             (not (eq (gptel-fsm-state fsm) 'WAIT)))
        (with-current-buffer (plist-get info :buffer)
          (setq gptel--fsm-last fsm))
      (funcall orig fsm))))

(defun gptel-acp--after-reject (&optional tool-calls _ov)
  "Answer an ACP permission prompt when tool calls are rejected.
TOOL-CALLS is the list passed to `gptel--reject-tool-calls'.
Upstream leaves that argument unused, so the callback is invoked
here.  A call with no argument reads `:gptel-acp-permission'."
  (let* ((info (and (boundp 'gptel--fsm-last) gptel--fsm-last
                    (gptel-fsm-info gptel--fsm-last)))
         (calls (or tool-calls
                    (and info (plist-get info :gptel-acp-permission)))))
    (dolist (call calls)
      (when (and (gptel-acp--permission-answered-p (car call))
                 (functionp (nth 2 call)))
        (funcall (nth 2 call) 'reject)))))

(defun gptel-acp--steer-suffix-visible-p ()
  "Show the menu steer suffix for a live ACP request or for tools."
  (and (gptel--fsm-live-p)
       (let ((info (gptel-fsm-info gptel--fsm-last)))
         (or (gptel-acp-p (or (plist-get info :backend) gptel-backend))
             (plist-get info :tools)))))

(defun gptel-acp--around-suffix-steer (orig)
  "Steer an ACP request from the menu, or call ORIG."
  (let* ((info (and (boundp 'gptel--fsm-last) gptel--fsm-last
                    (gptel-fsm-info gptel--fsm-last)))
         (backend (or (and info (plist-get info :backend)) gptel-backend)))
    (if (not (gptel-acp-p backend))
        (funcall orig)
      (when-let* ((msg (read-string "Steering instructions for ongoing query: "))
                  (live (gptel-fsm-info gptel--fsm-last)))
        (gptel-acp--menu-steer live msg)))))

(defvar gptel-acp--menu-installed nil
  "Non-nil after the ACP profile infix has been added to `gptel-menu'.")

(defun gptel-acp--install-menu ()
  "Add the ACP profile infix and point menu steering at ACP."
  (unless gptel-acp--menu-installed
    (transient-define-infix gptel--infix-acp-profile ()
      "Switch the ACP profile between chat and agent.

Chat does not advertise write or terminal access.  Agent does."
      :description "ACP profile"
      :class 'gptel-lisp-variable
      :variable 'gptel-acp-profile
      :format " %k %d %v"
      :set-value #'gptel--set-with-scope
      :display-nil "default"
      :display-map '((nil . "default")
                     (chat . "chat")
                     (agent . "agent"))
      :key "-A"
      :prompt "ACP profile: "
      :reader (lambda (prompt &rest _)
                (let* ((choices '(("chat" . chat)
                                  ("agent" . agent)
                                  ("default" . nil)))
                       (pref (completing-read prompt choices nil t)))
                  (cdr (assoc pref choices)))))
    (transient-append-suffix 'gptel-menu 'gptel--infix-use-tools
      '(gptel--infix-acp-profile
        :if (lambda () (gptel-acp-p gptel-backend))))
    (transient-suffix-put 'gptel-menu 'gptel--suffix-steer
                          :if #'gptel-acp--steer-suffix-visible-p)
    (advice-add 'gptel--suffix-steer :around #'gptel-acp--around-suffix-steer)
    (setq gptel-acp--menu-installed t)))

(defun gptel-acp--around-handle-wait (orig fsm)
  "Send ACP backends through `gptel-backend-send'.
Official gptel only calls curl or `url-retrieve' from
`gptel--handle-wait'.  Other backends keep that path."
  (let* ((info (gptel-fsm-info fsm))
         (backend (or (plist-get info :backend) gptel-backend)))
    (if (not (gptel-acp-p backend))
        (funcall orig fsm)
      (dolist (key '(:tool-result :tool-use :error :http-status :reasoning :tokens))
        (when (plist-get info key)
          (plist-put info key nil)))
      (gptel-backend-send backend fsm)
      (when (buffer-live-p (plist-get info :buffer))
        (with-current-buffer (plist-get info :buffer)
          (run-hooks 'gptel-post-request-hook))))))

(defun gptel-acp--install-hooks ()
  "Advise gptel entry points that have no backend method."
  (advice-add 'gptel--handle-wait :around #'gptel-acp--around-handle-wait)
  (advice-add 'gptel--realize-query :around #'gptel-acp--around-realize-query)
  (advice-add 'gptel-send--steer :around #'gptel-acp--around-send-steer)
  (advice-add 'gptel--update-wait :around #'gptel-acp--around-update-wait)
  (advice-add 'gptel--reject-tool-calls :after #'gptel-acp--after-reject))

(gptel-acp--install-hooks)
(with-eval-after-load 'gptel-transient
  (gptel-acp--install-menu))

(provide 'gptel-acp)
;;; gptel-acp.el ends here
