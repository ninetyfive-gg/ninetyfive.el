;;; ninetyfive.el --- NinetyFive -*- lexical-binding: t; -*-

;; Author: NinetyFive
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (websocket "1.12"))
;; Keywords: convenience, productivity
;; URL: https://github.com/ninetyfive-gg/ninetyfive.el

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

;; This package provides really fast code completions!
;;
;; Usage:
;;   To start the completion service, call `ninetyfive-start' or add
;;   this to your init file:
;;
;;     (add-hook 'emacs-startup-hook #'ninetyfive-start)
;;

;;; Code:

(require 'websocket)
(require 'async)
(require 'json)

(defgroup ninetyfive nil
  "NinetyFive completion."
  :group 'completion
  :prefix "ninetyfive-")

(defcustom ninetyfive-websocket-url "wss://api.ninetyfive.gg"
  "Server URL."
  :type 'string
  :group 'ninetyfive)

(defcustom ninetyfive-debug-messages nil
  "Whether to show debug messages."
  :type 'boolean
  :group 'ninetyfive)

(defvar ninetyfive--reconnect-delay 5
  "Seconds to wait before attempting to reconnect to NinetyFive.")

(defvar ninetyfive--websocket-id 0
  "Monotonically increasing ID to identify the current WebSocket connection.")

(defvar ninetyfive--reconnect-timer nil
  "Timer object used for reconnection attempts.")

(defvar ninetyfive--websocket nil
  "WebSocket connection to NinetyFive API.")

(defvar ninetyfive--connected nil
  "Whether we're connected to the WebSocket.")

(defvar ninetyfive--request-id-counter 0
  "Counter for generating unique request IDs.")

(defvar ninetyfive--current-request-id nil
  "Current active request ID.")

(defvar ninetyfive--completion-text ""
  "Completion accumulated so far.")

(defvar ninetyfive--completion-overlay nil
  "Overlay for displaying completion text.")

(defvar ninetyfive--buffer-last-content ""
  "Last known content of the buffer for delta calculation.")

(defvar ninetyfive--buffer-content-sent nil
  "Whether initial file content has been sent for current buffer.")

(defvar ninetyfive--buffer-local-vars
  '(ninetyfive--buffer-last-content
    ninetyfive--buffer-content-sent)
  "List of buffer-local variables.")

;; This is tracked per buffer
(dolist (var ninetyfive--buffer-local-vars)
  (make-variable-buffer-local var))

(defun ninetyfive--generate-request-id ()
  "Generate a unique request ID."
  (setq ninetyfive--request-id-counter (1+ ninetyfive--request-id-counter))
  (format "%s%d" (format-time-string "%s") ninetyfive--request-id-counter))

(defun ninetyfive--get-file-path ()
  "Get the current file path or \='Untitled-1\=' if buffer is not visiting a file."
  (or (buffer-file-name) "Untitled-1"))

(defun ninetyfive--get-buffer-content ()
  "Get the current buffer content."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ninetyfive--get-text-to-cursor ()
  "Get text from beginning of buffer to current cursor position."
  (buffer-substring-no-properties (point-min) (point)))

(defun ninetyfive--debug-message (format-string &rest args)
  "Display debug message if debug messages are enabled."
  (when ninetyfive-debug-messages
    (apply #'message (concat "NinetyFive: " format-string) args)))

(defun ninetyfive--send-message (message)
  "Send MESSAGE to the WebSocket server."
  (when (and ninetyfive--websocket ninetyfive--connected)
    (let ((json-string (json-encode message)))
      (websocket-send-text ninetyfive--websocket json-string))))


(defun ninetyfive--calculate-and-send-delta ()
  "Calculate delta between current buffer content and last known content, then send it."
  (when ninetyfive--connected
    (let* ((current-content (ninetyfive--get-buffer-content))
           (last-content ninetyfive--buffer-last-content)
           (current-bytes (string-to-multibyte current-content))
           (last-bytes (string-to-multibyte last-content)))
      
      (if (not ninetyfive--buffer-content-sent)
          ;; First time - send content and finish, see https://www.gnu.org/software/emacs/manual/html_node/eintr/progn.html
          (progn
            (ninetyfive--send-file-content)
            (setq ninetyfive--buffer-content-sent t)
            (setq ninetyfive--buffer-last-content current-content))
        
        ;; Calculate delta
        (let ((delta-info (ninetyfive--find-delta last-bytes current-bytes)))
          (when delta-info
            (let ((start (plist-get delta-info :start))
                  (end (plist-get delta-info :end))
                  (text (plist-get delta-info :text)))
              
              (ninetyfive--debug-message "Sending file delta - start: %d, end: %d, text length: %d"
                                         start end (length text))
              
              ;; Send delta message
              (let ((message `((type . "file-delta")
                               (start . ,start)
                               (end . ,end)
                               (text . ,text))))
                (ninetyfive--send-message message))
              
              ;; Update last known content
              (setq ninetyfive--buffer-last-content current-content))))))))

(defun ninetyfive--find-delta (old-text new-text)
  "Find the delta between OLD-TEXT and NEW-TEXT.
Returns a plist with :start, :end, and :text, or nil if no change."
  (let ((old-len (length old-text))
        (new-len (length new-text)))
    
    (if (string= old-text new-text)
        nil ; No change
      
      ;; Find common prefix
      (let ((start 0))
        (while (and (< start old-len)
                    (< start new-len)
                    (= (aref old-text start) (aref new-text start)))
          (setq start (1+ start)))
        
        ;; Find common suffix
        (let ((old-end old-len)
              (new-end new-len))
          (while (and (> old-end start)
                      (> new-end start)
                      (= (aref old-text (1- old-end)) (aref new-text (1- new-end))))
            (setq old-end (1- old-end))
            (setq new-end (1- new-end)))
          
          ;; Extract the changed text
          (let ((changed-text (substring new-text start new-end)))
            (list :start start
                  :end old-end
                  :text changed-text)))))))

(defun ninetyfive--send-set-workspace ()
  "Send set-workspace message to the server."
  (let ((message `((type . "set-workspace"))))
    (ninetyfive--send-message message)))

(defun ninetyfive--send-file-content ()
  "Send file content message to the server."
  (let ((message `((type . "file-content")
                   (path . ,(ninetyfive--get-file-path))
                   (text . ,(ninetyfive--get-buffer-content)))))
    (ninetyfive--send-message message)))

(defun ninetyfive--send-delta-completion-request ()
  "Send delta completion request to the server."
  (let* ((text-to-cursor (ninetyfive--get-text-to-cursor))
         (byte-length (string-bytes text-to-cursor))
         (request-id (ninetyfive--generate-request-id))
         (completion-message `((type . "delta-completion-request")
                               (requestId . ,request-id)
                               (repo . "unknown")
                               (pos . ,byte-length))))
    ;; Clear previous completion and set new request ID
    (ninetyfive--clear-completion)
    (setq ninetyfive--current-request-id request-id)
    (setq ninetyfive--completion-text "")
    
    ;; Send delta or full content as needed
    (ninetyfive--calculate-and-send-delta)
    
    (ninetyfive--debug-message "Sending completion request - ID: %s, pos: %d" request-id byte-length)
    (ninetyfive--send-message completion-message)))

(defun ninetyfive--on-websocket-open (_websocket id)
  (when (eq id ninetyfive--websocket-id)
    (message "[ninetyfive] WebSocket opened (id %s)" id)
    (when ninetyfive--reconnect-timer
      (cancel-timer ninetyfive--reconnect-timer)
      (setq ninetyfive--reconnect-timer nil))
    (setq ninetyfive--connected t)
    (message "[ninetyfive] Connected flag set to t.")
    (ninetyfive--send-set-workspace)
    (when (and ninetyfive-mode (buffer-file-name))
      (ninetyfive--on-file-opened))))

(defun ninetyfive--clear-completion ()
  "Clear the current completion overlay."
  (when ninetyfive--completion-overlay
    (delete-overlay ninetyfive--completion-overlay)
    (setq ninetyfive--completion-overlay nil))
  ;; Remove the keybind on tab
  (when ninetyfive-mode
    (define-key ninetyfive-mode-map (kbd "TAB") nil)
    (define-key ninetyfive-mode-map (kbd "<tab>") nil)))

(defun ninetyfive--show-completion (text)
  "Show completion TEXT as an overlay at current point."
  (ninetyfive--clear-completion)
  (when (and text (> (length text) 0))
    (setq ninetyfive--completion-overlay (make-overlay (point) (point)))
    (overlay-put ninetyfive--completion-overlay 'after-string
                 (propertize text 'face '(:foreground "gray" :slant italic)))
    (overlay-put ninetyfive--completion-overlay 'ninetyfive-completion t)
    ;; Add the keybind when the user can accept something
    (when ninetyfive-mode
      (define-key ninetyfive-mode-map (kbd "TAB") #'ninetyfive-accept-completion)
      (define-key ninetyfive-mode-map (kbd "<tab>") #'ninetyfive-accept-completion))))

(defun ninetyfive--handle-completion-response (message)
  "Handle completion response, we dont care about other MESSAGEs right now."
  (let ((request-id (cdr (assq 'r message)))
        (completion-value (cdr (assq 'v message))))
    (if (and request-id
             completion-value
             (string= request-id ninetyfive--current-request-id))
        (progn
          (ninetyfive--debug-message "Received completion response - ID: %s, value: %S, total length: %d"
                                     request-id
                                     completion-value
                                     (+ (length ninetyfive--completion-text) (length completion-value)))
          ;; Accumulate completion text
          (setq ninetyfive--completion-text
                (concat ninetyfive--completion-text completion-value))
          ;; Show updated completion
          (ninetyfive--show-completion ninetyfive--completion-text))
      ;; TODO: We should send a cancel message here
      (when request-id
        (ninetyfive--debug-message "Ignoring completion response - ID: %s (current: %s)"
                                   request-id ninetyfive--current-request-id)))))

(defun ninetyfive--on-websocket-message (_websocket frame)
  "Handle WEBSOCKET message received.
Argument FRAME: payload"
  (let* ((payload (websocket-frame-payload frame))
         (message (json-read-from-string payload)))
    ;; Check if this is a completion response
    (if (assq 'r message)
        (ninetyfive--handle-completion-response message)
      ;; Handle other message types if needed
      (ninetyfive--debug-message "Received message: %s" payload))))

(defun ninetyfive--schedule-reconnect ()
  "Schedule a safe, non-blocking reconnection attempt."
  (if ninetyfive--connected
      (message "[ninetyfive] Skipping reconnect — already connected.")
    (if ninetyfive--reconnect-timer
        (message "[ninetyfive] Reconnect already scheduled.")
      (message "[ninetyfive] Scheduling reconnect in %s seconds..." ninetyfive--reconnect-delay)
      (setq ninetyfive--reconnect-timer
            (run-at-time ninetyfive--reconnect-delay nil
                         (lambda ()
                           (setq ninetyfive--reconnect-timer nil)
                           (ninetyfive--connect)))))))

(defun ninetyfive--attempt-reconnect ()
  "Try to reconnect to the WebSocket server, non-blocking."
  (setq ninetyfive--reconnect-timer nil)

  (let ((url (url-generic-parse-url ninetyfive-websocket-url)))
    (condition-case err
        (progn
          ;; avoids blocking
          (let ((probe (make-network-process
                        :name "ninetyfive-probe"
                        :host (url-host url)
                        :service (url-port url)
                        :noquery t
                        :nowait t)))
            (delete-process probe)
            (ninetyfive--connect)))
      (error
       (message "[ninetyfive] Connection failed: %s — retrying..." (error-message-string err))
       (ninetyfive--schedule-reconnect)))))

(defun ninetyfive--on-websocket-close (_websocket id)
  (when (eq id ninetyfive--websocket-id)
    (message "[ninetyfive] WebSocket closed (id %s)" id)
    (setq ninetyfive--connected nil)
    (setq ninetyfive--websocket nil)
    (ninetyfive--schedule-reconnect)))

(defun ninetyfive--on-websocket-error (_websocket _type err id)
  (when (eq id ninetyfive--websocket-id)
    (message "[ninetyfive] WebSocket error (id %s): %s" id err)
    (setq ninetyfive--connected nil)))

(defun ninetyfive--connect ()
  "Asynchronously probe and connect to the NinetyFive WebSocket server."
  (message "[ninetyfive] >>> connect called")

  ;; exit if we're already connected
  (when ninetyfive--connected
    (cl-return-from ninetyfive--connect nil))

  (when ninetyfive--websocket
    (message "[ninetyfive] Closing existing WebSocket...")
    (ignore-errors (websocket-close ninetyfive--websocket))
    (setq ninetyfive--websocket nil))

  ;; we need to set a websocket id during reconnect to ensure we don't hit a race condition
  ;; when we try to reconnect.
  (setq ninetyfive--websocket-id (1+ ninetyfive--websocket-id))
  (let ((this-id ninetyfive--websocket-id))

    (let* ((parsed-url (url-generic-parse-url ninetyfive-websocket-url))
           (host (url-host parsed-url))
           (raw-port (url-port parsed-url))
           (port (if (or (null raw-port) (= raw-port 0))
                     (if (string= (url-type parsed-url) "wss") 443 80)
                   raw-port)))

      (async-start
       `(lambda ()
          (let ((host ,host)
                (port ,port))
            (message "[ninetyfive] (child) Probing %s:%s..." host port)
            (condition-case err
                (let ((sock (make-network-process
                             :name "ninetyfive-probe"
                             :host host
                             :service port
                             :noquery t
                             :nowait nil)))
                  (delete-process sock)
                  (message "[ninetyfive] (child) Probe succeeded.")
                  'success)
              (error
               (message "[ninetyfive] (child) Probe failed: %S" err)
               (list 'error err)))))

       (lambda (result)
         (message "[ninetyfive] >>> async callback fired with result: %S" result)
         (pcase result
           (`success
            (if ninetyfive--connected
                (message "[ninetyfive] Probe succeeded, but already connected.")
              (setq ninetyfive--websocket
                    (websocket-open ninetyfive-websocket-url
                                    :on-open (lambda (ws)
                                               (ninetyfive--on-websocket-open ws this-id))
                                    :on-message #'ninetyfive--on-websocket-message
                                    :on-close (lambda (ws)
                                                (ninetyfive--on-websocket-close ws this-id))
                                    :on-error (lambda (ws type err)
                                                (ninetyfive--on-websocket-error ws type err this-id))))))
           
           (`(error ,err)
            (unless ninetyfive--connected
              (message "[ninetyfive] Failed to connect (from async): %s" (error-message-string err))
              (ninetyfive--schedule-reconnect)))))))))

(defun ninetyfive--disconnect ()
  "Disconnect from the NinetyFive WebSocket server."
  (when ninetyfive--websocket
    (websocket-close ninetyfive--websocket)
    (setq ninetyfive--websocket nil)
    (setq ninetyfive--connected nil)))

(defun ninetyfive--on-file-opened ()
  "Handle file opened event."
  (when ninetyfive--connected
    (setq ninetyfive--buffer-content-sent nil)
    (setq ninetyfive--buffer-last-content "")
    (ninetyfive--calculate-and-send-delta)))

;; Hook functions
(defun ninetyfive--find-file-hook ()
  "Hook function for when a file is opened."
  (ninetyfive--on-file-opened))

(defun ninetyfive--buffer-switch-hook ()
  "Function to trigger when switching buffers, since we need to update the workspace."
  (when ninetyfive--connected
    (ninetyfive--send-set-workspace)
    (ninetyfive--on-file-opened)))

(defun ninetyfive--accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (when (and ninetyfive--completion-overlay ninetyfive--completion-text)
    (insert ninetyfive--completion-text)
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)))

(defvar ninetyfive--last-command nil
  "Command to use when checking if completion should trigger.")

(defun ninetyfive--should-clear-completion ()
  "Check if completion should be cleared based on current command."
  (or (memq ninetyfive--last-command '(forward-char backward-char
                           next-line previous-line
                           beginning-of-line end-of-line
                           forward-word backward-word
                           scroll-up-command scroll-down-command
                           mouse-set-point))
      (and (not (eq ninetyfive--last-command 'self-insert-command))
           (not (eq ninetyfive--last-command 'ninetyfive--accept-completion)))))

(defun ninetyfive--should-trigger-completion ()
  "Check if we should request a completion based on current command."
  ;; we need to store last-command since the debounce 'swallows' the command
  (memq ninetyfive--last-command '(self-insert-command
                       delete-char
                       delete-backward-char
                       delete-forward-char
                       backward-delete-char-untabify
                       newline
                       newline-and-indent)))

(defvar ninetyfive--completion-timer nil
  "Timer for debouncing delta completion requests.")

;; small debounce to avoid cpu overload during completion reset
(defconst ninetyfive--completion-delay 0.1
  "Delay in seconds before triggering a completion request.")

(defun ninetyfive--debounced-completion-request ()
  "Trigger the delta completion request if still valid."
  (setq ninetyfive--completion-timer nil)

  (when (and ninetyfive--connected
             (ninetyfive--should-trigger-completion))
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)
    (ninetyfive--send-delta-completion-request)))

(defun ninetyfive--post-command-hook ()
  "Efficient hook function for triggering completions."
  (setq ninetyfive--last-command this-command)
  (cond
   ((and ninetyfive--connected
         (ninetyfive--should-trigger-completion))
    ;; wipe pending timer if exists
    (when (timerp ninetyfive--completion-timer)
      (cancel-timer ninetyfive--completion-timer))

    (setq ninetyfive--completion-timer
      (run-at-time ninetyfive--completion-delay nil #'ninetyfive--debounced-completion-request)))

   ((and ninetyfive--completion-overlay (ninetyfive--should-clear-completion))
    (when (timerp ninetyfive--completion-timer)
      (cancel-timer ninetyfive--completion-timer)
      (setq ninetyfive--completion-timer nil))
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil))))


;;;###autoload
(defun ninetyfive-accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (ninetyfive--accept-completion))

;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html
;;;###autoload
(define-minor-mode ninetyfive-mode
  "Minor mode for NinetyFive."
  :lighter " NF"
  :global nil
  :keymap (make-sparse-keymap)
  (if ninetyfive-mode
      (progn
        ;; Enable mode
        (add-hook 'find-file-hook #'ninetyfive--find-file-hook nil t)
        (add-hook 'post-command-hook #'ninetyfive--post-command-hook nil t)

        ;; Initialize buffer state
        (setq ninetyfive--buffer-content-sent nil)
        (setq ninetyfive--buffer-last-content "")

        ;; Only send file content if we're already connected (don't try to connect here)
        (when (and ninetyfive--connected (buffer-file-name))
          (ninetyfive--on-file-opened)))

    ;; Disable mode
    (remove-hook 'find-file-hook #'ninetyfive--find-file-hook t)
    (remove-hook 'post-command-hook #'ninetyfive--post-command-hook t)
    
    ;; Clear any active completion
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)
    
    (setq ninetyfive--buffer-content-sent nil)
    (setq ninetyfive--buffer-last-content "")))

(defun ninetyfive-turn-on-unless-buffer-read-only ()
  "Turn on `ninetyfive-mode' if the buffer is writable."
  (unless (or buffer-read-only
              (minibufferp)) ;; This avoids showing suggestions when doing M-x...
    (ninetyfive-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ninetyfive-mode
  ninetyfive-mode ninetyfive-turn-on-unless-buffer-read-only
  :group 'ninetyfive)

;; Add global hook for buffer switching
(defun ninetyfive--setup-global-hooks ()
  "Setup global hooks for NinetyFive."
  (add-hook 'buffer-list-update-hook #'ninetyfive--buffer-switch-hook))

(defun ninetyfive--remove-global-hooks ()
  "Remove global hooks for NinetyFive."
  (remove-hook 'buffer-list-update-hook #'ninetyfive--buffer-switch-hook))

;;;###autoload
(defun ninetyfive-start ()
  "Start NinetyFive."
  (interactive)
  (ninetyfive--connect)
  (ninetyfive--setup-global-hooks)
  (global-ninetyfive-mode 1)
  (message "NinetyFive started"))

;;;###autoload
(defun ninetyfive-stop ()
  "Stop NinetyFive."
  (interactive)
  (global-ninetyfive-mode -1)
  (ninetyfive--remove-global-hooks)
  (ninetyfive--disconnect)
  (message "NinetyFive stopped"))

;;;###autoload
(defun ninetyfive-toggle-debug-messages ()
  "Toggle debug messages on/off."
  (interactive)
  (setq ninetyfive-debug-messages (not ninetyfive-debug-messages))
  (message "NinetyFive debug messages %s" 
           (if ninetyfive-debug-messages "enabled" "disabled")))

(provide 'ninetyfive)

;;; ninetyfive.el ends here
