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

(defvar ninetyfive--last-buffer nil
  "The most recent buffer where the user made an edit.
  Used to safely access buffer content from asynchronous contexts,
  may not point to the actual user-facing editing buffer.")

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

(defvar ninetyfive--last-point nil
  "Stores last known point to detect cursor movement.")

(defvar ninetyfive--completion-overlay nil
  "Overlay for displaying completion text.")

(defvar ninetyfive--buffer-content-sent nil
  "Whether initial file content has been sent for current buffer.")

(defvar ninetyfive--buffer-local-vars
  '(ninetyfive--buffer-content-sent)
  "List of buffer-local variables.")

(defvar ninetyfive--inhibit-after-change nil
  "If non-nil, suppresses behavior inside `ninetyfive--after-change-hook`.")

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
  "Get the content of the last known user-facing buffer, or an empty string if unset."
  (if (and ninetyfive--last-buffer
           (buffer-live-p ninetyfive--last-buffer))
      (with-current-buffer ninetyfive--last-buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    "")) ;; return empty

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

(defun ninetyfive--send-delta-from-change (start end previous-length text)
  "Send delta message using change information from after-change-functions.
START and END are buffer positions, PREVIOUS_LENGTH is the lenght of the original text, TEXT is the replacement text."
  (when ninetyfive--connected
    (if (not ninetyfive--buffer-content-sent)
        ;; First time - send full content
        (progn
          (ninetyfive--send-file-content)
          (setq ninetyfive--buffer-content-sent t))
      
      (let* ((byte-start (string-bytes (buffer-substring-no-properties (point-min) start)))
             (byte-end (+ byte-start previous-length)))
        
        (ninetyfive--debug-message "Sending file delta - start: %d, end: %d, text length: %d"
                                   byte-start byte-end (length text))
        
        ;; Send delta message
        (let ((message `((type . "file-delta")
                         (start . ,byte-start)
                         (end . ,byte-end)
                         (text . ,text))))
          (ninetyfive--send-message message))))))

(defun ninetyfive--calculate-and-send-delta ()
  "Send file content for buffers that haven't been sent yet."
  (when (and ninetyfive--connected (not ninetyfive--buffer-content-sent))
    (ninetyfive--send-file-content)
    (setq ninetyfive--buffer-content-sent t)))

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
    ;; Set new request ID
    (setq ninetyfive--current-request-id request-id)
    
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
    (let ((completion-overlay-text (propertize text 'face '(:foreground "gray" :slant italic))))
      ;; Place cursor before suggestion
      (put-text-property 0 1 'cursor t completion-overlay-text)
      (overlay-put ninetyfive--completion-overlay 'after-string completion-overlay-text))
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
  "Handle WEBSOCKET message received. Argument FRAME: payload."
  (let* ((payload (websocket-frame-payload frame))
         (lines (split-string payload "\n" t)))  ;; split by newline, omit empty lines
    (dolist (line lines)
      (condition-case err
          (let ((message (json-read-from-string line)))
            (if (assq 'r message)
                (ninetyfive--handle-completion-response message)
              (ninetyfive--debug-message "[95] Received non-completion message: %s" line)))
        (error
         (ninetyfive--debug-message "[95] Failed to parse JSON line: %s\nError: %s" line err))))))

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

(defun ninetyfive--after-change-hook (start end old-length)
  "Hook function for after-change-functions to send deltas and request completions.
START and END are the beginning and end of region just changed."
  (when ninetyfive--connected
    (let ((text (buffer-substring-no-properties start end)))
      (ninetyfive--send-delta-from-change start end old-length text)
      (setq ninetyfive--last-buffer (current-buffer))

      (ninetyfive--clear-completion)

      ;; only clear completion if the inhibit change is set, this is crucial. See ninetyfive--accept-completion
      (unless ninetyfive--inhibit-after-change
        (setq ninetyfive--completion-text ""))

      (setq ninetyfive--current-request-id nil)
      (ninetyfive--send-delta-completion-request))))

(defun ninetyfive--accept-completion ()
  "Accept the current completion suggestion, replacing the rest of the current line."
  (interactive)
  (when (and ninetyfive--completion-overlay ninetyfive--completion-text)
    ;; trigger the var flip when accepting so that it doesn't trigger the completion reset
    ;; during the after-change hook
    (let ((ninetyfive--inhibit-after-change t))
      (let ((start (point))
            (end (line-end-position)))
        (delete-region start end)
        (insert ninetyfive--completion-text)))

    ;; Clear overlay and reset state
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)))

(defun ninetyfive--maybe-set-last-buffer ()
  "Update `ninetyfive--last-buffer` if the selected buffer is a user-facing file buffer."
  (let ((buf (current-buffer)))
    (when (and
           (not (window-minibuffer-p)) 
           (not (string-match-p "^ \\*Echo Area" (buffer-name)))
           (not (string-prefix-p "*" (buffer-name)))
           (buffer-file-name buf) ;; only real files...
           (get-buffer-window buf 'visible)) ;; and must be visible...
      (unless (eq buf ninetyfive--last-buffer)
        (setq ninetyfive--last-buffer buf)))))

(defun ninetyfive--maybe-clear-on-cursor-move ()
  "Clear completion if the cursor has moved."
  (when (and ninetyfive--completion-overlay
             (/= (point) ninetyfive--last-point))
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil))
  (setq ninetyfive--last-point (point)))

;;;###autoload
(defun ninetyfive-accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (ninetyfive--accept-completion))

;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html
;;;###autoload
(define-minor-mode ninetyfive-mode
  "Minor mode for NinetyFive."
  :lighter " NinetyFive"
  :global nil
  :keymap (make-sparse-keymap)
  (if ninetyfive-mode
      (progn
        (add-hook 'find-file-hook #'ninetyfive--find-file-hook nil t)
        (add-hook 'after-change-functions #'ninetyfive--after-change-hook nil t)
        (add-hook 'buffer-list-update-hook #'ninetyfive--maybe-set-last-buffer nil t)
        (add-hook 'post-command-hook #'ninetyfive--maybe-clear-on-cursor-move nil t)

        (setq ninetyfive--last-buffer (current-buffer))

        ;; reset state
        (setq ninetyfive--buffer-content-sent nil)
        (setq ninetyfive--completion-text "")
        (setq ninetyfive--current-request-id nil)
        (setq ninetyfive--last-point (point))

        (when (and ninetyfive--connected (buffer-file-name))
          (ninetyfive--on-file-opened)))

    (remove-hook 'find-file-hook #'ninetyfive--find-file-hook t)
    (remove-hook 'after-change-functions #'ninetyfive--after-change-hook t)
    (remove-hook 'buffer-list-update-hook #'ninetyfive--maybe-set-last-buffer nil t)
    (remove-hook 'post-command-hook #'ninetyfive--maybe-clear-on-cursor-move nil t)

    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)
    (setq ninetyfive--buffer-content-sent nil)))

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
