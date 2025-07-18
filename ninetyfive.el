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
(require 'json)

(defgroup ninetyfive nil
  "NinetyFive completion."
  :group 'completion
  :prefix "ninetyfive-")

(defcustom ninetyfive-websocket-url "wss://api.ninetyfive.gg"
  "Server URL."
  :type 'string
  :group 'ninetyfive)

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

(defun ninetyfive--send-message (message)
  "Send MESSAGE to the WebSocket server."
  (when (and ninetyfive--websocket ninetyfive--connected)
    (let ((json-string (json-encode message)))
      (websocket-send-text ninetyfive--websocket json-string))))

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
    
    (message "NinetyFive: Sending updated file content before completion request")
    (ninetyfive--send-file-content)
    
    (message "NinetyFive: Sending completion request - ID: %s, pos: %d" request-id byte-length)
    (ninetyfive--send-message completion-message)))

(defun ninetyfive--on-websocket-open (_websocket)
  "Handle websocket connection opened."
  (setq ninetyfive--connected t)
  (message "NinetyFive: Connected to WebSocket server"))

(defun ninetyfive--clear-completion ()
  "Clear the current completion overlay."
  (when ninetyfive--completion-overlay
    (delete-overlay ninetyfive--completion-overlay)
    (setq ninetyfive--completion-overlay nil)))

(defun ninetyfive--show-completion (text)
  "Show completion TEXT as an overlay at current point."
  (ninetyfive--clear-completion)
  (when (and text (> (length text) 0))
    (setq ninetyfive--completion-overlay (make-overlay (point) (point)))
    (overlay-put ninetyfive--completion-overlay 'after-string
                 (propertize text 'face '(:foreground "gray" :slant italic)))
    (overlay-put ninetyfive--completion-overlay 'ninetyfive-completion t)))

(defun ninetyfive--handle-completion-response (message)
  "Handle completion response, we dont care about other MESSAGEs right now."
  (let ((request-id (cdr (assq 'r message)))
        (completion-value (cdr (assq 'v message))))
    (if (and request-id
             completion-value
             (string= request-id ninetyfive--current-request-id))
        (progn
          (message "NinetyFive: Received completion response - ID: %s, value: %S, total length: %d"
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
        (message "NinetyFive: Ignoring completion response - ID: %s (current: %s)"
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
      (message "NinetyFive: Received message: %s" payload))))

(defun ninetyfive--on-websocket-close (_websocket)
  "Handle WEBSOCKET connection closed."
  (setq ninetyfive--connected nil)
  (setq ninetyfive--websocket nil)
  (message "NinetyFive: WebSocket connection closed"))

(defun ninetyfive--on-websocket-error (_websocket _type err)
  "Handle WEBSOCKET error.
Argument TYPE error type from websocket connection.
Argument ERR error."
  (setq ninetyfive--connected nil)
  (message "NinetyFive: WebSocket error: %s" err))

(defun ninetyfive--connect ()
  "Connect to the NinetyFive WebSocket server."
  (when ninetyfive--websocket
    (websocket-close ninetyfive--websocket))
  
  (setq ninetyfive--websocket
        (websocket-open ninetyfive-websocket-url
                        :on-open #'ninetyfive--on-websocket-open
                        :on-message #'ninetyfive--on-websocket-message
                        :on-close #'ninetyfive--on-websocket-close
                        :on-error #'ninetyfive--on-websocket-error)))

(defun ninetyfive--disconnect ()
  "Disconnect from the NinetyFive WebSocket server."
  (when ninetyfive--websocket
    (websocket-close ninetyfive--websocket)
    (setq ninetyfive--websocket nil)
    (setq ninetyfive--connected nil)))

(defun ninetyfive--on-file-opened ()
  "Handle file opened event."
  (when ninetyfive--connected
    (ninetyfive--send-file-content)))

;; Hook functions
(defun ninetyfive--find-file-hook ()
  "Hook function for when a file is opened."
  (ninetyfive--on-file-opened))

(defun ninetyfive--accept-completion ()
  "Accept the current completion suggestion."
  (interactive)
  (when (and ninetyfive--completion-overlay ninetyfive--completion-text)
    (insert ninetyfive--completion-text)
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)))

(defun ninetyfive--should-clear-completion ()
  "Check if completion should be cleared based on current command."
  (or (memq this-command '(forward-char backward-char
                           next-line previous-line
                           beginning-of-line end-of-line
                           forward-word backward-word
                           scroll-up-command scroll-down-command
                           mouse-set-point))
      (and (not (eq this-command 'self-insert-command))
           (not (eq this-command 'ninetyfive--accept-completion)))))

(defun ninetyfive--should-trigger-completion ()
  "Check if we should request a completion based on current command."
  (memq this-command '(self-insert-command
                       delete-char
                       delete-backward-char
                       delete-forward-char
                       backward-delete-char-untabify
                       newline
                       newline-and-indent)))

(defun ninetyfive--post-command-hook ()
  "Hook function for post-command events."
  (cond
   ;; Send completion request if user is typing, deleting, or doing newlines
   ((and ninetyfive--connected
         (ninetyfive--should-trigger-completion))
    ;; Clear any existing completion first, then request new one
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)
    (ninetyfive--send-delta-completion-request))
   ;; Clear completion if cursor moved or other non-typing command
   ((and ninetyfive--completion-overlay (ninetyfive--should-clear-completion))
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
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") #'ninetyfive-accept-completion)
            (define-key map (kbd "<tab>") #'ninetyfive-accept-completion)
            map)
  (if ninetyfive-mode
      (progn
        ;; Enable mode
        (add-hook 'find-file-hook #'ninetyfive--find-file-hook nil t)
        (add-hook 'post-command-hook #'ninetyfive--post-command-hook nil t)
        ;; Only send file content if we're already connected (don't try to connect here)
        (when (and ninetyfive--connected (buffer-file-name))
          (ninetyfive--on-file-opened)))
    ;; Disable mode
    (remove-hook 'find-file-hook #'ninetyfive--find-file-hook t)
    (remove-hook 'post-command-hook #'ninetyfive--post-command-hook t)
    ;; Clear any active completion
    (ninetyfive--clear-completion)
    (setq ninetyfive--completion-text "")
    (setq ninetyfive--current-request-id nil)))

(defun ninetyfive-turn-on-unless-buffer-read-only ()
  "Turn on `ninetyfive-mode' if the buffer is writable."
  (unless buffer-read-only
    (ninetyfive-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ninetyfive-mode
  ninetyfive-mode ninetyfive-turn-on-unless-buffer-read-only
  :group 'ninetyfive)

;;;###autoload
(defun ninetyfive-start ()
  "Start NinetyFive."
  (interactive)
  (ninetyfive--connect)
  (global-ninetyfive-mode 1)
  (message "NinetyFive started"))

;;;###autoload
(defun ninetyfive-stop ()
  "Stop NinetyFive."
  (interactive)
  (global-ninetyfive-mode -1)
  (ninetyfive--disconnect)
  (message "NinetyFive stopped"))

(provide 'ninetyfive)

;;; ninetyfive.el ends here
