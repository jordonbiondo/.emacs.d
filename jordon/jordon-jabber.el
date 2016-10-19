;;; jordon-jabber.el --- Function overrides for jabber display.
;;
;; Filename: jordon-jabber.el
;; Description: Function overrides for jabber display.
;; Author: Jordon Biondo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Function overrides for jabber display.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; (defun jabber-display-roster ()
;;   "switch to the main jabber buffer and refresh the roster display to reflect the current information"
;;   (interactive)
;;   (with-current-buffer (get-buffer-create jabber-roster-buffer)
;;     (if (not (eq major-mode 'jabber-roster-mode))
;;         (jabber-roster-mode))
;;     (setq buffer-read-only nil)
;;     ;; line-number-at-pos is in Emacs >= 21.4.  Only used to avoid
;;     ;; excessive scrolling when updating roster, so not absolutely
;;     ;; necessary.
;;     (let ((current-line (and (fboundp 'line-number-at-pos) (line-number-at-pos)))
;;           (current-column (current-column)))
;;       (erase-buffer)
;;       (setq jabber-roster-ewoc nil)
;;       (when jabber-roster-show-title
;;         (insert (jabber-propertize "chat" 'face `(:inherit jabber-title-large :inherit font-lock-preprocessor-face)) "\n"))
;;       (when jabber-roster-show-bindings
;;         (insert "RET      Open chat buffer        C-k      Delete roster item
;; e        Edit item               s        Send subscription request
;; q        Bury buffer             i        Get disco items
;; I        Get disco info          b        Browse
;; j        Join groupchat (MUC)    v        Get client version
;; a        Send presence           o        Show offline contacts on/off
;; C-c C-c  Chat menu               C-c C-m  Multi-User Chat menu
;; C-c C-i  Info menu               C-c C-r  Roster menu
;; C-c C-s  Service menu

;; H        Toggle displaying this text
;; "))
;;       (insert "__________________________________\n\n")
;;       (if (null jabber-connections)
;;           (insert "Not connected\n")
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map [mouse-2] #'jabber-send-presence)
;;           (insert (jabber-propertize (concat (format " - %s"
;;                                                      (cdr (assoc *jabber-current-show* jabber-presence-strings)))
;;                                              (if (not (zerop (length *jabber-current-status*)))
;;                                                  (format " (%s)"
;;                                                          (jabber-fix-status *jabber-current-status*)))
;;                                              " -")
;;                                      'face (or (cdr (assoc *jabber-current-show* jabber-presence-faces))
;;                                                'jabber-roster-user-online)
;;                                      ;;'mouse-face (cons 'background-color "light grey")
;;                                      'keymap map)
;;                   "\n")))

;;       (dolist (jc jabber-connections)
;;         ;; use a hash-based roster
;;         (when (not (plist-get (fsm-get-state-data jc) :roster-hash))
;;           (jabber-roster-prepare-roster jc))
;;         ;; We sort everything before putting it in the ewoc
;;         (jabber-sort-roster jc)
;;         (let ((before-ewoc (point))
;;               (ewoc (ewoc-create
;;                      (lexical-let ((jc jc))
;;                        (lambda (data)
;;                          (let* ((group (car data))
;;                                 (group-name (car group))
;;                                 (buddy (car (cdr data))))
;;                            (jabber-display-roster-entry jc group-name buddy))))
;;                      (concat
;;                       (jabber-propertize (concat
;;                                           (plist-get (fsm-get-state-data jc) :username))
;;                                          'face 'jabber-title-medium)
;;                       "\n__________________________________\n")
;;                      "__________________________________"))
;;               (new-groups '()))
;;           (plist-put(fsm-get-state-data jc) :roster-ewoc ewoc)
;;           (dolist (group (plist-get (fsm-get-state-data jc) :roster-groups))
;;             (let* ((group-name (car group))
;;                    (buddies (jabber-roster-filter-display
;;                              (gethash group-name
;;                                       (plist-get (fsm-get-state-data jc) :roster-hash)))))
;;               (when (or jabber-roster-show-empty-group
;;                         (> (length buddies) 0))
;;                 (let ((group-node (ewoc-enter-last ewoc (list group nil))))
;;                   (if (not (find
;;                             group-name
;;                             (plist-get (fsm-get-state-data jc) :roster-roll-groups)
;;                             :test 'string=))
;;                       (dolist (buddy (reverse buddies))
;;                         (ewoc-enter-after ewoc group-node (list group buddy))))))))
;;           (goto-char (point-max))
;;           (insert "\n")
;;           (put-text-property before-ewoc (point)
;;                              'jabber-account jc)))

;;       (goto-char (point-min))
;;       (setq buffer-read-only t)
;;       (if (interactive-p)
;;           (dolist (hook '(jabber-info-message-hooks jabber-alert-info-message-hooks))
;;             (run-hook-with-args hook 'roster (current-buffer) (funcall jabber-alert-info-message-function 'roster (current-buffer)))))
;;       (when current-line
;;         ;; Go back to previous line - don't use goto-line, since it
;;         ;; sets the mark.
;;         (goto-char (point-min))
;;         (forward-line (1- current-line))
;;         ;; ...and go back to previous column
;;         (move-to-column current-column)))))

;; (defun jabber-display-roster-entry (jc group-name buddy)
;;   "Format and insert a roster entry for BUDDY at point.
;; BUDDY is a JID symbol."
;;   (if buddy
;;       (let ((buddy-str (format-spec
;;                         jabber-roster-line-format
;;                         (list
;;                          (cons ?a (jabber-propertize
;;                                    " "
;;                                    'display (get buddy 'avatar)))
;;                          (cons ?c (if (get buddy 'connected)
;;                                       (propertize "•" 'face  (cond
;;                                                               ((or (equal "away" (get buddy 'show))
;;                                                                    (equal "xa" (get buddy 'show))) 'font-lock-preprocessor-face)
;;                                                               (t 'font-lock-function-name-face)))

;;                                     " "))
;;                          (cons ?u (cdr (assoc
;;                                         (or "none"
;;                                             (get buddy 'subscription))
;;                                         jabber-roster-subscription-display)))
;;                          (cons ?n (if (> (length (get buddy 'name)) 0)
;;                                       (get buddy 'name)
;;                                     (symbol-name buddy)))
;;                          (cons ?j (symbol-name buddy))
;;                          (cons ?r (or "" (get buddy 'resource) ""))
;;                          (cons ?s (or ""
;;                                       (cdr (assoc (get buddy 'show)
;;                                                   jabber-presence-strings))
;;                                       (get buddy 'show)))
;;                          (cons ?S (if (and (get buddy 'status)
;;                                            (or (equal "Away" (get buddy 'status))
;;                                                (equal "Online" (get buddy 'status))))
;;                                       ""
;;                                     (or (jabber-fix-status (get buddy 'status)) "")))
;;                          ))))
;;         ;; NOTE!
;;         ;; start the coloration after three characters, need to update if format changes
;;         (add-text-properties 3
;;                              (length buddy-str)
;;                              (list
;;                               'face
;;                               (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
;;                                   'jabber-roster-user-online)
;;                               ;;'mouse-face
;;                               ;;(cons 'background-color "light grey")
;;                               'help-echo
;;                               (symbol-name buddy)
;;                               'jabber-jid
;;                               (symbol-name buddy)
;;                               'jabber-account
;;                               jc)
;;                              buddy-str)
;;         (insert buddy-str)

;;         (when (or (eq jabber-show-resources 'always)
;;                   (and (eq jabber-show-resources 'sometimes)
;;                        (> (jabber-count-connected-resources buddy) 1)))
;;           (dolist (resource (get buddy 'resources))
;;             (when (plist-get (cdr resource) 'connected)
;;               (let ((resource-str (format-spec jabber-resource-line-format
;;                                                (list
;;                                                 (cons ?c "*")
;;                                                 (cons ?n (if (>
;;                                                               (length
;;                                                                (get buddy 'name)) 0)
;;                                                              (get buddy 'name)
;;                                                            (symbol-name buddy)))
;;                                                 (cons ?j (symbol-name buddy))
;;                                                 (cons ?r (if (>
;;                                                               (length
;;                                                                (car resource)) 0)
;;                                                              (car resource)
;;                                                            "empty"))
;;                                                 (cons ?s (or
;;                                                           (cdr (assoc
;;                                                                 (plist-get
;;                                                                  (cdr resource) 'show)
;;                                                                 jabber-presence-strings))
;;                                                           (plist-get
;;                                                            (cdr resource) 'show)))
;;                                                 (cons ?S (if (plist-get
;;                                                               (cdr resource) 'status)
;;                                                              (jabber-fix-status
;;                                                               (plist-get (cdr resource)
;;                                                                          'status))
;;                                                            ""))
;;                                                 (cons ?p (number-to-string
;;                                                           (plist-get (cdr resource)
;;                                                                      'priority)))))))
;;                 (add-text-properties 0
;;                                      (length resource-str)
;;                                      (list
;;                                       'face
;;                                       (or (cdr (assoc (plist-get
;;                                                        (cdr resource)
;;                                                        'show)
;;                                                       jabber-presence-faces))
;;                                           'jabber-roster-user-online)
;;                                       'jabber-jid
;;                                       (format "%s/%s" (symbol-name buddy) (car resource))
;;                                       'jabber-account
;;                                       jc)
;;                                      resource-str)
;;                 (insert "\n" resource-str))))))
;;     (let ((group-name (or group-name
;;                           jabber-roster-default-group-name)))
;;       (add-text-properties 0
;;                            (length group-name)
;;                            (list
;;                             'face 'jabber-title-small
;;                             'jabber-group group-name
;;                             'jabber-account jc)
;;                            group-name)
;;       (insert group-name))))

;; ;; Arguments are FROM, BUFFER, TEXT and TITLE.  FROM is the JID of
;; ;; the sender, BUFFER is the the buffer where the message can be
;; ;; read, and TEXT is the text of the message.  TITLE is the string
;; ;; returned by `jabber-alert-message-function' for these arguments,
;; ;; so that hooks do not have to call it themselves.

;; ;; This hook is meant for user customization of message alerts.  For
;; ;; other uses, see `jabber-message-hooks'.

;; (defun toast (message)
;;   "Display a toast notification."
;;   (interactive "sMessage: ")
;;   (if (processp (ignore-errors
;;                   (start-process "toast-proc" "*toast-notifications*" "toast" (format "%s" message))))
;;       message
;;     (warn "Could not create toast notification, is toast installed?")))

;; (defun flash-buffer (buffer &optional color)
;;   (lexical-let ((this-buffer buffer)
;;                 (this-color (or color (face-foreground 'default)))
;;                 (this-cookie nil))
;;     (with-current-buffer this-buffer
;;       (setq this-cookie (face-remap-add-relative 'default `(:background ,this-color))))
;;     (run-with-timer .05 nil (lambda(&rest args)
;;                               (with-current-buffer this-buffer
;;                                 (face-remap-remove-relative this-cookie))))))

;; (defun mail-it (subject body)
;;   (require 'smtpmail)
;;   (let ((mail-interactive nil))
;;     (with-temp-buffer
;;       (insert "From:  <jordon.biondo@WIND-021>\nTo: \nSubject: \n--text follows this line--\n")
;;       (mail-to) (insert "jordon.biondo@parelio.com")
;;       (mail-subject) (insert subject)
;;       (insert "\nMime-Version: 1.0;")
;;       (insert "\nContent-Type: text/html; charset=\"ISO-8859-1\";")
;;       (insert "\nContent-Transfer-Encoding: 7bit;")
;;       (mail-text) (insert body)
;;       (mail-send))))


;; (defun jordon-jabber-toast-notification (from buffer text title)
;;   "Show a toast notification if chat buffer not visible."
;;   (if (not (get-buffer-window buffer))
;;       (toast (format "%s\n%s" title text))
;;     (unless (equal (buffer-name (current-buffer)) (buffer-name buffer))
;;       (flash-buffer buffer (face-foreground (face-foreground 'font-lock-comment-face))))))

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; OSX Terminal notification stuff
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

;; (defun terminal-notifier-notify (title message)
;;   "Show a message with `terminal-notifier-command`."
;;   (start-process "terminal-notifier"
;;                  "*terminal-notifier*"
;;                  terminal-notifier-command
;;                  "-title" (format "Emacs: %s" title)
;;                  "-message" message
;;                  "-activate" "org.gnu.Emacs"))

;; (defun jordon-jabber-terminal-notification (from buffer text title)
;;   "Show a osx notification if chat buffer not visible."
;;   (if (not (get-buffer-window buffer))
;;       (terminal-notifier-notify title text)))



;; (defvar jordon-jabber-pending-mail-msgs '()
;;   "List of messages not yet sent via email.")

;; (defvar jordon-jabber-mail-timer nil
;;   "Timer waiting to send message mail.")

;; (defvar jordon-jabber-send-mail-idle-time 120
;;   "Emacs must be idle for this many seconds in order to send mail for jabber messages.")


;; (defun jordon-jabber-send-mail-notification (from buffer text title)
;;   "Set up an idle task to send an email to myself with the message if I've \
;; been idle for a little bit when the message comes in."
;;   ;; mail only happens if I've been idle for 30 seconds!
;;   (when (> (time-to-seconds (or (current-idle-time) (seconds-to-time 0)))
;;            (or jordon-jabber-send-mail-idle-time 120))
;;     (run-with-timer
;;      2 nil
;;      (lambda (from buffer text title)
;;        (run-with-idle-timer
;;         2 nil
;;         (lambda (from buffer text title)
;;           (mail-it (format "JABBERNOT: (%s)" title)
;;                    (format "AUTOMATIC NOTIFICATION FROM JABBERNOT-BOT 9000\n\n[%s]: %s"
;;                            from text)))
;;         from buffer text title))
;;      from buffer text title)))

;; (defvar jordon-jabber-chat-header-line-format
;;   '(""
;;     (:eval (jabber-jid-displayname jabber-chatting-with))
;;     "       "
;;     (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
;;              (propertize (or (cdr (assoc (get buddy 'show)
;;                                         jabber-presence-strings))
;;                             (get buddy 'show))
;;                          'face
;;                          (or (cdr (assoc (get buddy 'show)
;;                                         jabber-presence-faces))
;;                             'jabber-roster-user-online))))
;;     "       "
;;     (:eval (jabber-fix-status (get (jabber-jid-symbol jabber-chatting-with)
;;                                    'status)))
;;     "       "
;;     jabber-events-message
;;     "       "
;;     jabber-chatstates-message))

;; (defun jordon-jabber-im-online ()
;;   (interactive)
;;   (jabber-send-presence "" "" 1))

(provide 'jordon-jabber)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jordon-jabber.el ends here
