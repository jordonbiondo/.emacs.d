;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customer snippet server / creator
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'simple-httpd)
(require 'htmlfontify)

(defvar jordon-snippet-current-name "Snippet: <unknown>")

(defvar jordon-snippet-server nil
  "Server process")

(defun jordon-stop-snippet-server ()
  "Stop the snippet server."
  (interactive)
  (when (process-status "snippet-server")
    (delete-process "snippet-server")
    (httpd-log `(stop ,(current-time-string)))))

(defun jordon-start-snippet-server()
  "(re)start the snippet server."
  (interactive)
  (jordon-stop-snippet-server)
  (setq jordon-snippet-server
        (make-network-process
         :name     "snippet-server"
         :service  80
         :server   t
         :host     httpd-host
         :family   httpd-ip-family
         :filter   'httpd--filter
         :filter-multibyte nil
         :coding   'utf-8-unix  ; *should* be ISO-8859-1 but that doesn't work
         :log      'httpd--log)))

(defun jordon-snippet-region (beg end)
  "Create a new snippet of the region, url will be added to kill ring."
  (interactive "r")
  (let* ((name (buffer-name))
         (text (buffer-substring beg end))
         (file (new-snippet-name))
         (path (format "%s/snippets/%s" httpd-root file)))
    (with-temp-file path
      (rename-buffer (concat "Snippet: " name))
      (let ((inhibit-read-only t))
        (when (progn
                (let ((hfy-page-header 'jordon-snippet-header))
                  (with-named-snippet name
                    (insert (htmlfontify-string text))))
                (do-in-snippet)
                t)
          (message "snippet created: %s" (kill-new (format "http://10.23.1.93/snippets/%s" file))))))
    path))


(defun new-snippet-name ()
  (format "%s.html"
          (substring (md5 (format "%S" (current-time))) 0 6)))

(defun jordon-snippet-buffer ()
  "Create a new snippet of the buffer, url will be added to kill ring."
  (interactive)
  (let* ((name (buffer-name))
         (file (new-snippet-name))
         (path (format "%s/snippets/%s" httpd-root file)))
    (with-current-buffer (let ((inhibit-read-only t)
                               (hfy-page-header 'jordon-snippet-header))
                           (with-named-snippet name
                             (htmlfontify-buffer)))
      (let ((text (buffer-string)))
        (with-temp-file path
          (insert text)
          (do-in-snippet))
        (message "snippet created: %s" (kill-new (format "http://10.23.1.93/snippets/%s" file)))))
    path))

(defun do-in-snippet ()
  "hacky"
  (save-excursion
    (goto-char (point-min))
    (search-forward "<head>")
    (goto-char (point-at-eol))
    (newline)
    (insert "<link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono&subset=latin,cyrillic-ext' rel='stylesheet' type='text/css'>")
    (search-forward "<style")
    (goto-char (point-at-eol))
    (newline)
    (insert "pre {font: monospace; font-family: 'Ubuntu Mono', Consolas, monospace;}")
    (replace-regexp "\\(font-family: [a-z\-]+;\\)" "font: monospace; font-family: 'Ubuntu Mono', monospace Consolas;" nil (point-min) (point-max))))

(defmacro with-named-snippet (name &rest body)
  (declare (indent defun))
  `(let ((jordon-snippet-current-name (concat "Snippet: " ,name)))
     ,@body))

(defun jordon-snippet-header(file style)
  (with-temp-buffer
    (insert (funcall 'hfy-default-header file style))
    (goto-char 1)
    (search-forward "<title>")
    (delete-region (point-at-bol) (point-at-eol))
    (insert (format "<title>%s</title>" (mapconcat (lambda (c) (hfy-html-quote (string c))) jordon-snippet-current-name nil )))
    (insert "\n")
    (insert "<link rel=\"icon\" type=\"image/png\" href=\"/favicon.png\"/>\n")
    (buffer-string)))

(defun browse-snippet ()
  (interactive)
  (browse-url (read-file-name "snippet: " "~/public_html/snippets"/ nil t)))

(defun send-snippet-region (beg end)
  (interactive "r")
  (let ((path (jordon-snippet-region beg end)))
    (mail-it "Test output"
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string)))))

(provide 'jordon-snipppets)
