;;; helm-mpd.el --- Helm interface for Music Player Daemon
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Version: 0.1
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; Package-Requires: ((helm-core "1.9.7"))
;; License: GPL3
;;
;;; Code:

(require 'helm)

(defgroup helm-mpd ()
  "Helm interface for Music Player Daemon."
  :group 'helm)

(defcustom helm-mpd-connection-host "localhost"
  "MPD host."
  :group 'helm-mpd
  :type '(choice (const nil)
                 string))

(defcustom helm-mpd-connection-port 6600
  "MPD port."
  :group 'helm-mpd
  :type '(choice number
                 string))

(defcustom helm-mpd-connection-family nil
  "Address family."
  :group 'helm-mpd
  :type '(choice (const :tag "default" nil)
                 (const :tag "socket" local)
                 (const :tag "IPv4" ipv4)
                 (const :tag "IPv6" ipv6)))

;;;; Interaction with server

(defun helm-mpd-make-connection-parameter (&rest args)
  (list :host (or (plist-get args :host)
                  helm-mpd-connection-host)
        :service (or (plist-get args :service)
                     helm-mpd-connection-port)
        :family (or (plist-get args :family)
                    helm-mpd-connection-family)))

(defun helm-mpd-retrieve-synchronously (cmd &rest args)
  "Send CMD, wait for response and return a new buffer containing the response.

ARGS are the following keyword arguments passed to `make-network-process'.

`:name'

The name of process.

`:host'

Host name. Default value is `helm-mpd-host'.

`:service'

Port. Default value is `helm-mpd-port'.

`:family'

To use a local address, set this property to `local'
and set `:service' to the path to the socket."
  (let* ((buf (generate-new-buffer "*helm-mpd-output*"))
         (proc (apply #'make-network-process
                      :name (or (plist-get args :name)
                                (format "helm-mpd:%s" cmd))
                      :buffer buf
                      :sentinel 'ignore
                      (apply #'helm-mpd-make-connection-parameter args))))
    (process-send-string proc (concat cmd "\n"))
    (process-send-eof proc)
    (while (process-live-p proc)
      (accept-process-output proc))
    buf))

(defun helm-mpd-send-command (cmd &rest args)
  "Send CMD asynchronously and return a process.

ARGS are same as `helm-mpd-retrieve-synchronously'."
  (let ((proc (apply #'make-network-process
                     :name (or (plist-get args :name)
                               (format "helm-mpd:%s" cmd))
                     (apply #'helm-mpd-make-connection-parameter args))))
    (process-send-string proc (concat cmd "\n"))
    (process-send-eof proc)
    proc))

(defvar helm-mpd-item-keywords '("file" "directory" "playlist"))

(defun helm-mpd-parse-response ()
  "Parse response from the point."
  (let ((buf (generate-new-buffer "helm-mpd-temp-buffer")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "(())")
            (backward-char 2))
          (while (search-forward-regexp "^\\([^:\n]*\\): \\([^\n]*\\)\n" nil t)
            (let ((key (match-string 1))
                  (value (match-string 2)))
              (with-current-buffer buf
                (when (seq-position helm-mpd-item-keywords key 'equal)
                  (up-list)
                  (insert "()")
                  (backward-char 1))
                (insert (format "(%s . %S)" key value)))))
          (with-current-buffer buf
            (goto-char (point-min))
            (cdr (sexp-at-point))))
      (kill-buffer buf))))

;;;; Collect candidates

(defvar helm-mpd-candidates-cache (make-hash-table :test 'equal))

(defun helm-mpd-clear-candidates-cache ()
  (clrhash helm-mpd-candidates-cache))

(defun helm-mpd-candidates-synchronously (cmd &rest args)
  "Collect candidates for CMD synchronously.

ARGS are passed to `helm-mpd-candidates-synchronously'
and allow some additional arguments.

`:cache'

If non-nil, try to use the previous result for CMD."
  (let ((cache nil))
    (when (plist-get args :cache)
      (setq cache (gethash cmd helm-mpd-candidates-cache)))
    (or cache
        (let ((buf (apply 'helm-mpd-retrieve-synchronously cmd args)))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (puthash cmd (mapcar 'helm-mpd-filter-one-by-one (helm-mpd-parse-response))
                         helm-mpd-candidates-cache))
            (kill-buffer buf))))))

;;;; Display candidates

(defun helm-mpd-display-object-default (object)
  (or (cdr (assq 'directory object))
      (cdr (assq 'playlist object))
      (concat (cdr (assq 'Artist object))
              " "
              (cdr (assq 'Title object))
              " "
              (cdr (assq 'Album object)))))

(defcustom helm-mpd-display-object-function
  'helm-mpd-display-object-default
  "Function displaying an object."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-filter-one-by-one (object)
  "Convert OBJECT to (DISPLAY . OBJECT)."
  (cons (propertize (funcall helm-mpd-display-object-function
                             object)
                    'mpd-object object)
        object))

;;;; Match function

(defun helm-mpd-match-function-1 (pattern object &optional migemo)
  (let* ((mfn1 (if migemo
                   'helm-mm-migemo-string-match
                 'string-match))
         (mfn (cond ((string-match "^<\\([^<>]*\\)>\\(.*\\)$" pattern)
                     (let ((key (match-string 1 pattern))
                           (value (match-string 2 pattern)))
                       `(lambda (x)
                          (and (string-match ,key (format "%s" (car x)))
                               (funcall mfn1 ,value (cdr x))))))
                    (t
                     (lambda (x)
                       (funcall mfn1 pattern (cdr x)))))))
    (seq-filter mfn object)))

(defun helm-mpd-match-function (pattern object &optional migemo)
  (not (seq-filter (lambda (p)
                     (not (helm-mpd-match-function-1 p object migemo)))
                   (split-string pattern))))

(defun helm-mpd-match (candidate)
  "Match function.

By default search by any tag.

To specify a tag, input \"<TAG>PATTERN\"."
  (let ((object (get-text-property 0 'mpd-object candidate)))
    (helm-mpd-match-function helm-pattern object
                             (and (boundp 'helm-migemo-mode) helm-migemo-mode))))

;;;; Actions

;;;;; General

(defvar helm-mpd--info-buffer "*helm-mpd-info*")

(defun helm-mpd-object-show (_ignore)
  "Show candidates' information."
  (let ((buf (get-buffer-create helm-mpd--info-buffer)))
    (display-buffer buf)
    (with-current-buffer buf
      (view-mode)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (save-excursion
          (mapc (lambda (c)
                  (mapc (lambda (x)
                          (when (consp x)
                            (insert (format "%s: %s\n"
                                            (car x) (cdr x)))))
                        c)
                  (insert "\n"))
                (helm-marked-candidates)))))))

(defcustom helm-mpd-object-action
  (helm-make-actions
   "Show object(s) information" 'helm-mpd-object-show)
  "Default action on objects."
  :group 'helm-mpd
  :type 'alist)

;;;;; Actions for the current playlist

(defun helm-mpd-action-play (object)
  "Play the selected candidate."
  (helm-mpd-send-command (format "playid %s"
                                 (cdr (assq 'Id object)))))

(defun helm-mpd-action-delete (_ignore)
  "Delete the selected candidates from the current playlist."
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "deleteid %s"
                                                      (cdr (assq 'Id c))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-current-playlist (actions object)
  (append actions
          (when (assq 'Id object)
            '(("Play song" . helm-mpd-action-play)
              ("Delete song(s)" . helm-mpd-action-delete)))))

;;;;; Actions for songs

(defun helm-mpd-action-add (_ignore)
  "Add the selected candidates to the current playlist."
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "add %s"
                                                      (cdr (or (assq 'file c)
                                                               (assq 'directory c)))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-song (actions object)
  (append actions
          (when (or (assq 'file object) (assq 'directory object))
            '(("Add song(s)" . helm-mpd-action-add)))))

;;;;; Actions for playlists

(defun helm-mpd-action-load (_ignore)
  "Load the selected playlists."
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "load %s"
                                                      (cdr (assq 'playlist c))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-playlist (actions object)
  (append actions
          (when (assq 'playlist object)
            '(("Load playlist(s)" . helm-mpd-action-load)))))

;;;; Helm sources

(defclass helm-source-mpd-base (helm-source)
  ((candidates :initform (lambda ()
                           (helm-mpd-candidates-synchronously (helm-attr 'mpd-command)
                                                              :cache (helm-attr-defined 'mpd-cache))))
   (filter-one-by-one :initform 'helm-mpd-filter-one-by-one)
   (action :initform 'helm-mpd-object-action)
   (action-transformer :initform '(helm-mpd-action-transformer-current-playlist
                                   helm-mpd-action-transformer-song
                                   helm-mpd-action-transformer-playlist))
   (update :initform (lambda ()
                       (when (helm-attr-defined 'mpd-cache)
                         (helm-mpd-candidates-synchronously (helm-attr 'mpd-command)))))
   (match :initform '(helm-mpd-match))
   (mpd-command :initarg :mpd-command
                :documentation "A command to retrieve candidates.")
   (mpd-cache :initarg :mpd-cache
              :initform nil
              :documentation "If non-nil, use candidates cache.")))

(defvar helm-source-mpd-current-playlist
  (helm-make-source "Current playlist" 'helm-source-mpd-base
    :mpd-command "playlistinfo"))
(defvar helm-source-mpd-songs
  (helm-make-source "Songs" 'helm-source-mpd-base
    :mpd-command "listallinfo"
    :mpd-cache t))
(defvar helm-source-mpd-playlists
  (helm-make-source "Playlists" 'helm-source-mpd-base
    :mpd-command "listplaylists"))

(defvar helm-source-mpd
  '(helm-source-mpd-current-playlist
    helm-source-mpd-songs
    helm-source-mpd-playlists))

;;;; Entry point

(defun helm-mpd-read-connection-parameter ()
  (let ((family (let ((type (get 'helm-mpd-connection-family 'custom-type)))
                  (when (and (consp type) (eq (car type) 'choice))
                    (let ((ass (mapcar (lambda (x) (cons (plist-get (cdr x) :tag)
                                                         (nth (1- (length x)) x)))
                                       (cdr type))))
                      (cdr (assoc (completing-read "Family: " ass
                                                   nil t nil nil "default")
                                  ass))))))
        (host nil)
        (service nil))
    (cond ((eq family 'local)
           (setq service (read-file-name "Socket: "
                                         nil nil t "~/.config/mpd/socket")))
          (t
           (setq host (read-string "Host: " (or helm-mpd-connection-host "localhost")))
           (setq service (read-number "Port: " (if (numberp helm-mpd-connection-port)
                                                   helm-mpd-connection-port
                                                 6600)))))
    (list :family family
          :host host
          :service service)))

;;;###autoload
(defun helm-mpd (&rest args)
  "Helm for MPD.

Called interactively with a prefix argument, prompt address family, host and port."
  (interactive (when current-prefix-arg
                 (helm-mpd-read-connection-parameter)))
  (when args
    (helm-mpd-clear-candidates-cache))
  (when (plist-member args :family)
    (setq helm-mpd-connection-family (plist-get args :family)))
  (when (plist-member args :host)
    (setq helm-mpd-connection-host (plist-get args :host)))
  (when (plist-member args :service)
    (setq helm-mpd-connection-port (plist-get args :service)))
  (helm :sources helm-source-mpd
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
