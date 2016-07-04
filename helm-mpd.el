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

(defun helm-mpd-send-command-synchronously (cmd &rest args)
  "Send CMD synchronously and discard response.

ARGS are same as `helm-mpd-send-command'."
  (let ((proc (apply #'helm-mpd-send-command cmd args)))
    (while (process-live-p proc)
      (accept-process-output proc))))

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

(defun helm-mpd-get-candidate-cache (cmd)
  (gethash cmd helm-mpd-candidates-cache))

(defun helm-mpd-put-candidate-cache (cmd value)
  (puthash cmd value helm-mpd-candidates-cache))

(defun helm-mpd-remove-candidate-cache (cmd)
  (remhash cmd helm-mpd-candidates-cache))

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
      (setq cache (helm-mpd-get-candidate-cache cmd)))
    (or cache
        (helm-mpd-put-candidate-cache
         cmd
         (let ((buf (apply 'helm-mpd-retrieve-synchronously cmd args)))
           (unwind-protect
               (with-current-buffer buf
                 (goto-char (point-min))
                 (mapcar 'helm-mpd-filter-one-by-one (helm-mpd-parse-response)))
             (kill-buffer buf)))))))

;;;; Display candidates

(defface helm-mpd-directory-face
  '((t (:inherit dired-directory)))
  "Directory face"
  :group 'helm-mpd)

(defface helm-mpd-playlist-face
  '((t (:inherit font-lock-function-name-face)))
  "Playlist face"
  :group 'helm-mpd)

(defface helm-mpd-Artist-face
  '((t (:inherit font-lock-keyword-face)))
  "Artist face"
  :group 'helm-mpd)

(defface helm-mpd-Title-face
  '((t (:inherit font-lock-variable-name-face)))
  "Title face"
  :group 'helm-mpd)

(defface helm-mpd-Album-face
  '((t (:inherit font-lock-type-face)))
  "Album face"
  :group 'helm-mpd)

(defface helm-mpd-Pos-face
  '((t ()))
  "Position face"
  :group 'helm-mpd)

(defun helm-mpd-format-object (object width)
  (let* ((Artist-w (floor (* 0.2 width)))
         (Album-w (floor (* 0.3 width)))
         (Title-w (- width Artist-w Album-w))
         (Pos-w 0)
         (tags '(Artist Title Album)))
    (when (assq 'Pos object)
      (setq tags (cons 'Pos tags)
            Pos-w 4)
      (setq Title-w (- Title-w Pos-w)))
    (mapconcat (lambda (tag)
                 (propertize (truncate-string-to-width (or (cdr (assq tag object)) "")
                                                       (symbol-value (intern (format "%s-w" tag)))
                                                       nil ?\ )
                             'face (intern (format "helm-mpd-%s-face" tag))))
               tags "")))

(defun helm-mpd-display-object-default (object)
  (cond ((assq 'directory object)
         (propertize (cdr (assq 'directory object))
                     'face 'helm-mpd-directory-face))
        ((assq 'playlist object)
         (propertize (cdr (assq 'playlist object))
                     'face 'helm-mpd-playlist-face))
        (t
         (helm-mpd-format-object object (with-helm-window
                                          (window-text-width))))))

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

(defun helm-mpd-insert-subcontents (cmd &optional fill-prefix)
  (let ((buf (helm-mpd-retrieve-synchronously cmd))
        (fill-prefix (or fill-prefix "    ")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (goto-char (point-min))
            (while (search-forward-regexp "^\\(OK\\|ACK\\)" nil t)
              (kill-whole-line 0))
            (indent-region (point-min) (point-max)))
          (insert-buffer-substring buf))
      (kill-buffer buf))))

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
                  (cond ((assq 'directory c)
                         (helm-mpd-insert-subcontents
                          (format "listfiles %s" (cdr (assq 'directory c)))))
                        ((assq 'playlist c)
                         (helm-mpd-insert-subcontents
                          (format "listplaylist %s" (cdr (assq 'playlist c))))))
                  (insert "\n"))
                (helm-marked-candidates)))))))

(defvar helm-mpd-persistent-action 'helm-mpd-object-show)
(defvar helm-mpd-persistent-help "Show object(s) information")

(defcustom helm-mpd-object-action
  (helm-make-actions
   helm-mpd-persistent-help helm-mpd-persistent-action)
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
  (append (when (assq 'Id object)
            '(("Play song" . helm-mpd-action-play)
              ("Delete song(s)" . helm-mpd-action-delete)))
          actions))

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
  (append (when (or (assq 'file object) (assq 'directory object))
            '(("Add song(s)" . helm-mpd-action-add)))
          actions))

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
  (append (when (assq 'playlist object)
            '(("Load playlist(s)" . helm-mpd-action-load)))
          actions))

;;;; Key map

(defun helm-mpd-make-command (actions)
  "Return a command to run the first ACTIONS that can be performed in the
current helm session without exiting the session."
  (lexical-let ((actions actions))
    (lambda ()
      (interactive)
      (with-helm-alive-p
        (let* ((helm-action (helm-get-actions-from-current-source))
               (action (seq-find (lambda (a)
                                   (rassq a helm-action))
                                 actions)))
          (if action
              (progn
                (helm-attrset 'helm-mpd-persistent-action action)
                (helm-execute-persistent-action 'helm-mpd-persistent-action)
                (helm-refresh))
            (message "Cannot be perform actions %s" actions)))))))

(defun helm-mpd-refresh ()
  "Update server and refresh helm."
  (interactive)
  (helm-mpd-send-command-synchronously "update")
  (helm-refresh))

(defvar helm-mpd-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (mapc (lambda (v)
            (define-key m (kbd (car v)) (cdr v)))
          '(("C-c C-u" . helm-mpd-refresh)))
    m))

(defvar helm-mpd-source-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-mpd-map)
    (mapc (lambda (v)
            (define-key m (kbd (car v)) (cdr v)))
          `(("C-c a" . ,(helm-mpd-make-command '(helm-mpd-action-add
                                                 helm-mpd-action-load)))
            ("C-c d" . ,(helm-mpd-make-command '(helm-mpd-action-delete)))
            ("C-c RET" . ,(helm-mpd-make-command '(helm-mpd-action-play
                                                   helm-mpd-action-add
                                                   helm-mpd-action-load
                                                   helm-mpd-send-command-synchronously)))))
    m))

;;;; Helm sources

(defvar helm-mpd-base-help-message
  "* Helm for MPD

** Tag search

`helm-mpd' supports tag search. Try input \"<TAG>PATTERN\". For example
\"<artist>beatle\" matches objects that has a tag like \"artist\" whose value is
like \"beatle\". TAG and PATTERN can be regular expressions. For PATTERN, it uses
`migemo' if `helm-migemo-mode' is enabled.

** Key bindings

Some keys do different actions depending on the selected candidates.
The default bindings are following.

| Key     | Current playlist             | Songs                   | Playlists  |
|---------+------------------------------+-------------------------+------------|
| C-c a   | Add to current playlist      | Add to current playlist | Load       |
| C-c d   | Delete from current playlist | Do nothing              | Do nothing |
| C-c RET | Play                         | Add to current playlist | Load       |
")

(defclass helm-source-mpd-base (helm-source)
  ((candidates :initform (lambda ()
                           (helm-mpd-candidates-synchronously (helm-attr 'mpd-command)
                                                              :cache (helm-attr-defined 'mpd-cache))))
   (action :initform 'helm-mpd-object-action)
   (action-transformer :initform '(helm-mpd-action-transformer-playlist
                                   helm-mpd-action-transformer-song
                                   helm-mpd-action-transformer-current-playlist))
   (update :initform (lambda ()
                       (helm-mpd-remove-candidate-cache (helm-attr 'mpd-command))))
   (match :initform '(helm-mpd-match))
   (keymap :initform helm-mpd-source-map)
   (persistent-action :initform (symbol-value 'helm-mpd-persistent-action))
   (persistent-help :initform (symbol-value 'helm-mpd-persistent-help))
   (help-message :initform helm-mpd-base-help-message)
   (mpd-command :initarg :mpd-command
                :documentation "A command to retrieve candidates.")
   (mpd-cache :initarg :mpd-cache
              :initform nil
              :documentation "If non-nil, use candidates cache.")))

(defface helm-mpd-current-song-face
  '((t (:inverse-video t)))
  "Current song face"
  :group 'helm-mpd)

(defun helm-mpd-current-playlist-highlight-current (candidates &optional source)
  (let ((buf (helm-mpd-retrieve-synchronously "currentsong")))
    (unwind-protect
        (let ((id (with-current-buffer buf
                    (goto-char (point-min))
                    (when (search-forward-regexp "^Id: \\(.*\\)$" nil t)
                      (match-string 1)))))
          (if id
              (do ((up nil (cons (car down) up))
                   (down candidates (cdr down))
                   (cur nil cur))
                  ((or cur (null down))
                   (append (reverse (if cur
                                        (cons cur (cdr up))
                                      up))
                           down))
                (let* ((x (car down))
                       (i (assq 'Id (cdr x))))
                  (when (and i (equal id (cdr i)))
                    (setq cur
                          (cons (let ((nt (propertize (car x))))
                                  (add-face-text-property 0 (length nt)
                                                          'helm-mpd-current-song-face
                                                          nil nt)
                                  nt)
                                (cdr x))))))
            candidates))
      (kill-buffer buf))))

(defvar helm-source-mpd-current-playlist
  (helm-make-source "Current playlist" 'helm-source-mpd-base
    :filtered-candidate-transformer '(helm-mpd-current-playlist-highlight-current)
    :mpd-command "playlistinfo"))
(defvar helm-source-mpd-songs
  (helm-make-source "Songs" 'helm-source-mpd-base
    :mpd-command "listallinfo"
    :mpd-cache t))
(defvar helm-source-mpd-playlists
  (helm-make-source "Playlists" 'helm-source-mpd-base
    :mpd-command "listplaylists"))

;;;;; MPD commands

(defun helm-mpd-command-candidates ()
  "Get available commands."
  (let ((cmd "commands"))
    (or (helm-mpd-get-candidate-cache cmd)
        (helm-mpd-put-candidate-cache
         cmd
         (let ((buf (helm-mpd-retrieve-synchronously cmd)))
           (unwind-protect
               (let ((res nil))
                 (with-current-buffer buf
                   (goto-char (point-min))
                   (while (search-forward-regexp "^command: \\(.*\\)$" nil t)
                     (setq res (cons (match-string 1)
                                     res)))
                   (reverse res)))
             (kill-buffer buf)))))))

(defun helm-mpd-command-match (candidate)
  "Match CANDIDATE with the first word of `helm-pattern'."
  (let ((ptn (car (split-string helm-pattern nil t))))
    (or (null ptn)
        (string-match ptn candidate))))

(defun helm-mpd-command-filtered-candidate-transformer (candidates source)
  "Compose CANDIDATE and `helm-pattern' but the first word."
  (let ((args (cdr (split-string helm-pattern nil t))))
    (if args
        (let ((tail (mapconcat 'identity (cons "" args) " ")))
          (mapcar (lambda (x)
                    (concat x tail))
                  candidates))
      candidates)))

(defun helm-mpd-command-action-send (command)
  "Send COMMAND and display the response."
  (let ((buf (helm-mpd-retrieve-synchronously command)))
    (with-current-buffer buf
      (rename-buffer (format "helm-mpd-response:%s" command)
                     t)
      (view-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

(defvar helm-source-mpd-command
  (helm-make-source "Commands" 'helm-source
    :candidates 'helm-mpd-command-candidates
    :match '(helm-mpd-command-match)
    :filtered-candidate-transformer '(helm-mpd-command-filtered-candidate-transformer)
    :action '(("Send command" . helm-mpd-command-action-send)
              ("Send command (discard response)" . helm-mpd-send-command-synchronously))
    :keymap helm-mpd-source-map))

;;;;; Put together

(defvar helm-source-mpd
  '(helm-source-mpd-current-playlist
    helm-source-mpd-songs
    helm-source-mpd-playlists
    helm-source-mpd-command))

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
        :buffer (format "*helm-mpd:%s*"
                        (if (eq helm-mpd-connection-family 'local)
                            helm-mpd-connection-port
                          (format "%s:%s"
                                  helm-mpd-connection-host
                                  helm-mpd-connection-port)))
        :keymap helm-mpd-map
        :truncate-lines t))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
