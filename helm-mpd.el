;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-24 21:00:44 tuemura>
;;
;;; Code:

(require 'helm)
(require 'helm-mpdlib)
(eval-when-compile (require 'cl))

(defgroup helm-mpd nil
  "Predefined configurations for `helm-mpd.el'."
  :group 'helm)

(defcustom helm-mpd-network-parameters
  (list :name "helm-mpd connection"
        :buffer "*helm-mpd-default-output*"
        :host 'local
        :service 6600)
  "Default network parameters for `helm-mpd'."
  :group 'helm-mpd
  :type 'list)

(defun helm-mpd-interactive-network-args ()
  "Read network parameters.

If the current prefix argument is non-nil, read them from input.
Otherwise returns `helm-mpd-network-parameters'."
  (if current-prefix-arg
      (let ((family (intern (completing-read "Family: "
                                             '(default local ipv4 ipv6)
                                             nil t)))
            (host nil)
            (service nil))
        (case family
          ((default ipv4 ipv6)
           (setq host (read-string "Host (default: localhost): " nil nil "localhost"))
           (setq service (read-number "Port: " 6600)))
          ((local)
           (setq service (read-file-name "Socket: " (expand-file-name "~/.config/mpd/") nil t "socket"))))
        (when (eq family 'default)
          (setq family nil))
        (helm-mpdlib-delete-all-processes)
        (helm-mpdlib-plist-update helm-mpd-network-parameters
                                  :family family
                                  :host host
                                  :service service))
    helm-mpd-network-parameters))

(defun helm-mpd-send (str &optional callback cbargs &rest network-args)
  "Run `helm-mpdlib-send' with `helm-mpd-network-parameters'."
  (let ((network-args (apply #'helm-mpdlib-plist-update
                             helm-mpd-network-parameters
                             network-args)))
    (apply #'helm-mpdlib-send
           network-args
           str callback cbargs)))

(defun helm-mpd-action (fun &optional on-marked command)
  "Make a helm action.

FUN must be a function with one parameter.

If ON-MARKED is nil, call FUN with the selected candidate.
If ON-MRAKED is non-nil, call FUN with the list of the marked candidates.

If COMMAND is non-nil, make an interactive function
which is called in a helm session.
If COMMAND is the simbol `persistent', the function does not exit helm session."
  (lexical-let* ((fun fun)
                 (g (if on-marked
                        (lambda (_ignore)
                          (funcall fun (helm-marked-candidates)))
                      fun)))
    (case command
      ((nil) g)
      ((persistent)
       (lambda ()
         (interactive)
         (with-helm-alive-p
           (helm-attrset 'mpd-persistent-action (cons g 'never-split))
           (helm-execute-persistent-action 'mpd-persistent-action))))
      (otherwise
       (lambda ()
         (interactive)
         (with-helm-alive-p
           (helm-exit-and-execute-action g)))))))

(defun helm-mpd-show-raw-data (candidates)
  (switch-to-buffer "*helm-mpd-raw-data*")
  (erase-buffer)
  (dolist (c candidates)
    (insert (format "%S" c) "\n")))

;;;; Songs

(defface helm-mpd-state-face
  '((t (:inherit bold)))
  "State face."
  :group 'helm-mpd)

(defface helm-mpd-artist-face
  '((t (:inherit font-lock-keyword-face)))
  "Artist face."
  :group 'helm-mpd)

(defface helm-mpd-title-face
  '((t (:inherit font-lock-function-name-face)))
  "Title face."
  :group 'helm-mpd)

(defface helm-mpd-album-face
  '((t (:inherit font-lock-type-face)))
  "Album face."
  :group 'helm-mpd)

(defface helm-mpd-time-face
  '((t (:inherit font-lock-builtin-face)))
  "Time face."
  :group 'helm-mpd)

(defface helm-mpd-date-face
  '((t (:inherit font-lock-constant-face)))
  "Date face."
  :group 'helm-mpd)

(defface helm-mpd-track-face
  '((t (:inherit font-lock-string-face)))
  "Track face."
  :group 'helm-mpd)

(defcustom helm-mpd-song-format
  (lambda (song)
    (let* ((track-width 6)
           (time-width 6)
           (date-width 5)
           (total-width (window-width))
           (w (- total-width track-width time-width date-width))
           (artist-width (floor (* 0.2 w)))
           (title-width (floor (* 0.5 w)))
           (album-width (- w artist-width title-width)))
      (labels ((f (key width face)
                  (propertize (truncate-string-to-width (or (cdr (assq key song)) "")
                                                        width nil ? )
                              'face face)))
        (concat (f 'Artist artist-width 'helm-mpd-artist-face)
                (f 'Track track-width 'helm-mpd-track-face)
                (f 'Title title-width 'helm-mpd-title-face)
                (f 'Date date-width 'helm-mpd-date-face)
                (f 'Album album-width 'helm-mpd-album-face)
                (propertize (truncate-string-to-width
                             (let ((time (cdr (assq 'Time song))))
                               (if time
                                   (helm-mpd-format-time (string-to-number time))
                                 ""))
                             time-width nil ? )
                            'face 'helm-mpd-time-face)))))
  "Function to format a song."
  :group 'helm-mpd
  :type 'function)

(defvar helm-mpd-song-match-filters
  '(("f" . file)
    ("a" . Artist)
    ("t" . Title)
    ("b" . Album)
    ("y" . Date)
    ("n" . Track)
    ("g" . Genre)))

(defun helm-mpd-song--match-pattern (p song)
  (let ((mfn (if helm-migemo-mode
                 #'helm-mm-migemo-string-match
               #'string-match)))
    (cl-labels ((default (x)
                  (cl-loop for v in song
                           thereis (funcall mfn x (cdr v)))))
      (if (string-match "^%\\(.\\)\\(.*\\)$" p)
          (let* ((lead (match-string 1 p))
                 (q (match-string 2 p))
                 (key (cdr (assoc lead helm-mpd-song-match-filters))))
            (if key
                (let ((s (cdr (assq key song))))
                  (when s
                    (funcall mfn q s)))
              (default (concat lead q))))
        (default p)))))

(defun helm-mpd-songs-match-function (candidate)
  (let ((song (get-text-property 0 :real-value candidate)))
    (cl-loop with pattern = helm-pattern
             for p in (split-string pattern " ")
             always (helm-mpd-song--match-pattern p song))))

(defun helm-mpd-display-song (song)
  (propertize (funcall helm-mpd-song-format song)
              :real-value song))

(defun helm-mpd-songs-enqueue (songs)
  (let ((paths (apply #'append
                      (mapcar (lambda (song)
                                (when (assq 'file song)
                                  (list (cdr (assq 'file song)))))
                              songs))))
    (helm-mpd-send (mapcar (lambda (path)
                             (helm-mpdlib-make-command 'add path))
                           paths))))

;;;; Sources

(eval-when-compile
  (defmacro defsource (name command form class &optional args)
    (let* ((candidates (intern (format "helm-mpd-%s-candidates" name)))
           (source (intern (format "helm-mpd-%s-source" name)))
           (retrieve (intern (format "helm-mpd-%s-retrieve" name)))
           (buf (format "*helm-mpd-%s-output*" name))
           (build-source (intern (format "helm-mpd-%s-build-source" name)))
           (source-name (capitalize (replace-regexp-in-string "-" " " (format "%s" name))))
           (buf-var (intern (format "%s:buffer" retrieve)))
           (name-var (intern (format "%s:name" build-source)))
           (args-var (intern (format "%s:args" build-source))))
      `(progn
         (defvar ,candidates (cons nil nil))
         (defvar ,source nil)
         (defun ,retrieve ()
           (let ((,buf-var ,buf))
             (helm-mpd-send (helm-mpdlib-make-command ',command)
                            (lambda (&rest _ignore)
                              (while (helm-mpdlib-received-p)
                                (setq ,candidates
                                      (cons t ,form)))
                              (with-helm-buffer
                                (helm-update nil ,source)))
                            nil :buffer ,buf-var)))
         (defun ,build-source (&optional ,name-var &rest ,args-var)
           (setq ,name-var (or ,name-var ,source-name))
           (apply #'helm-make-source ,name-var ',class
                  :candidates (lambda ()
                                (unless (car ,candidates)
                                  (,retrieve))
                                (cdr ,candidates))
                  :update (lambda () (setcar ,candidates nil))
                  (append ,args
                          ,args-var))))))

  (defmacro run-helm (sources &rest args)
    (let ((let-vars (mapcar (lambda (s)
                              (unless (consp s)
                                (setq s (list s)))
                              (list (intern (format "helm-mpd-run-helm:%s-source" (car s)))
                                    (cons (intern (format "helm-mpd-%s-build-source" (car s)))
                                          (cdr s))))
                            sources))
          (args0 (apply #'append
                        (mapcar (lambda (s)
                                  (when (consp s)
                                    (setq s (car s)))
                                  (list (intern (format ":mpd-%s-source" s))
                                        (intern (format "helm-mpd-run-helm:%s-source" s))))
                                sources))))
      `(let ,let-vars
         (helm :sources (list ,@(mapcar #'car let-vars))
               :truncate-lines t
               ,@args0
               ,@args))))

  (defmacro defcommand (name &rest body)
    `(defun ,name (&rest network-args)
       (interactive (helm-mpd-interactive-network-args))
       (setq helm-mpd-network-parameters
             (apply #'helm-mpdlib-plist-update
                    helm-mpd-network-parameters
                    network-args))
       ,@body)))

(defclass helm-source-mpd-base (helm-source)
  ((is-mpd-source :initform t)))

(defun helm-source-mpd-p (source)
  (assq 'is-mpd-source source))

(defvar helm-source-mpd-songs-help-message
  "* Helm MPD songs

** Completion

*** Search by tag

If you enter a pattern prefixed by \"%CHARACTER\" helm will search for songs
by the tag specified by CHARACTER. The alist `helm-mpd-song-match-filters'
determines which character specifies a tag.

*** Examples

if I enter in pattern prompt:

    %abeatle

helm will search for songs in whose artist name \"beatle\" matches.

if I enter in pattern prompt:

    %tsmoke %adeep

helm will search for songs in whose title \"smoke\" matches and in whose artist
name \"deep\" matches.")

(defclass helm-source-mpd-songs (helm-source-mpd-base)
  ((match :initform '(helm-mpd-songs-match-function))
   (real-to-display :initform 'helm-mpd-display-song)
   (volatile :initform t)
   (help-message :initform 'helm-source-mpd-songs-help-message)))

(defvar helm-source-mpd-after-init-hook nil)
(add-hook 'helm-source-mpd-after-init-hook 'helm-mpd-mode-line-update)

(defvar helm-mpd-keys
  '((delete :key "M-D" :key-persistent "C-c d")
    (raw-data :key "C-c RET" :key-persistent "C-c C-j")
    (list :key "M-L" :key-persistent "C-c l")
    (info :key "M-I" :key-persistent "C-c i")
    (rename :key "M-R")))

(defun helm-mpd-make-actions (arg)
  "Make helm actions from ARG.

ARG must be a list of PLISTs.

Each PLIST can have properties `:action', `:type', `:key' and `:key-persistent'.

The value of `:action' must be either nil or `(DISPLAY . ACTION)'.
ACTION is passed to `helm-mpd-action'.
If DISPLAY contains the string \"(s)\", ACTION is executed on marked candidates.

The value of `:type' must be a symbol.
If the value is in `helm-mpd-keys', defines keys for ACTION and persistent ACTION.

The value of `:key' or `:key-persistent' must be either nil or `(KEY . ACTION)'.
KEY must be a string passed to `kbd' function.
They overwrite keymaps defined by `:type'.

The return value is a plist which has `:action' and `:keymap' properties."
  (do ((lst arg (cdr lst))
       (actions nil actions)
       (keymap (let ((m (make-sparse-keymap)))
                 (set-keymap-parent m helm-map)
                 m)
               keymap))
      ((null lst) (list :action (reverse actions)
                        :keymap keymap))
    (let* ((x (car lst))
           (action (plist-get x :action))
           (type (plist-get x :type))
           (key (plist-get x :key))
           (key-persistent (plist-get x :key-persistent)))
      (when action
        (let* ((disp (car action))
               (act0 (cdr action))
               (keys (cdr (assq type helm-mpd-keys)))
               (on-marked (string-match "(s)" disp)))
          (setq key (or key
                        (let ((k (plist-get keys :key)))
                          (when k
                            (cons k (helm-mpd-action act0 on-marked t))))))
          (setq key-persistent (or key-persistent
                                   (let ((k (plist-get keys :key-persistent)))
                                     (when k
                                       (cons k (helm-mpd-action act0 on-marked 'persistent))))))
          (setq actions
                (cons (cons (apply #'concat disp
                                   `(,@(when key
                                         `(" `" ,(car key) "'"))
                                     ,@(when key-persistent
                                         `(" `" ,(car key-persistent) " (keeping session)'"))))
                            (helm-mpd-action act0 on-marked))
                      actions))))
      (when key
        (define-key keymap (kbd (car key)) (cdr key)))
      (when key-persistent
        (define-key keymap (kbd (car key-persistent)) (cdr key-persistent))))))

;;;;; Current playlist

(defun helm-mpd-current-playlist-play (song)
  (let ((pos (cdr (assq 'Pos song))))
    (when pos
      (helm-mpd-send (helm-mpdlib-make-command 'play pos)))))

(defun helm-mpd-current-playlist-delete (songs)
  (let ((poss (apply #'append
                     (mapcar (lambda (song)
                               (let ((p (assq 'Pos song)))
                                 (when p
                                   (list (string-to-number (cdr p))))))
                             songs))))
    (helm-mpd-send (mapcar (lambda (pos)
                             (helm-mpdlib-make-command 'delete pos))
                           (seq-uniq (seq-sort '> poss))))))

(defun helm-mpd-current-playlist-move (n)
  (interactive (list (if current-prefix-arg
                         (cond ((listp current-prefix-arg)
                                (car current-prefix-arg))
                               (t current-prefix-arg))
                       (read-number "Move to: "))))
  (with-helm-alive-p
    (let ((c (helm-get-selection)))
      (when c
        (let ((pos (cdr (assq 'Pos c))))
          (when pos
            (helm-mpd-send (helm-mpdlib-make-command 'move pos n))))))))
(put 'helm-mpd-current-playlist-move 'helm-only t)

(defvar helm-mpd-current-playlist-actions
  `((:action ("Play song" . helm-mpd-current-playlist-play))
    (:action ("Delete song(s)" . helm-mpd-current-playlist-delete)
             :type delete)
    (:action ("Show raw data(s)" . helm-mpd-show-raw-data)
             :type raw-data)
    (:key-persistent ("M-g g" . helm-mpd-current-playlist-move))))

(defsource current-playlist playlistinfo
  (helm-mpdlib-read-objects '(file))
  helm-source-mpd-songs
  (helm-mpd-make-actions helm-mpd-current-playlist-actions))

(defcommand helm-mpd-current-playlist
  (run-helm ((current-playlist nil
                               :after-init-hook 'helm-source-mpd-after-init-hook))
            :buffer "*helm-mpd-current-playlist*"))

;;;;; Libraries

(defvar helm-mpd-library-actions
  '((:action ("Enqueue song(s)" . helm-mpd-songs-enqueue))
    (:action ("Show raw data(s)" . helm-mpd-show-raw-data)
             :type raw-data)))

(defsource library listallinfo
  (cl-loop for x in (helm-mpdlib-read-objects '(file directory playlist))
           when (assq 'file x)
           collect x)
  helm-source-mpd-songs
  (helm-mpd-make-actions helm-mpd-library-actions))

(defcommand helm-mpd-library
  (run-helm ((library nil
                      :after-init-hook 'helm-source-mpd-after-init-hook))
            :buffer "*helm-mpd-library*"))

;;;;; Play lists

(defun helm-mpd-playlist-names (playlists)
  (apply #'append
         (mapcar (lambda (x)
                   (when (assq 'playlist x)
                     (list (cdr (assq 'playlist x)))))
                 playlists)))

(defun helm-mpd-playlist-load (playlists)
  (helm-mpd-send (mapcar (lambda (n)
                           (helm-mpdlib-make-command 'load n))
                         (helm-mpd-playlist-names playlists))))

(defun helm-mpd-playlist-remove (playlists)
  (helm-mpd-send (mapcar (lambda (n)
                           (helm-mpdlib-make-command 'rm n))
                         (helm-mpd-playlist-names playlists))))

(defun helm-mpd-playlist-rename (playlist)
  (let ((x (assq 'playlist playlist)))
    (when x
      (let ((name (completing-read "Rename to: "
                                   (mapcar (lambda (c)
                                             (cdr (assq 'playlist c)))
                                           helm-mpd-playlist-candidates))))
        (helm-mpd-send (helm-mpdlib-make-command 'rename (cdr x) name))))))

(defun helm-mpd-playlist-info (playlists)
  (let ((buf "*helm-mpd-playlist-info*"))
    (switch-to-buffer buf)
    (erase-buffer)
    (helm-mpd-send (mapcar (lambda (n)
                             (helm-mpdlib-make-command 'listplaylistinfo n))
                           (helm-mpd-playlist-names playlists))
                   #'ignore nil :buffer buf)))

(defun helm-mpd-playlist-list (playlists)
  (let ((buf "*helm-mpd-playlist-list*"))
    (switch-to-buffer buf)
    (erase-buffer)
    (helm-mpd-send (mapcar (lambda (n)
                             (helm-mpdlib-make-command 'listplaylist n))
                           (helm-mpd-playlist-names playlists))
                   #'ignore nil :buffer buf)))

(defvar helm-mpd-playlist-actions
  '((:action ("Load playlist(s)" . helm-mpd-playlist-load))
    (:action ("List playlist(s)" . helm-mpd-playlist-list)
             :type list)
    (:action ("Show playlist(s)' info" . helm-mpd-playlist-info)
             :type info)
    (:action ("Remove playlist(s)" . helm-mpd-playlist-remove)
             :type delete)
    (:action ("Rename playlist" . helm-mpd-playlist-rename)
             :type rename)
    (:action ("Show raw data(s)" . helm-mpd-show-raw-data)
             :type raw-data)))

(defsource playlist listplaylists
  (cl-loop for x in (helm-mpdlib-read-objects '(playlist))
           when (assq 'playlist x)
           collect x)
  helm-source-mpd-base
  `(:real-to-display (lambda (c)
                       (cdr (assq 'playlist c)))
                     ,@(helm-mpd-make-actions helm-mpd-playlist-actions)))

(defcommand helm-mpd-playlist
  (run-helm ((playlist nil
                       :after-init-hook 'helm-source-mpd-after-init-hook))
            :buffer "*helm-mpd-playlist*"))

(defun helm-mpd-new-playlist-save (name)
  (helm-mpd-send (helm-mpdlib-make-command 'save name)))

(defvar helm-mpd-new-playlist-actions
  (helm-make-actions
   "Save the current playlist" (helm-mpd-action 'helm-mpd-new-playlist-save))
  "Actions for `helm-mpd-new-playlist'.")

(defun helm-mpd-new-playlist-build-source (&optional name &rest args)
  (setq name (or name "Create playlist"))
  (apply #'helm-make-source name 'helm-source-mpd-base
         :action 'helm-mpd-new-playlist-actions
         :filtered-candidate-transformer (lambda (c s)
                                           (list helm-pattern))
         args))

;;;;; Command

(defun helm-mpd-command-send (command)
  (let ((buf "*helm-mpd-send-command-output*")
        (args (read-string "Arguments: ")))
    (switch-to-buffer buf)
    (erase-buffer)
    (helm-mpd-send (helm-mpdlib-make-command command args)
                   #'ignore nil
                   :buffer buf)))

(defun helm-mpd-command-browse-help (command)
  "Browse COMMAND help on `eww'."
  (eww (gethash (format "%s" command) helm-mpd-command-help-paths)))

(defvar helm-mpd-command-actions
  '((:action ("Send command" . helm-mpd-command-send))
    (:action ("Browse command help" . helm-mpd-command-browse-help))))

(defsource command commands
  (cl-loop for x in (cdr (assq :data (helm-mpdlib-read-response)))
           collect (cdr x))
  helm-source-mpd-base
  `(:persistent-action helm-mpd-command-browse-help
                       :persistent-help "Browse help"
                       ,@(helm-mpd-make-actions helm-mpd-command-actions)))

(defcommand helm-mpd-command
  (run-helm ((command nil
                      :after-init-hook 'helm-source-mpd-after-init-hook))
            :buffer "*helm-mpd-commands*"))

;;;;; Put together

(defcommand helm-mpd
  (run-helm ((current-playlist nil
                               :after-init-hook 'helm-source-mpd-after-init-hook)
             library playlist command new-playlist)
            :buffer "*helm-mpd*"))

;;;; Mode line

(defcustom helm-mpd-fancy-mode-line t
  "If non-nil, display the current playback info in the mode line of the helm buffer."
  :group 'helm-mpd
  :type 'boolean)

(defvar helm-mpd-mode-line-data nil)

(defun helm-mpd-mode-line-data-update (key value)
  "Update a value of `helm-mpd-mode-line-data'."
  (let ((c (assq key helm-mpd-mode-line-data)))
    (if c
        (setcdr c value)
      (setq helm-mpd-mode-line-data
            (cons (cons key value) helm-mpd-mode-line-data)))))

(defun helm-mpd-mode-line-update-callback (proc)
  (while (helm-mpdlib-received-p)
    (let ((res (cdr (assq :data (helm-mpdlib-read-response)))))
      (mapc (lambda (x)
              (helm-mpd-mode-line-data-update (car x) (cdr x)))
            res)))
  (condition-case e
      (with-helm-buffer
        (force-mode-line-update))
    (error nil)))

(defun helm-mpd-mode-line-update ()
  (when helm-mpd-fancy-mode-line
    (condition-case e
        (with-helm-buffer
          (let ((buf "*helm-mpd-mode-line-output*"))
            (helm-mpd-send (list (helm-mpdlib-make-command 'status)
                                 (helm-mpdlib-make-command 'stats)
                                 (helm-mpdlib-make-command 'currentsong))
                           #'helm-mpd-mode-line-update-callback
                           nil :buffer buf))
          (run-with-timer 1 nil #'helm-mpd-mode-line-update))
      (error nil))))

(defun helm-mpd-format-time (time)
  (let ((fmt (if (>= time 3600)         ;more than or equal to 1 hour
                 "%H:%M:%S"
               "%M:%S")))
    (format-time-string fmt `(0 ,time 0 0))))

(defcustom helm-mpd-mode-line-format
  '(""
    (:propertize (:eval (let ((state (cdr (assq 'state helm-mpd-mode-line-data))))
                          (cond ((equal state "play")
                                 "Playing: ")
                                ((equal state "pause")
                                 "Paused: ")
                                ((equal state "stop")
                                 "Stopped: ")
                                (t "Unknown state: "))))
                 face helm-mpd-state-face)
    (:propertize (:eval (cdr (assq 'Artist helm-mpd-mode-line-data)))
                 face helm-mpd-artist-face)
    " "
    ("("
     (:propertize (:eval (cdr (assq 'Track helm-mpd-mode-line-data)))
                  face helm-mpd-track-face)
     ")")
    " "
    (:propertize (:eval (cdr (assq 'Title helm-mpd-mode-line-data)))
                 face helm-mpd-title-face)
    " "
    ("("
     (:propertize (:eval (cdr (assq 'Date helm-mpd-mode-line-data)))
                  face helm-mpd-date-face)
     ")")
    " "
    (:propertize (:eval (cdr (assq 'Album helm-mpd-mode-line-data)))
                 face helm-mpd-album-face)
    " "
    ("["
     (:eval (let ((x (cdr (assq 'time helm-mpd-mode-line-data))))
              (when x
                (let ((y (helm-mpdlib-parse-time x)))
                  (when y
                    `((:propertize ,(helm-mpd-format-time (car y))
                                   face helm-mpd-time-face)
                      "/"
                      (:propertize ,(helm-mpd-format-time (cdr y))
                                   face helm-mpd-time-face)))))))
     "]"))
  "Mode line format in `helm-mpd'."
  :group 'helm-mpd
  :type 'sexp)

(defun helm-mpd-display-mode-line-ad (source &optional force)
  "Advice for `helm-display-mode-line'."
  (when (and helm-mpd-fancy-mode-line (helm-source-mpd-p source))
    (setq mode-line-format helm-mpd-mode-line-format)
    (when force
      (force-mode-line-update))))

(advice-add 'helm-display-mode-line :after 'helm-mpd-display-mode-line-ad)

;;;; Misc

(defvar helm-mpd-command-help-paths
  (let ((m (make-hash-table :test 'equal))
        (root "http://www.musicpd.org/doc/protocol/"))
    (dolist (v '(("add" . "queue.html")
                 ("addid" . "queue.html")
                 ("addtagid" . "queue.html")
                 ("channels" . "client_to_client.html")
                 ("clear" . "queue.html")
                 ("clearerror" . "command_reference.html")
                 ("cleartagid" . "queue.html")
                 ("close" . "connection_commands.html")
                 ("commands" . "reflection_commands.html")
                 ("config" . "reflection_commands.html")
                 ("consume" . "playback_option_commands.html")
                 ("count" . "database.html")
                 ("crossfade" . "playback_option_commands.html")
                 ("currentsong" . "command_reference.html")
                 ("decoders" . "reflection_commands.html")
                 ("delete" . "queue.html")
                 ("deleteid" . "queue.html")
                 ("disableoutput" . "output_commands.html")
                 ("enableoutput" . "output_commands.html")
                 ("find" . "database.html")
                 ("findadd" . "database.html")
                 ("idle" . "command_reference.html")
                 ("kill" . "connection_commands.html")
                 ("list" . "database.html")
                 ("listall" . "database.html")
                 ("listallinfo" . "database.html")
                 ("listfiles" . "database.html")
                 ("listmounts" . "mount.html")
                 ("listplaylist" . "playlist_files.html")
                 ("listplaylistinfo" . "playlist_files.html")
                 ("listplaylists" . "playlist_files.html")
                 ("load" . "playlist_files.html")
                 ("lsinfo" . "database.html")
                 ("mixrampdb" . "playback_option_commands.html")
                 ("mixrampdelay" . "playback_option_commands.html")
                 ("mount" . "mount.html")
                 ("move" . "queue.html")
                 ("moveid" . "queue.html")
                 ("next" . "playback_commands.html")
                 ("notcommands" . "reflection_commands.html")
                 ("outputs" . "output_commands.html")
                 ("password" . "connection_commands.html")
                 ("pause" . "playback_commands.html")
                 ("ping" . "connection_commands.html")
                 ("play" . "playback_commands.html")
                 ("playid" . "playback_commands.html")
                 ("playlist" . "queue.html")
                 ("playlistadd" . "playlist_files.html")
                 ("playlistclear" . "playlist_files.html")
                 ("playlistdelete" . "playlist_files.html")
                 ("playlistfind" . "queue.html")
                 ("playlistid" . "queue.html")
                 ("playlistinfo" . "queue.html")
                 ("playlistmove" . "playlist_files.html")
                 ("playlistsearch" . "queue.html")
                 ("plchanges" . "queue.html")
                 ("plchangesposid" . "queue.html")
                 ("previous" . "playback_commands.html")
                 ("prio" . "queue.html")
                 ("prioid" . "queue.html")
                 ("random" . "playback_option_commands.html")
                 ("rangeid" . "queue.html")
                 ("readcomments" . "database.html")
                 ("readmessages" . "client_to_client.html")
                 ("rename" . "playlist_files.html")
                 ("repeat" . "playback_option_commands.html")
                 ("replay_gain_mode" . "playback_option_commands.html")
                 ("replay_gain_status" . "playback_option_commands.html")
                 ("rescan" . "database.html")
                 ("rm" . "playlist_files.html")
                 ("save" . "playlist_files.html")
                 ("search" . "database.html")
                 ("searchadd" . "database.html")
                 ("searchaddpl" . "database.html")
                 ("seek" . "playback_commands.html")
                 ("seekcur" . "playback_commands.html")
                 ("seekid" . "playback_commands.html")
                 ("sendmessage" . "client_to_client.html")
                 ("setvol" . "playback_option_commands.html")
                 ("shuffle" . "queue.html")
                 ("single" . "playback_option_commands.html")
                 ("stats" . "command_reference.html")
                 ("status" . "command_reference.html")
                 ("stop" . "playback_commands.html")
                 ("subscribe" . "client_to_client.html")
                 ("swap" . "queue.html")
                 ("swapid" . "queue.html")
                 ("tagtypes" . "reflection_commands.html")
                 ("toggleoutput" . "output_commands.html")
                 ("unsubscribe" . "client_to_client.html")
                 ("update" . "database.html")
                 ("urlhandlers" . "reflection_commands.html")
                 ("volume" . "playback_option_commands.html")))
      (puthash (car v) (concat root
                               (cdr v)
                               "#command_"
                               (car v))
               m))
    (puthash "sticker" (concat root "stickers.html") m)
    (puthash "unmount" (concat root "mount.html" "#command_umount") m)
    m))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
