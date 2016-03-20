;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-20 19:04:17 tuemura>
;;
;;; Code:

(require 'helm)
(require 'helm-mpdlib)
(require 'libmpdee)

(defgroup helm-mpd nil
  "Predefined configurations for `helm-mpd.el'."
  :group 'helm)

(defcustom helm-mpd-host "localhost"
  "Default host for `helm-mpd'."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-port 6600
  "Default port for `helm-mpd'."
  :group 'helm-mpd
  :type 'integer)

(defun helm-mpd-interactive-host-and-port ()
  "Read host and port.

If the current prefix argument is non-nil, read them from input.
Otherwise returns `helm-mpd-host' and `helm-mpd-port'."
  (if current-prefix-arg
      (list (read-string "Host: " nil nil helm-mpd-host)
            (read-number "Port: " helm-mpd-port))
    (list helm-mpd-host helm-mpd-port)))

(defun helm-mpd-read-host-and-port ()
  "Read MPD host and port, and return a MPD connection."
  (if current-prefix-arg
      (let ((host (car mpd-interactive-connection-parameters))
            (port (cadr mpd-interactive-connection-parameters))
            (args (cddr mpd-interactive-connection-parameters)))
        (mpd-conn-new (read-string (format "Host (default: %s): " host)
                                   nil nil host)
                      (read-number "Port: " port)
                      helm-mpd-timeout))
    mpd-inter-conn))

(defun helm-mpd-send (str callback &optional cbarg)
  "Run `helm-mpdlib-send' with `helm-mpd-host' and `helm-mpd-port'."
  (helm-mpdlib-send helm-mpd-host helm-mpd-port
                    str callback cbarg))

(eval-when-compile
  (defun filter-variables (vars f)
    (apply 'concatenate 'list
           (mapcar (lambda (x)
                     (unless (eq (aref (symbol-name x) 0) ?&)
                       (list (funcall f x))))
                   vars)))

  (defmacro defclosure (name vars &optional docstring &rest body)
    "Define a closure.
BODY should return a `lambda' form."
    (declare (indent defun)
             (doc-string 3))
    (let ((let-vars (filter-variables vars (lambda (x) (list x x))))
          (doc nil))
      (if (stringp docstring)
          (setq doc (list docstring))
        (setq body (cons docstring body)))
      `(defun ,name ,vars
         ,@doc
         (lexical-let ,let-vars
           ,@body))))

  (defmacro helm-mpd-defaction (name vars &rest body)
    "Define a `helm-mpd' action. It defines three closures `helm-mpd-NAME',
`helm-mpd-run-NAME' and `helm-mpd-run-NAME-persistent'.

BODY must return a helm action.

`helm-mpd-NAME' is used as a helm action.

You can bind some key to `helm-mpd-run-NAME' in helm key map.

`helm-mpd-run-NAME-persistent' is similar to `helm-mpd-run-NAME'
but does not exit helm session."
    (declare (indent defun)
             (doc-string 3))
    (let* ((action (intern (format "helm-mpd-%s" name)))
           (run-action (intern (format "helm-mpd-run-%s" name)))
           (run-action-doc (format "Run `%s' action from helm session." action))
           (persistent-action (intern (format "helm-mpd-run-%s-persistent" name)))
           (persistent-action-doc (format "Run `%s' action without exiting helm session." action))
           (vars1 (filter-variables vars 'identity)))
      `(progn
         (defclosure ,action ,vars
           ,@body)
         (defclosure ,run-action ,vars
           ,run-action-doc
           (lambda ()
             (interactive)
             (with-helm-alive-p
               (helm-exit-and-execute-action (,action ,@vars1)))))
         (defclosure ,persistent-action ,vars
           ,persistent-action-doc
           (lambda ()
             (interactive)
             (with-helm-alive-p
               (helm-attrset 'mpd-persistent-action (cons (,action ,@vars1) 'never-split))
               (helm-execute-persistent-action 'mpd-persistent-action)
               (message nil)
               (helm-force-update))))))))

;; ----------------------------------------------------------------
;; Common
;; ----------------------------------------------------------------

(defclosure helm-mpd-refresh (conn)
  "Update MPD library and recalculate helm candidates."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (mpd-update conn)
      (helm-force-update))))

(helm-mpd-defaction goto-top (conn)
  "Go back to the top of `helm-mpd' session."
  (lambda (_ignore)
    (helm-mpd conn)))

(defun helm-mpd-format-time (sec)
  "Format SEC."
  (when sec
    (format-time-string "%M:%S" (list 0 sec 0 0))))

(defun helm-mpd-mode-line (conn)
  "Mode line format for `helm-mpd'."
  `(:eval (let ((song (mpd-get-current-song ,conn))
                (status (mpd-get-status ,conn)))
            (run-with-timer 1 nil 'force-mode-line-update t)
            (list (case (plist-get status 'state)
                    ((play) "Playing")
                    ((pause) "Paused")
                    ((stop) "Stopped"))
                  ": "
                  (funcall helm-mpd-song-format song)
                  " [" (helm-mpd-format-time (plist-get status 'time-elapsed))
                  "/" (helm-mpd-format-time (plist-get status 'time-total))
                  "]"
                  " [" (when (> (plist-get status 'repeat) 0) "r")
                  (when (> (plist-get status 'random) 0) "z")
                  (when (> (string-to-int (plist-get status 'single)) 0) "s")
                  "]"))))

(defclass helm-mpd-source (helm-source-sync)
  ((mpd-conn :initarg :mpd-conn)))

(defun helm-mpd-build-mpd-source (conn name &rest args)
  (apply #'helm-make-source name 'helm-mpd-source
         :mpd-conn conn
         args))

(defun helm-mpd-display-mode-line-ad (source &optional force)
  "Advice for `helm-display-mode-line'."
  (let ((conn0 (assoc 'mpd-conn source)))
    (when conn0
      (let ((conn (cdr conn0)))
        (setq mode-line-format (helm-mpd-mode-line conn))
        (when force
          (force-mode-line-update))))))

(advice-add 'helm-display-mode-line :after 'helm-mpd-display-mode-line-ad)

(defclosure helm-mpd-simple-mpd-action (fun conn)
  "Make a simple MPD action."
  (lambda ()
    (interactive)
    (funcall fun conn)))

(defun helm-mpd-action-map (conn)
  "Keymap for MPD action in `helm-mpd' session."
  (let ((m (make-sparse-keymap)))
    (dolist (v '(("p" . mpd-pause)
                 ("s" . mpd-stop)
                 ("r" . mpd-toggle-repeat)
                 ("z" . mpd-toggle-random)
                 ("y" . mpd-toggle-single)
                 (">" . mpd-next)
                 ("<" . mpd-prev)
                 ("u" . mpd-update)
                 ("c" . mpd-clear-playlist)))
      (define-key m (kbd (car v)) (helm-mpd-simple-mpd-action (cdr v) conn)))
    m))

(defun helm-mpd-map (conn)
  "Parent keymap of all `helm-mpd' keymaps."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("C-c u" . ,(helm-mpd-refresh conn))
                 ("C-c t" . ,(helm-mpd-run-goto-top conn))
                 ("C-c C-c" . ,(helm-mpd-action-map conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

;; ----------------------------------------------------------------
;; Song format
;; ----------------------------------------------------------------

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

(defcustom helm-mpd-song-format
  (lambda (song)
    (mapconcat (lambda (x)
                 (propertize (or (cdr (assq (car x) song)) "") 'face (cdr x)))
               '((Artist . helm-mpd-artist-face)
                 (Title . helm-mpd-title-face)
                 (Album . helm-mpd-album-face))
               " "))
  "Function to format a song."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-format-songs (songs)
  "Format song candidates."
  (mapcar (lambda (song)
            (cons (funcall helm-mpd-song-format song) song))
          songs))

;; ----------------------------------------------------------------
;; (Optional) Tag editor
;; 
;; Available when `emacs-id3.el' <https://github.com/uemurax/emacs-id3> is installed.
;; ----------------------------------------------------------------

(defun helm-mpd-has-tag-editor-p ()
  "Return t if Emacs has an id3 tag editor."
  (and (locate-library "emacs-id3")
       (executable-find "mid3v2")
       t))

(defcustom helm-mpd-tag-edit-buffer-name "*helm-mpd-tag-edit*"
  "Tag editor buffer name."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-library-directory (expand-file-name "~/Music")
  "Local MPD library directory."
  :group 'helm-mpd
  :type 'string)

(eval-when-compile
  (declare-function id3-edit-mode 'emacs-id3))
(defun helm-mpd-spawn-tag-editor (&rest songs)
  "Edit SONGS."
  (switch-to-buffer helm-mpd-tag-edit-buffer-name)
  (erase-buffer)
  (apply #'id3-read-with-mid3v2 t
         (mapcar (lambda (song) (expand-file-name song helm-mpd-library-directory))
                 songs))
  (id3-edit-mode)
  (goto-char (point-min)))

;; ----------------------------------------------------------------
;; Lyrics
;; ----------------------------------------------------------------

(defcustom helm-mpd-lyrics-directory
  (expand-file-name "~/.lyrics")
  "Lyrics directory."
  :group 'helm-mpd
  :type 'string)

(defun helm-mpd-format-lyrics (song)
  "File name for SONG's lyrics."
  (format "%s - %s.txt"
          (plist-get song 'Artist)
          (plist-get song 'Title)))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defclosure helm-mpd-play-song (conn)
  "Play the selected song."
  (lambda (song)
    (mpd-play conn (plist-get song 'Id) t)))

(helm-mpd-defaction delete-songs (conn)
  "Delete selected songs from the current playlist."
  (lambda (_ignore)
    (dolist (song (helm-marked-candidates))
      (mpd-delete conn (plist-get song 'Id) t)
      (message "Delete %s from the current playlist" (plist-get song 'Title)))))

(helm-mpd-defaction swap-songs (conn)
  "Swap two selected songs in the current playlist."
  (lambda (first)
    (let ((cs (helm-marked-candidates))
          (second nil))
      (cond ((= (length cs) 2)
             (setq first (car cs)
                   second (cadr cs)))
            ((and (= (length cs) 1) (not (eq first (car cs))))
             (setq second (car cs))))
      (if second
          (progn
            (mpd-swap conn (list (plist-get first 'Id)) (list (plist-get second 'Id)) t)
            (message "Swap %s and %s" (plist-get first 'Title) (plist-get second 'Title)))
        (message "That action can be performed only with two candidates.")))))

(defclosure helm-mpd-move (conn n)
  "Move the selected song by N."
  (lambda (song)
    (let ((pos (plist-get song 'Pos)))
      (if pos
          (mpd-move conn (list pos) (list (+ pos n)))
        (message "Invalid song.")))))

(defclosure helm-mpd-run-move-down-persistent (conn)
  "Move down the selected song without exiting helm session."
  (lambda (n)
    (interactive "p")
    (with-helm-alive-p
      (helm-attrset 'move-down-action (cons (helm-mpd-move conn n) 'never-split))
      (helm-execute-persistent-action 'move-down-action)
      (message nil)
      (helm-force-update))))

(defclosure helm-mpd-run-move-up-persistent (conn)
  "Move up the selected song without exiting helm session."
  (lambda (n)
    (interactive "p")
    (funcall (helm-mpd-run-move-down-persistent conn) (- n))))

(helm-mpd-defaction edit-songs (conn)
  "Edit selected songs."
  (lambda (_ignore)
    (apply #'helm-mpd-spawn-tag-editor
           (mapcar (lambda (song)
                     (plist-get song 'file))
                   (helm-marked-candidates)))))

(defun helm-mpd-edit-lyrics (song)
  "Edit SONG's lyrics."
  (find-file (expand-file-name (helm-mpd-format-lyrics song)
                               helm-mpd-lyrics-directory)))

(defun helm-mpd-run-edit-lyrics ()
  "Run `helm-mpd-edit-lyrics' from helm session."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-mpd-edit-lyrics)))
(put 'helm-mpd-run-edit-lyrics 'helm-only t)

(defun helm-mpd-current-playlist-actions (conn)
  "Actions for `helm-mpd-current-playlist'."
  (helm-make-actions
   "Play song" (helm-mpd-play-song conn)
   "Delete song(s)" (helm-mpd-delete-songs conn)
   "Swap song(s)" (helm-mpd-swap-songs conn)
   "Move song up" (helm-mpd-move conn -1)
   "Move song down" (helm-mpd-move conn 1)
   (when (helm-mpd-has-tag-editor-p)
     "Edit song(s)")
   (helm-mpd-edit-songs conn)
   "Edit lyrics" 'helm-mpd-edit-lyrics))

(defun helm-mpd-current-playlist-map (conn)
  "Keymap in `helm-mpd-current-playlist'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-delete-songs conn))
                 ("C-c d" . ,(helm-mpd-run-delete-songs-persistent conn))
                 ("M-S" . ,(helm-mpd-run-swap-songs conn))
                 ("C-c s" . ,(helm-mpd-run-swap-songs-persistent conn))
                 ("C-c C-n" . ,(helm-mpd-run-move-down-persistent conn))
                 ("C-c C-p" . ,(helm-mpd-run-move-up-persistent conn))
                 ,@(when (helm-mpd-has-tag-editor-p)
                     (list (cons "M-E" (helm-mpd-run-edit-songs conn))))
                 ("M-L" . helm-mpd-run-edit-lyrics)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defclosure helm-mpd-current-playlist-candidates (conn)
  "Get current playlist."
  (lambda ()
    (helm-mpd-format-songs (mpd-get-playlist-entry conn))))

(defun helm-mpd-show-raw-data (candidates)
  (switch-to-buffer "*helm-mpd-raw-data*")
  (erase-buffer)
  (dolist (c candidates)
    (insert (format "%S" c) "\n")))

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
           (helm-execute-persistent-action 'mpd-persistent-action)
           (helm-force-update))))
      (otherwise
       (lambda ()
         (interactive)
         (with-helm-alive-p
           (helm-exit-and-execute-action g)))))))

(defun helm-mpd-song--match-pattern (p song)
  (let ((mfn (if helm-migemo-mode
                 #'helm-mm-migemo-string-match
               #'string-match)))
    (if (string-match "^%\\(.\\)\\(.*\\)$" p)
        (let ((lead (match-string 1 p))
              (q (match-string 2 p))
              (key nil))
          (cond ((equal lead "f")
                 (setq key 'file))
                ((equal lead "a")
                 (setq key 'Artist))
                ((equal lead "t")
                 (setq key 'Title))
                ((equal lead "b")
                 (setq key 'Album))
                ((equal lead "y")
                 (setq key 'Date))
                ((equal lead "n")
                 (setq key 'Track))
                ((equal lead "g")
                 (setq key 'Genre)))
          (when (and key (assq key song))
            (funcall mfn q (cdr (assq key song)))))
      (cl-loop for v in song
               thereis (funcall mfn p (cdr v))))))

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
                           paths)
                   nil)))

(defclass helm-source-mpd-songs (helm-source)
  ((match :initform '(helm-mpd-songs-match-function))
   (real-to-display :initform 'helm-mpd-display-song)))

(defvar helm-mpd-current-playlist-candidates nil)

(defun helm-mpd-current-playlist-retrieve (&optional host port)
  "Retrieve the current playlist."
  (setq host (or host helm-mpd-host)
        port (or port helm-mpd-port))
  (helm-mpdlib-send host port
                    (helm-mpdlib-make-command 'playlistinfo)
                    (lambda ()
                      (while (helm-mpdlib-received-p)
                        (setq helm-mpd-current-playlist-candidates
                              (helm-mpdlib-read-objects '(file)))))))

(defun helm-mpd-current-playlist-play (song)
  (let ((pos (cdr (assq 'Pos song))))
    (when pos
      (helm-mpd-send (helm-mpdlib-make-command 'play pos)
                     nil))))

(defun helm-mpd-current-playlist-delete (songs)
  (let ((poss (apply #'append
                     (mapcar (lambda (song)
                               (let ((p (assq 'Pos song)))
                                 (when p
                                   (list (string-to-number (cdr p))))))
                             songs))))
    (helm-mpd-send (mapcar (lambda (pos)
                             (helm-mpdlib-make-command 'delete pos))
                           (seq-uniq (seq-sort '> poss)))
                   nil)))

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
            (helm-mpd-send (helm-mpdlib-make-command 'move pos n)
                           nil)))))))
(put 'helm-mpd-current-playlist-move 'helm-only t)

(defvar helm-mpd-current-playlist-actions
  (helm-make-actions
   "Play song" (helm-mpd-action 'helm-mpd-current-playlist-play)
   "Delete song(s)" (helm-mpd-action 'helm-mpd-current-playlist-delete t)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-current-playlist'.")

(defvar helm-mpd-current-playlist-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("M-D" . ,(helm-mpd-action 'helm-mpd-current-playlist-delete t t))
                 ("C-c d" . ,(helm-mpd-action 'helm-mpd-current-playlist-delete t 'persistent))
                 ("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))
                 ("M-g g" . helm-mpd-current-playlist-move)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-current-playlist'.")

(defun helm-mpd-current-playlist-build-source (&optional name &rest args)
  (setq name (or name "Current playlist"))
  (apply #'helm-make-source name 'helm-source-mpd-songs
    :candidates 'helm-mpd-current-playlist-candidates
    :init 'helm-mpd-current-playlist-retrieve
    :action 'helm-mpd-current-playlist-actions
    :keymap helm-mpd-current-playlist-map
    args))

;;;###autoload
(defun helm-mpd-current-playlist (host port)
  "Helm for current playlist."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (helm :sources (helm-mpd-current-playlist-build-source)
          :buffer "*helm-mpd-current-playlist*")))

;; ----------------------------------------------------------------
;; Libraries
;; ----------------------------------------------------------------

(defvar helm-mpd-library-candidates nil)

(defun helm-mpd-library-retrieve (&optional host port)
  "Retrieve the library."
  (setq host (or host helm-mpd-host)
        port (or port helm-mpd-port))
  (helm-mpdlib-send host port
                    (helm-mpdlib-make-command 'listallinfo)
                    (lambda ()
                      (while (helm-mpdlib-received-p)
                        (setq helm-mpd-library-candidates
                              (cl-loop for x in (helm-mpdlib-read-objects '(file directory playlist))
                                       when (assq 'file x)
                                       collect x))))))

(defvar helm-mpd-library-actions
  (helm-make-actions
   "Enqueue song(s)" (helm-mpd-action 'helm-mpd-songs-enqueue t))
  "Actions for `helm-mpd-library'.")

(defun helm-mpd-library-build-source (&optional name &rest args)
  (setq name (or name "Library"))
  (helm-make-source name 'helm-source-mpd-songs
    :candidates 'helm-mpd-library-candidates
    :init 'helm-mpd-library-retrieve
    :action 'helm-mpd-library-actions))

;;;###autoload
(defun helm-mpd-library (host port)
  "Helm for MPD library."
  (interactive (helm-mpd-interactive-host-and-port))
  (helm :sources (helm-mpd-library-build-source)
        :buffer "*helm-mpd-library*"))

;; ----------------------------------------------------------------
;; Play lists
;; ----------------------------------------------------------------

(defclosure helm-mpd-playlist-candidates (conn)
  "Get all playlists."
  (lambda ()
    (let ((ls nil))
      (mpd-get-directory-info conn nil
                              (lambda (obj type)
                                (when (eq type 'playlist)
                                  (push obj ls))))
      ls)))

(defclosure helm-mpd-save-playlist (conn)
  "Save the current playlist."
  (lambda (pname)
    (mpd-save-playlist conn pname)
    (message "Save the current playlist as %s" pname)))

(defclosure helm-mpd-load-playlists (conn)
  "Load selected playlists."
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-load-playlist conn playlists)
      (message "Load playlists %s" playlists))))

(helm-mpd-defaction remove-playlists (conn)
  "Remove selected playlists."
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-remove-playlist conn playlists)
      (message "Remove playlists %s" playlists))))

(defun helm-mpd-new-playlist-actions (conn)
  "Actions for new playlists."
  (helm-make-actions
   "Save current playlist to file" (helm-mpd-save-playlist conn)))

(defun helm-mpd-playlist-actions (conn)
  "Actions for existing playlists."
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-load-playlists conn)
   "Remove playlist(s)" (helm-mpd-remove-playlists conn)))

(defun helm-mpd-playlist-map (conn)
  "Keymap in `helm-mpd-playlist'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-remove-playlists conn))
                 ("C-c d" . ,(helm-mpd-run-remove-playlists-persistent conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-existing-playlist-source (conn)
  "Build sources for existing playlists."
  (helm-mpd-build-mpd-source conn "Playlists"
                             :candidates (helm-mpd-playlist-candidates conn)
                             :action (helm-mpd-playlist-actions conn)
                             :keymap (helm-mpd-playlist-map conn)))

(defun helm-mpd-build-new-playlist-source (conn)
  "Build sources for new playlists."
  (helm-build-dummy-source "Create playlist"
    :action (helm-mpd-new-playlist-actions conn)
    :keymap (helm-mpd-map conn)))

(defun helm-mpd-build-playlist-source (conn)
  "Build sources for `helm-mpd-playlist'."
  (list (helm-mpd-build-existing-playlist-source conn)
        (helm-mpd-build-new-playlist-source conn)))

;;;###autoload
(defun helm-mpd-playlist (conn)
  "Helm for MPD playlists."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-playlist-source conn)
        :buffer "*helm-mpd-playlist*"))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

;;;###autoload
(defun helm-mpd (conn)
  "Helm for MPD.

This is a mixture of `helm-mpd-current-playlist', `helm-mpd-library' and
`helm-mpd-playlist'."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (concatenate 'list
                              (list (helm-mpd-build-current-playlist-source conn))
                              (helm-mpd-build-library-source conn)
                              (helm-mpd-build-playlist-source conn))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
