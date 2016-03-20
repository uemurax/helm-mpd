;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-21 02:24:00 tuemura>
;;
;;; Code:

(require 'helm)
(require 'helm-mpdlib)
(eval-when-compile (require 'cl))

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

(cl-defun helm-mpd-send (str &optional callback cbarg
                             &key (output-buffer "*helm-mpd-default-output*"))
  "Run `helm-mpdlib-send' with `helm-mpd-host' and `helm-mpd-port'."
  (helm-mpdlib-send helm-mpd-host helm-mpd-port
                    str callback cbarg :output-buffer output-buffer))

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

(defun helm-mpd-show-raw-data (candidates)
  (switch-to-buffer "*helm-mpd-raw-data*")
  (erase-buffer)
  (dolist (c candidates)
    (insert (format "%S" c) "\n")))

;; ----------------------------------------------------------------
;; Songs
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

(defclass helm-source-mpd-songs (helm-source)
  ((match :initform '(helm-mpd-songs-match-function))
   (real-to-display :initform 'helm-mpd-display-song)))

(defun helm-mpd-songs-enqueue (songs)
  (let ((paths (apply #'append
                      (mapcar (lambda (song)
                                (when (assq 'file song)
                                  (list (cdr (assq 'file song)))))
                              songs))))
    (helm-mpd-send (mapcar (lambda (path)
                             (helm-mpdlib-make-command 'add path))
                           paths))))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defvar helm-mpd-current-playlist-candidates (cons nil nil))
(defvar helm-mpd-current-playlist-source nil)

(defun helm-mpd-current-playlist-retrieve ()
  "Retrieve the current playlist."
  (let ((buf "*helm-mpd-current-playlist-output*"))
    (helm-mpd-send (helm-mpdlib-make-command 'playlistinfo)
                   (lambda ()
                     (while (helm-mpdlib-received-p)
                       (setq helm-mpd-current-playlist-candidates
                             (cons t (helm-mpdlib-read-objects '(file)))))
                     (with-helm-buffer
                       (helm-update nil helm-mpd-current-playlist-source)))
                   nil :output-buffer buf)))

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
         :candidates (lambda ()
                       (if (car helm-mpd-current-playlist-candidates)
                           (setcar helm-mpd-current-playlist-candidates nil)
                         (helm-mpd-current-playlist-retrieve))
                       (cdr helm-mpd-current-playlist-candidates))
         :action 'helm-mpd-current-playlist-actions
         :keymap helm-mpd-current-playlist-map
         args))

;;;###autoload
(defun helm-mpd-current-playlist (host port)
  "Helm for current playlist."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port)
        (src (helm-mpd-current-playlist-build-source)))
    (helm :sources (list src)
          :buffer "*helm-mpd-current-playlist*"
          :mpd-current-playlist-source src)))

;; ----------------------------------------------------------------
;; Libraries
;; ----------------------------------------------------------------

(defvar helm-mpd-library-candidates (cons nil nil))
(defvar helm-mpd-library-source nil)
(defun helm-mpd-library-retrieve ()
  "Retrieve the library."
  (let ((buf "*helm-mpd-library-output*"))
    (helm-mpd-send (helm-mpdlib-make-command 'listallinfo)
                   (lambda ()
                     (while (helm-mpdlib-received-p)
                       (setq helm-mpd-library-candidates
                             (cons t
                                   (cl-loop for x in (helm-mpdlib-read-objects '(file directory playlist))
                                            when (assq 'file x)
                                            collect x))))
                     (with-helm-buffer
                       (helm-update nil helm-mpd-library-source)))
                   nil :output-buffer buf)))

(defvar helm-mpd-library-actions
  (helm-make-actions
   "Enqueue song(s)" (helm-mpd-action 'helm-mpd-songs-enqueue t)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-library'.")

(defvar helm-mpd-library-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-library'.")

(defun helm-mpd-library-build-source (&optional name &rest args)
  (setq name (or name "Library"))
  (apply #'helm-make-source name 'helm-source-mpd-songs
         :candidates (lambda ()
                       (if (car helm-mpd-library-candidates)
                           (setcar helm-mpd-library-candidates nil)
                         (helm-mpd-library-retrieve))
                       (cdr helm-mpd-library-candidates))
         :action 'helm-mpd-library-actions
         :keymap helm-mpd-library-map
         args))

;;;###autoload
(defun helm-mpd-library (host port)
  "Helm for MPD library."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port)
        (src (helm-mpd-library-build-source)))
    (helm :sources (list src)
          :buffer "*helm-mpd-library*"
          :mpd-library-source src)))

;; ----------------------------------------------------------------
;; Play lists
;; ----------------------------------------------------------------

(defvar helm-mpd-playlist-candidates (cons nil nil))
(defvar helm-mpd-playlist-source nil)

(defun helm-mpd-playlist-retrieve ()
  (let ((buf "*helm-mpd-playlist-output*"))
    (helm-mpd-send (helm-mpdlib-make-command 'listplaylists)
                   (lambda ()
                     (while (helm-mpdlib-received-p)
                       (setq helm-mpd-playlist-candidates
                             (cons t
                                   (cl-loop for x in (helm-mpdlib-read-objects '(playlist))
                                            when (assq 'playlist x)
                                            collect x))))
                     (with-helm-buffer
                       (helm-update nil helm-mpd-playlist-source)))
                   nil :output-buffer buf)))

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

(defvar helm-mpd-playlist-actions
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-action 'helm-mpd-playlist-load t)
   "Remove playlist(s)" (helm-mpd-action 'helm-mpd-playlist-remove t)
   "Rename playlist" (helm-mpd-action 'helm-mpd-playlist-rename)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-playlist'.")

(defvar helm-mpd-playlist-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("M-D" . ,(helm-mpd-action 'helm-mpd-playlist-remove t t))
                 ("C-c d" . ,(helm-mpd-action 'helm-mpd-playlist-remove t 'persistent))
                 ("M-R" . ,(helm-mpd-action 'helm-mpd-playlist-rename t t))
                 ("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-playlist'.")

(defun helm-mpd-playlist-build-source (&optional name &rest args)
  (setq name (or name "Playlists"))
  (apply #'helm-make-source name 'helm-source
         :real-to-display (lambda (c)
                            (cdr (assq 'playlist c)))
         :candidates (lambda ()
                       (if (car helm-mpd-playlist-candidates)
                           (setcar helm-mpd-playlist-candidates nil)
                         (helm-mpd-playlist-retrieve))
                       (cdr helm-mpd-playlist-candidates))
         :action 'helm-mpd-playlist-actions
         :keymap helm-mpd-playlist-map
         args))

;;;###autoload
(defun helm-mpd-playlist (host port)
  "Helm for MPD playlists."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port)
        (src (helm-mpd-playlist-build-source)))
    (helm :sources (list src)
          :buffer "*helm-mpd-playlist*"
          :mpd-playlist-source src)))

(defun helm-mpd-new-playlist-save (name)
  (helm-mpd-send (helm-mpdlib-make-command 'save name)))

(defvar helm-mpd-new-playlist-actions
  (helm-make-actions
   "Save the current playlist" (helm-mpd-action 'helm-mpd-new-playlist-save))
  "Actions for `helm-mpd-new-playlist'.")

(defun helm-mpd-new-playlist-build-source (&optional name &rest args)
  (setq name (or name "Create playlist"))
  (apply #'helm-make-source name 'helm-source-dummy
         :action 'helm-mpd-new-playlist-actions
         args))

;;;###autoload
(defun helm-mpd-new-playlist (host port)
  "Helm for new MPD playlist."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (helm :sources (helm-mpd-new-playlist-build-source)
          :buffer "*helm-mpd-new-playlist*")))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

;;;###autoload
(defun helm-mpd (host port)
  "Helm for MPD.

This is a mixture of `helm-mpd-current-playlist', `helm-mpd-library',
`helm-mpd-playlist' and `helm-mpd-new-playlist'."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port)
        (cur-pl-src (helm-mpd-current-playlist-build-source))
        (lib-src (helm-mpd-library-build-source))
        (pl-src (helm-mpd-playlist-build-source)))
    (helm :sources (list cur-pl-src
                         lib-src
                         pl-src
                         (helm-mpd-new-playlist-build-source))
          :buffer "*helm-mpd*"
          :mpd-current-playlist-source cur-pl-src
          :mpd-library-source lib-src
          :mpd-playlist-source pl-src)))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
