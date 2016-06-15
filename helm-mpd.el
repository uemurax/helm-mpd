;;; helm-mpd.el --- Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Version: 0.1
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; Package-Requires: ((helm "1"))
;; License: GPL3
;;
;;; Code:

(defgroup helm-mpd ()
  "Predefined configurations for `helm-mpd'."
  :group 'helm)

(defcustom helm-mpd-host "localhost"
  "MPD host."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-port 6600
  "MPD port."
  :group 'helm-mpd
  :type 'number)

(defvar helm-mpd-song-attributes
  '(artist album albumartist comment composer date disc genre performer
           title track time file position mtime mdate))

(defvar helm-mpd-song-tag-begin "<%s>")
(defvar helm-mpd-song-tag-end "</%s>")

(defun helm-mpd-song-format (attrs)
  (mapconcat (lambda (x)
               (concat "["
                       (format helm-mpd-song-tag-begin x)
                       "%" (format "%s" x) "%"
                       (format helm-mpd-song-tag-end x)
                       "]"))
             attrs
             ""))

(defun helm-mpd-mpc-command (&rest args)
  `("mpc"
    "-h" ,helm-mpd-host
    "-p" ,(format "%s" helm-mpd-port)
    ,@args))

;;;; Collect songs

(defun helm-mpd-parse-song (song-string)
  (do ((res nil res)
       (attrs helm-mpd-song-attributes (cdr attrs)))
      ((null attrs) res)
    (let ((attr (car attrs)))
      (when (string-match (concat (format helm-mpd-song-tag-begin attr)
                                  "\\(.*\\)"
                                  (format helm-mpd-song-tag-end attr))
                          song-string)
        (setq res (cons `(,attr . ,(match-string 1 song-string))
                        res))))))

(defun helm-mpd-display-song-default (song)
  (concat (cdr (assq 'artist song))
          " "
          (cdr (assq 'title song))
          " "
          (cdr (assq 'album song))))

(defcustom helm-mpd-display-song-function
  'helm-mpd-display-song-default
  "Function displaying a song."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-songs-candidate-transformer (candidates)
  (mapcar (lambda (c)
            (let ((song (helm-mpd-parse-song c)))
              (cons (propertize (funcall helm-mpd-display-song-function
                                         song)
                                'mpd-song song)
                    song)))
          candidates))

(defun helm-mpd-songs-candidates-process ()
  (let* ((source (helm-get-current-source))
         (args (cdr (assq 'mpd-args source)))
         (filter (cdr (assq 'mpd-filter source)))
         (attrs (cdr (assq 'mpd-attrs source))))
    (make-process :name "helm-mpd-song-process"
                  :command (list "sh" "-c"
                                 (mapconcat 'identity
                                            (append (helm-mpd-mpc-command
                                                     "-f" (format "'%s'" (helm-mpd-song-format attrs))
                                                     (funcall args helm-pattern))
                                                    `("|" ,(funcall filter helm-pattern)))
                                            " ")))))

(defun helm-mpd-search-arguments (pattern)
  (format "search any %S"
          pattern))

(defun helm-mpd-current-playlist-arguments (pattern)
  "playlist")

(defun helm-mpd-current-playlist-filter (pattern)
  (format "grep -i %S"
          (mapconcat (lambda (attr)
                       (concat (format helm-mpd-song-tag-begin attr)
                               (format ".*%s.*" pattern)
                               (format helm-mpd-song-tag-end attr)))
                     helm-mpd-song-attributes
                     "\\|")))

;;;; Actions

(defcustom helm-mpd-songs-action
  (helm-make-actions
   "Show song(s) information" 'helm-mpd-song-show)
  "Default action on songs."
  :group 'helm-mpd
  :type 'alist)

(defun helm-mpd-songs-action-transformer (actions song)
  (let ((add nil))
    (when (assq 'file song)
      (setq add (append '(("Add song(s)" . helm-mpd-song-add))
                        add)))
    (when (assq 'position song)
      (setq add (append '(("Play song" . helm-mpd-song-play)
                          ("Delete song(s)" . helm-mpd-song-del))
                        add)))
    (append actions add)))

(defvar helm-mpd--info-buffer "*helm-mpd-info*")

(defun helm-mpd-song-show (_ignore)
  (let ((buf (get-buffer-create helm-mpd--info-buffer)))
    (display-buffer buf)
    (with-current-buffer buf
      (view-mode)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (goto-char (point-min))
        (mapc (lambda (c)
                (insert (format "%S" c)))
              (helm-marked-candidates))))))

(defun helm-mpd-song-add (_ignore)
  (let ((proc (make-process :name "mpc-add"
                            :command (helm-mpd-mpc-command "add"))))
    (process-send-string proc
                         (apply #'concat
                                (mapcar (lambda (c)
                                          (let ((x (assq 'file c)))
                                            (when x
                                              (concat (cdr x) "\n"))))
                                        (helm-marked-candidates))))
    (process-send-eof proc)))

(defun helm-mpd-song-del (_ignore)
  (let ((proc (make-process :name "mpc-del"
                            :command (helm-mpd-mpc-command "del"))))
    (process-send-string proc
                         (apply #'concat
                                (mapcar (lambda (c)
                                          (let ((x (assq 'position c)))
                                            (when x
                                              (concat (cdr x) "\n"))))
                                        (helm-marked-candidates))))
    (process-send-eof proc)))

(defun helm-mpd-song-play (song)
  (make-process :name "mpc-play"
                :command (helm-mpd-mpc-command "play"
                                               (cdr (assq 'position song)))))

;;;; Entry point

(defclass helm-source-mpd-songs (helm-source-async)
  ((candidates-process :initform 'helm-mpd-songs-candidates-process)
   (candidate-transformer :initform '(helm-mpd-songs-candidate-transformer))
   (action :initform 'helm-mpd-songs-action)
   (action-transformer :initform '(helm-mpd-songs-action-transformer))
   (mpd-args :initarg :mpd-args)
   (mpd-filter :initarg :mpd-filter
               :initform (lambda (pattern) "cat"))
   (mpd-attrs :initarg :mpd-attrs
              :initform (symbol-value 'helm-mpd-song-attributes))))

;;;###autoload
(defun helm-mpd (&optional host port)
  (interactive (if current-prefix-arg
                   (list (read-string "Host: " nil nil helm-mpd-host)
                         (read-number "Port: " helm-mpd-port))
                 (list helm-mpd-host helm-mpd-port)))
  (setq helm-mpd-host host
        helm-mpd-port port)
  (helm :sources (mapcar (lambda (xs)
                           (apply (lambda (name &rest kw)
                                    (apply #'helm-make-source name 'helm-source-mpd-songs
                                           kw))
                                  xs))
                         `(("Current playlist"
                            :mpd-args helm-mpd-current-playlist-arguments
                            :mpd-filter helm-mpd-current-playlist-filter)
                           ("MPD songs"
                            :mpd-args helm-mpd-search-arguments
                            :mpd-attrs ,(remove 'position helm-mpd-song-attributes))))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
