;;; helm-mpd.el --- Helm interface for MPD
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

(defvar helm-mpd-tag-begin "<%s>")
(defvar helm-mpd-tag-end "</%s>")
(defvar helm-mpd-item-keywords '("file" "directory" "playlist"))

(defun helm-mpd-process-name (cmd)
  (format "helm-mpd-process:%s" cmd))

(defun helm-mpd-make-process (cmd &optional output-file output-proc)
  (let* ((proc-args (append (list :name (helm-mpd-process-name cmd)
                                  :host helm-mpd-host
                                  :service helm-mpd-port)
                            (when output-file
                              (list :buffer (let ((b (get-buffer-create (format "helm-mpd-temporary-output:%s" cmd))))
                                              (with-current-buffer b
                                                (erase-buffer))
                                              b)
                                    :filter 'helm-mpd-process-filter
                                    :sentinel 'helm-mpd-process-sentinel))))
         (proc (apply #'make-network-process proc-args)))
    (process-put proc 'helm-mpd-output-file output-file)
    (process-put proc 'helm-mpd-output-process output-proc)
    (process-send-string proc (concat cmd "\n"))
    (process-send-eof proc)
    proc))

(defun helm-mpd-process-sentinel (proc string)
  (cond ((string-match "^\\(connection broken\\|deleted\\|finished\\|killed\\|exited\\|failed\\)" string)
         (let ((out-proc (process-get proc 'helm-mpd-output-process)))
           (when (process-live-p out-proc)
             (process-send-eof out-proc))))))

(defun helm-mpd-process-filter (proc string)
  (let ((buf (process-buffer proc))
        (file (process-get proc 'helm-mpd-output-file))
        (out-proc (process-get proc 'helm-mpd-output-process)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (insert string))
      (helm-mpd-parse-response file out-proc))))

(defun helm-mpd-parse-response (output-file &optional proc)
  (let ((buf (generate-new-buffer "helm-mpd-temporary-buffer")))
    (unwind-protect
        (progn
          (while (search-forward-regexp "^\\([^:\n]*\\): \\([^\n]*\\)\n" nil t)
            (let ((key (match-string 1))
                  (value (match-string 2)))
              (with-current-buffer buf
                (when (seq-position helm-mpd-item-keywords key 'equal)
                  (insert "\n"))
                (insert (format helm-mpd-tag-begin key)
                        value
                        (format helm-mpd-tag-end key)))))
          (mkdir (file-name-directory output-file) t)
          (with-temp-file output-file
            (when (file-exists-p output-file)
              (forward-char (nth 1 (insert-file-contents output-file))))
            (insert-buffer-substring buf))
          (when (process-live-p proc)
            (with-current-buffer buf
              (process-send-region proc (point-min) (point-max)))))
      (kill-buffer buf))))

(defun helm-mpd-parse-object (object-string)
  (let ((res nil)
        (tag "\\([^/<>]*\\)"))
    (with-temp-buffer
      (insert object-string)
      (goto-char (point-min))
      (while (search-forward-regexp (concat (format helm-mpd-tag-begin tag)
                                            "\\([^]*\\)"
                                            (format helm-mpd-tag-end tag))
                                    nil t)
        (setq res (cons `(,(intern (match-string 1)) . ,(match-string 2))
                        res)))
      res)))

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

(defun helm-mpd-filtered-candidate-transformer (candidates source)
  (mapcar (lambda (c)
            (let ((object (helm-mpd-parse-object c)))
              (cons (propertize (funcall helm-mpd-display-object-function
                                         object)
                                'mpd-object object)
                    object)))
          (seq-filter (lambda (c)
                        (> (length c) 0))
                      candidates)))

(defvar helm-mpd--info-buffer "*helm-mpd-info*")

(defun helm-mpd-object-show (_ignore)
  (let ((buf (get-buffer-create helm-mpd--info-buffer)))
    (display-buffer buf)
    (with-current-buffer buf
      (view-mode)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (format "%S" (helm-marked-candidates)))))))

(defcustom helm-mpd-object-action
  (helm-make-actions
   "Show object(s) information" 'helm-mpd-object-show)
  "Default action on objects."
  :group 'helm-mpd
  :type 'alist)

(defun helm-mpd-action-play (object)
  (helm-mpd-make-process (format "playid %s"
                                 (cdr (assq 'Id object)))))

(defun helm-mpd-action-delete (_ignore)
  (helm-mpd-make-process (concat "command_list_begin\n"
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

(defun helm-mpd-action-add (_ignore)
  (helm-mpd-make-process (concat "command_list_begin\n"
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

(defun helm-mpd-action-load (_ignore)
  (helm-mpd-make-process (concat "command_list_begin\n"
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

(defvar helm-mpd-tmp-directory
  (expand-file-name "tmp" (file-name-directory load-file-name)))

(defun helm-mpd-candidates-file (cmd)
  (expand-file-name (format "helm-mpd-candidates-%s" cmd)
                    helm-mpd-tmp-directory))

(defun helm-mpd-pattern-to-regexp (ptn)
  (let ((tag "[^/<>]*"))
    (when (string-match "^<\\([^<>]*\\)>\\(.*\\)$" ptn)
      (setq tag (match-string 1 ptn))
      (setq ptn (match-string 2 ptn)))
    (when (and (boundp 'helm-migemo-mode) helm-migemo-mode)
      (setq ptn
            (car (split-string
                  (replace-regexp-in-string "\\([()|]\\)" "\\\\\\1"
                                            (shell-command-to-string
                                             (mapconcat 'identity
                                                        (list migemo-command
                                                              "-d" migemo-dictionary
                                                              "-q" "-w" ptn)
                                                        " ")))
                  "\n"))))
    (concat (format helm-mpd-tag-begin tag)
            "[^]*\\(" ptn "\\)[^]*"
            (format helm-mpd-tag-end tag))))

(defun helm-mpd-candidates-process ()
  (let* ((cmd (helm-attr 'mpd-command))
         (file (helm-mpd-candidates-file cmd))
         (fex (file-exists-p file))
         (proc (make-process :name (format "helm-mpd-candidates-process:%s" cmd)
                             :command (list "sh" "-c"
                                            (mapconcat 'identity
                                                       (cons (concat "cat "
                                                                     (if fex file "-"))
                                                             (mapcar (lambda (ptn)
                                                                       (format "grep -i '%s'"
                                                                               (helm-mpd-pattern-to-regexp ptn)))
                                                                     (split-string helm-pattern)))
                                                       " | ")))))
    (unless fex
      (helm-mpd-make-process cmd file proc))
    proc))

(defclass helm-source-mpd-base (helm-source-async)
  ((filtered-candidate-transformer :initform '(helm-mpd-filtered-candidate-transformer))
   (candidates-process :initform 'helm-mpd-candidates-process)
   (action :initform 'helm-mpd-object-action)
   (action-transformer :initform '(helm-mpd-action-transformer-current-playlist
                                   helm-mpd-action-transformer-song
                                   helm-mpd-action-transformer-playlist))
   (update :initform (lambda ()
                       (let ((file (helm-mpd-candidates-file (helm-attr 'mpd-command))))
                         (when (file-exists-p file)
                           (delete-file file)))))
   (mpd-command :initarg :mpd-command)))

(defvar helm-source-mpd-current-playlist
  (helm-make-source "Current playlist" 'helm-source-mpd-base
    :mpd-command "playlistinfo"))
(defvar helm-source-mpd-songs
  (helm-make-source "Songs" 'helm-source-mpd-base
    :mpd-command "listallinfo"))
(defvar helm-source-mpd-playlists
  (helm-make-source "Playlists" 'helm-source-mpd-base
    :mpd-command "listplaylists"))

(defvar helm-source-mpd
  '(helm-source-mpd-current-playlist
    helm-source-mpd-songs
    helm-source-mpd-playlists))

;;;###autoload
(defun helm-mpd ()
  (interactive)
  (helm :sources helm-source-mpd
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
