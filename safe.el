;;; safe.el --- Open large/minified files safely in Emacs. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Yevgnen Koh
;;
;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Version: 0.1.0
;; Keywords: large file, minified file
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; References:
;; https://ztlevi.github.io/posts/A-simple-tweak-to-help-you-edit-minified-files-in-Emacs/
;;
;; See documentation on https://github.com/Yevgnen/safe.el.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defgroup safe nil
  "Open large/minified files safely in Emacs.")

(defcustom safe-source-file-warning-threshold (* 0.5 1024 1024)
  "Maximum size of a source file above which a confirmation is requested."
  :type 'float)

(defvar safe-archive-file-regexp-list
  '("zip" "gz" "tar.gz" "7z" "rar" "tar" "bz" "bz2" "epub"
    "docx?" "xlsx?" "pptx?"))

(defvar safe-archive-file-regexp
  (format "\\.\\(%s\\)$" (mapconcat 'identity safe-archive-file-regexp-list "\\|")))

(defvar-local safe-local--enabled-p nil)

;;;###autoload
(defun safe-file-size (filename)
  (or (nth 7 (file-attributes filename)) 0))

;;;###autoload
(defun safe-buffer-line-length (n)
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

;;;###autoload
(defun safe-archive-file-p (filename)
  (string-match safe-archive-file-regexp filename))

;;;###autoload
(defun safe-prog-buffer-p (buffer)
  (with-current-buffer buffer
    (and (derived-mode-p 'prog-mode)
         (not (eq major-mode 'json-mode)))))

;;;###autoload
(defun safe-buffer-too-large-p (buffer)
  (> (buffer-size buffer)
     large-file-warning-threshold))

;;;###autoload
(defun safe-file-too-large-p (filename)
  (> (safe-file-size filename)
     large-file-warning-threshold))

;;;###autoload
(defun safe-buffer-large-p (buffer)
  (let ((filename (buffer-file-name buffer)))
    (and (not (and filename
                   (safe-archive-file-p filename)))
         (safe-buffer-too-large-p buffer))))

;;;###autoload
(defun safe-file-large-p (filename)
  (and (not (safe-archive-file-p filename))
       (safe-file-too-large-p filename)))

;;;###autoload
(defun safe-prog-buffer-large-p (buffer)
  (and (safe-prog-buffer-p buffer)
       (safe-buffer-large-p buffer)))

;;;###autoload
(defun safe-buffer-minified-p (buffer &optional try-lines max-wdith)
  (with-current-buffer buffer
    (cl-some (lambda (n)
               (ignore-errors (> (safe-buffer-line-length n)
                                 (or max-wdith 1000))))
             (number-sequence 1 (or try-lines 10)))))

;;;###autoload
(defun safe-setup ()
  (setq-local safe-local--enabled-p t)
  (view-mode 1)
  (setq-local show-paren-mode nil)
  (if (featurep 'anzu) (anzu-mode -1))
  (if (featurep 'pangu-spacing) (pangu-spacing-mode -1))
  (buffer-disable-undo))

;;;###autoload
(defun safe-fundamental-mode ()
  (interactive)
  (let ((after-change-major-mode-hook (append after-change-major-mode-hook '(safe-setup))))
    (fundamental-mode))
  (setq-local mode-name "Fundamental[safe]"))

;;;###autoload
(defun safe-local-enabled-p ()
  safe-local--enabled-p)

;;;###autoload
(defun safe-danger-p ()
  (let ((buffer (current-buffer)))
    (or (safe-buffer-large-p buffer)
        (safe-buffer-minified-p buffer)
        (and buffer-file-name
             (safe-file-large-p buffer-file-name)))))

;;;###autoload
(define-minor-mode safe-mode
  "Minor mode for large file setups."
  :global t
  (if safe-mode
      (cl-pushnew (cons #'safe-danger-p #'safe-fundamental-mode) magic-mode-alist :test #'equal)
    (setq magic-mode-alist (cl-remove (cons #'safe-danger-p #'safe-fundamental-mode) magic-mode-alist :test #'equal))))

(provide 'safe)

;;; safe.el ends here
