;;; guid.el --- Simple GUID/UUID generator/updator

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/Emacs-guid/raw/master/guid.el
;; Emacs: GNU Emacs 24 or later
;; Version: 0.9.1
;; Package-Requires: ((emacs "24") (cl-lib "0.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; guid.el provides Simple GUID/UUID generator/updator.

;; ## Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'guid)

;; ## Usage:

;; * This guid package currently support guid and only uuid version 4
;;   defined at rfc4122 Section 4.4

;; * `guid-update-*' function is not simply replace GUID.
;;   These function keep identity if duplicated GUID exists
;;   while a one command.

;; * Update all GUID string in selected buffer.
;;
;;     M-x guid-update-buffer

;; * Update all GUID string in selected directory recursively.
;;
;;     M-x guid-update-directory

;; * Update all GUID string in selected file.
;;
;;     M-x guid-update-file

;; * Generate GUID string
;;
;;     M-x guid-generate-string
;;
;; => Now `kill-ring' has generated GUID.

;;; TODO:

;; * see rfc4122.txt current implementation is not enough.

;;    Field                  Data Type     Octet  Note
;;                                         #

;;    time_low               unsigned 32   0-3    The low field of the
;;                           bit integer          timestamp

;;    time_mid               unsigned 16   4-5    The middle field of the
;;                           bit integer          timestamp

;;    time_hi_and_version    unsigned 16   6-7    The high field of the
;;                           bit integer          timestamp multiplexed
;;                                                with the version number

;;    clock_seq_hi_and_rese  unsigned 8    8      The high field of the
;;    rved                   bit integer          clock sequence
;;                                                multiplexed with the
;;                                                variant

;;    clock_seq_low          unsigned 8    9      The low field of the
;;                           bit integer          clock sequence

;;    node                   unsigned 48   10-15  The spatially unique
;;                           bit integer          node identifier

;; * update GUID filename in `guid-update-directory'

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (defun guid--map-generator (proc)
    "PROC accept one arg"
    (mapcar proc '(4 2 2 2 6))))

(defcustom guid-generate-default-algorithm 'guid
  "To decide GUID/UUID generation algorithm.

`guid' completely random uuid (default)
`uuid-4' uuid version 4
"
  :group 'guid
  :type '(choice
          ;; Microsoft implementation
          ;; Strictly speaking, this is not `uuid' but for convenient.
          (const guid)
          ;; 4.4.  Algorithms for Creating a UUID from Truly Random or
          ;;       Pseudo-Random Numbers
          (const uuid-4)))

(defconst guid--string-format
  (eval-when-compile
    (mapconcat
     'identity
     (guid--map-generator
      (lambda (n) (mapconcat 'identity (make-list n "%02x") "")))
     "-"))
  "`format' string to serialize GUID/UUID data.")

(defconst guid--string-regexp
  (eval-when-compile
    (mapconcat
     'identity
     (guid--map-generator
      (lambda (n) (format "[a-fA-F0-9]\\{%d\\}" (* n 2))))
     "-"))
  "Regexp that match to GUID/UUID.")

;; for grep
(defconst guid--traditional-regexp
  (eval-when-compile
    (mapconcat
     'identity
     (guid--map-generator
      (lambda (n) (mapconcat 'identity (make-list (* n 2) "[a-fA-F0-9]") "")))
     "-"))
  "Regexp that works in any of regexp engine.")

(defun guid-rassoc (key alist)
  (cl-loop with searching = (upcase key)
           for kv in alist
           if (string= (upcase (cdr kv)) searching)
           return kv))

(defun guid--mask-result (16bytes &optional algorithm)
  (cl-ecase (or algorithm guid-generate-default-algorithm)
    (guid 16bytes)
    ((uuid-4)
     ;; Set the two most significant bits (bits 6 and 7) of the
     ;; clock_seq_hi_and_reserved to zero and one, respectively.
     (aset 16bytes 8 (logand ?\xbf (aref 16bytes 8)))
     ;; Set the four most significant bits (bits 12 through 15) of the
     ;; time_hi_and_version field to the 4-bit version number from
     ;; Section 4.1.3.
     (aset 16bytes 6 (logior ?\x40 (logand ?\x0f (aref 16bytes 6))))))
  16bytes)

;;;###autoload
(defun guid-string-p (string)
  "Check STRING satisfy GUID/UUID format."
  (let ((regexp (concat "\\`" guid--string-regexp "\\'")))
    (and (string-match regexp string) t)))

;;;###autoload
(defun guid-generate (&optional algorithm)
  "Create new uuid as vector."
  (cl-loop with v = (make-vector 16 nil)
           with nbits = (cl-loop with acc = -1
                                 for i from 0
                                 until (zerop acc)
                                 do (setq acc (lsh acc 1))
                                 finally return (1- i))
           with lim = t
           for i from 0
           for ignore across  v
           do (aset v i (random ?\x100))
           finally return (guid--mask-result v algorithm)))

;;;###autoload
(defun guid-generate-string (&optional upcase algorithm)
  "Create new uuid string.
Copy that GUID/UUID if interactive call."
  (interactive "P")
  (let* ((vec (guid-generate algorithm))
         (args (append vec nil))
         (string (apply 'format guid--string-format args)))
    (when upcase
      (setq string (upcase string)))
    (when (called-interactively-p 'interactive)
      (kill-new string)
      (message "Generated GUID/UUID: %s" string))
    string))

;;;###autoload
(defun guid-update-buffer (&optional buffer algorithm done-alist)
  "Update all uuid in BUFFER.
This uuid must match to `guid--string-regexp' with word boundary.
Preserve identity if there is duplicate GUID/UUID. (Indicate same GUID/UUID)
This function return alist which key is previous uuid, value is new uuid.

\(fn &optional buffer algorithm)"
  (interactive "bBuffer to update GUID/UUID: ")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((regexp (concat "\\b" guid--string-regexp "\\b")))
          (while (re-search-forward regexp nil t)
            (let* ((old (match-string 0))
                   (upper-p (string= (upcase old) old))
                   (tmp (assoc-string old done-alist t))
                   (already-done (guid-rassoc old done-alist)))
              (cond
               (tmp
                (let ((new (cdr tmp)))
                  (replace-match (if upper-p (upcase new) new))))
               ((not already-done)
                (let ((new (save-match-data
                             (guid-generate-string upper-p algorithm))))
                  (replace-match new)
                  (setq done-alist (cons (cons old new) done-alist)))))))
          done-alist)))))

;;;###autoload
(defun guid-update-file (file &optional algorithm done-alist no-msg)
  "Update FILE uuid and save it.
Other optional args are programmable interface.
This function return alist which key is previous uuid, value is new uuid.

\(fn FILE &optional algorithm)"
  (interactive "fFile to update GUID/UUID: ")
  (with-temp-buffer
    (let ((coding-system-for-read 'binary))
      (insert-file-contents file))
    (set-buffer-modified-p nil)
    (setq done-alist (guid-update-buffer nil algorithm done-alist))
    (when (buffer-modified-p)
      (write-region nil nil file nil (and no-msg 'no-msg))))
  done-alist)

(defun guid--update-directory-0 (directory &optional algorithm done-alist no-msg)
  (dolist (file (directory-files directory t))
    (cond
     ((member (file-name-nondirectory file) '("." "..")))
     ((file-directory-p file)
      (setq done-alist (guid--update-directory-0 file algorithm done-alist no-msg)))
     (t
      (setq done-alist (guid-update-file file algorithm done-alist no-msg)))))
  done-alist)

;;;###autoload
(defun guid-update-directory (directory &optional algorithm no-msg)
  "Update uuid in file under DIRECTORY recursively.
Other optional args are programmable interface.
This function return alist which key is previous uuid, value is new uuid.

\(fn DIRECTORY &optional algorithm)"
  (interactive "DDirectory to update GUID/UUID: ")
  (guid--update-directory-0 directory algorithm nil no-msg))

(defalias 'guid-update-all 'guid-update-buffer)

(provide 'guid)

;;; guid.el ends here
