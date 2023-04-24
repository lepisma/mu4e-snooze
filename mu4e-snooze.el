;;; mu4e-snooze.el --- Snooze feature for mu4e -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27"))
;; Keywords: 
;; URL: https://github.com/lepisma/mu4e-snooze

;;; Commentary:

;; Snooze feature for mu4e
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'calendar)
(require 'mu4e)
(require 'mu4e-meta)

(defcustom mu4e-snooze-label "[mu4e]/snoozed"
  "Label to use for filing snoozed emails."
  :type 'string)

(defun mu4e-snooze--create-label ()
  "Create snooze label in the server.")

(defun mu4e-snooze ()
  "Snooze a message until the selected date."
  (interactive)
  (let* ((snooze-date (calendar-read-date))
         (snooze-time (encode-time 0 0 0 (nth 1 snooze-date) (nth 0 snooze-date) (nth 2 snooze-date)))
         (snooze-date-str (format-time-string "%Y%m%d" snooze-time))
         (snooze-folder (mu4e-get-maildir-mu4e mu4e-snooze-label)))
    (mu4e-action-mark-set 'refile (concat snooze-folder "/" snooze-date-str))
    (mu4e-mark-execute-all t)))

(defun mu4e-snooze-process-snoozed ()
  "Move snoozed messages back to their original folders after the
snooze period and update the mu database."
  (interactive)
  (let ((database-updated nil))
    (dolist (file (directory-files-recursively (concat (mu4e-root-maildir) mu4e-snooze-label) "^[0-9].*"))
      (let* ((snooze-date (file-name-nondirectory file))
             (current-date (format-time-string "%Y%m%d")))
        (when (string< snooze-date current-date)
          (let* ((msg (mu4e-message (concat "file://" file)))
                 (msgid (mu4e-message-field msg :message-id))
                 (original-folder (shell-command-to-string (format "%s find --fields='l' 'msgid:%s'" mu4e-mu-binary msgid))))
            (rename-file file (concat (mu4e-root-maildir) (string-trim original-folder)))
            (setq database-updated t)))))
    (when database-updated
      (mu4e-update-index))))

(define-key mu4e-headers-mode-map (kbd "z") 'mu4e-snooze)
(run-at-time nil (* 5 60) 'mu4e-process-snoozed-messages)


(provide 'mu4e-snooze)

;;; mu4e-snooze.el ends here
