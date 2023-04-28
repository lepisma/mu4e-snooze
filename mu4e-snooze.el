;;; mu4e-snooze.el --- Snooze feature for mu4e -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28"))
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

(require 'org)
(require 'mu4e)

(defcustom mu4e-snooze-label "mu4e-snoozed"
  "Label to use for filing snoozed emails."
  :type 'string)

(defcustom mu4e-snooze-unsnooze-timer (* 5 60)
  "Number of seconds to use for unsoozing timer."
  :type 'int)

(defvar mu4e-snooze-unsnooze-label "INBOX"
  "Label to move snoozed items back to.")

(defvar mu4e-snooze-header "X-Mu4e-Snooze-Till"
  "Header where snooze time is stored. Note that tools like
offlineimap don't sync these and so you will have trouble working
with mu4e on multiple machines.")

(defun mu4e-snooze-folder (msg)
  "Return snooze folder for `msg'."
  (let ((msg-maildir (mu4e-message-field msg :maildir)))
    (string-join (list "" (car (split-string msg-maildir "/" t)) mu4e-snooze-label) "/")))

(defun mu4e-snooze-write-datetime-header (msg datetime-str)
  "Mark `msg' to have snooze time of `datetime-str'."
  (let ((path (mu4e-message-field msg :path)))
    (if (not (mu4e--contains-line-matching (concat mu4e-snooze-header ":.*") path))
        (mu4e--replace-first-line-matching
         "^$" (concat mu4e-snooze-header ": " datetime-str "\n") path)
      (mu4e--replace-first-line-matching
       (concat mu4e-snooze-header ":.*")
       (concat mu4e-snooze-header ": " datetime-str)
       path))
    (mu4e--refresh-message path)))

(defun mu4e-snooze-read-datetime-header (msg)
  "Return datetime header string value from X header if present."
  (let ((path (mu4e-message-field msg :path)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (regexp-quote mu4e-snooze-header) ":[ \t]*\\(.*\\)$") nil t)
        (let ((match (string-trim (match-string 1))))
          (unless (string= match "")
            match))))))

(defun mu4e-snooze-action (docid msg target)
  "Ask user for a snooze time for given message and snooze.

Allows patterns like +2d also since reading happens using
`org-read-date' function."
  (let* ((datetime (org-read-date t t nil "Snooze datetime: "))
         (datetime-str (format-time-string "%Y-%m-%dT%H:%M:%S%z" datetime)))
    (mu4e-snooze-write-datetime-header msg datetime-str)
    (mu4e--server-move docid (mu4e--mark-check-target target) "-N")
    (mu4e-message "Snoozed to %s" datetime-str)))

(defun mu4e-snooze-snoozed-mails (maildir)
  "Return list of snoozed emails from `maildir'."
  (let ((snooze-dir (string-join (list maildir mu4e-snooze-label) "/")))
    (with-temp-buffer
      (let ((status (call-process "mu" nil t nil
                                  "find" (shell-quote-argument (format "maildir:%s" snooze-dir))
                                  "--format=sexp")))
        (when (zerop status)
          (car (read-from-string (format "(%s)" (buffer-string)))))))))

(defun mu4e-snooze-unsnooze (msg)
  "Clear snooze time and move `msg' back to the relevant inbox."
  (let ((inbox (string-join (list "" (car (split-string (mu4e-message-field msg :maildir) "/" t)) mu4e-snooze-unsnooze-label) "/")))
    (mu4e-snooze-write-datetime-header msg "")
    (mu4e--server-move (mu4e-message-field msg :message-id) inbox)))

(defun mu4e-snooze-should-unsnooze-p (msg)
  "Return whether the `msg' should be unsnoozed."
  (let ((header-val (mu4e-snooze-read-datetime-header msg)))
    ;; If header is empty, we unconditionally unsnooze
    (or (null header-val)
        (let ((unsnooze-time (encode-time (parse-time-string header-val))))
          (time-less-p unsnooze-time (current-time))))))

(defun mu4e-snooze-unsnooze-maildir (maildir)
  "Check snoozed messages in `maildir' and unsnooze them if time
has passed."
  (let ((snoozed-mails (mu4e-snooze-snoozed-mails maildir)))
    (dolist (msg snoozed-mails)
      (when (mu4e-snooze-should-unsnooze-p msg)
        (mu4e-snooze-unsnooze msg)))))

(defun mu4e-snooze-process-snoozed ()
  "Process all snoozed mails and unsnooze ones that need moving."
  (let ((maildirs (mapcar (lambda (ctx) (concat "/" (mu4e-context-name ctx))) mu4e-contexts)))
    (dolist (maildir maildirs)
      (mu4e-snooze-unsnooze-maildir maildir))))

(defun mu4e-snooze ()
  (interactive)
  (mu4e-headers-mark-and-next 'snooze))

(add-to-list 'mu4e-marks
             `(snooze :char "z"
                      :prompt "snooze"
                      :dyn-target (lambda (target msg) (mu4e-snooze-folder msg))
                      :action ,#'mu4e-snooze-action))

(define-key mu4e-headers-mode-map (kbd "z") 'mu4e-snooze)
(run-at-time nil mu4e-snooze-unsnooze-timer 'mu4e-snooze-process-snoozed)

(provide 'mu4e-snooze)

;;; mu4e-snooze.el ends here
