;;; org-done-count.el --- 

;; Copyright (C) 2014  T. Ishida

;; Author: T. Ishida <toot.daiylfalaiydt@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 
;;; Code:



(defvar org-done:done-time-list ())

(defun org-done:add-time-list (done-list time)
  (setq org-done:done-time-list (cons time done-list)))

(defun org-done:search-done ()
  (interactive)
  (setq org-done:done-time-list ())
  (while (re-search-forward "\\(* DONE \\)\\(.+\\)\\(\n *CLOSED: \\)\\(.*\\)" nil t)
    (let ((time (string-to-number (org-done:format-time (match-string 4)))))
      (org-done:add-time-list org-done:done-time-list time))))

(defun org-done:format-time (time)
  (let ((len (length time)))
    (set-text-properties 0 (length time) nil time)
    (let ((time-str (split-string (substring time 1 (- len 1)) "[- :]")))
      (org-done:delete-day time-str)
      (apply 'concat time-str))))

(defun org-done:delete-day (time-list)
  (dolist (day day-list)
    (delete day time-list)))

(defvar day-list '("ì˙" "åé" "âŒ" "êÖ" "ñÿ" "ã‡" "ìy"))

(defun org-done:sort-with-time (time-list)
  (sort time-list '<))

(defun org-done:make-done-data (time-list)
  
  )
(defun org-done:amount-done ())

(defun org-done:make-table-for-done ())

(defun org-done:display-table ())

(defun org-done:insert-table ()
  (interactive)
  (goto-char (point-min))
  (org-done:search-done (org-done:done-time-list))
  (org-done:sort-with-time (org-done:done-time-list))
  (let ((done-data (org-done:make-done-data (time-list))))
    (org-done:display-table (done-data))))
  
  
  

(provide 'org-done-count)
;;; org-done-count.el ends here
