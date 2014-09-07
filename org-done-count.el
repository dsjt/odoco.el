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

(defvar org-done:day-list '("“ú" "ŒŽ" "‰Î" "…" "–Ø" "‹à" "“y"))


(defun org-done:add-time-list (done-list time)
  (setq org-done:done-time-list (cons time done-list)))

(defun org-done:search-done ()
  (interactive)                         ;scheduled delete
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
  (dolist (day org-done:day-list)
    (delete day time-list)))

(defun org-done:sort-with-time (time-list)
  (sort time-list '>))

(defun org-done:make-done-data (time-list)
  (let ((result ()))
    (dolist (item time-list result)
      (if (or (null result) (not (eq (org-done:filter-with-day item) (caar result))))
          (setq result (cons (cons (org-done:filter-with-day item) 1) result))
        (setq result (cons (cons (caar result) (1+ (cdar result))) (cdr result)))))))

(defun org-done:filter-with-day (time)
  (string-to-number (substring (number-to-string time) 0 8)))

(defun org-done:insert-table (done-data)
  (dolist (data done-data)
    (let ((day (org-done:format-day-from-number (car data)))
          (count (cdr data)))
      (insert (concat day " " (number-to-string count) "\n")))))

(defun org-done:format-day-from-number (day)
  (let* ((str (number-to-string day))
         (year (substring str 0 4))
         (month (substring str 4 6))
         (day (substring str 6 8)))
    (concat year " " month "/" day)))

(defun org-done:make-table ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-done:search-done))
  (setq org-done:done-time-list (org-done:sort-with-time org-done:done-time-list))
  (let ((done-data (org-done:make-done-data org-done:done-time-list)))
    (org-done:insert-table done-data)))

(defun org-done:make-graph (time-count-list)
  (let ((graph-data-file-name org-done:default-graph-file-name)
        (graph-name org-done:graph-name))
    (org-done:make-graphdata-file time-count-list graph-data-file-name)
    (org-done:submit-gnuplot graph-data-file-name graph-name)
    (org-done:insert-graph graph-name)))

(defun org-done:make-graphdata-file (tc-list file-name)
  (let ((str ""))
    (dolist (point tc-list)
      (let ((x (car point))
            (y (cdr point)))
        (setq str (concat (number-to-string x) " " (number-to-string y) "\n" str))))
    (write-region str nil file-name)))

(defun org-done:make-plt-file (plt-file-name)
  (unless (file-exists-p file-name)
    (write-region default-conf-str nil plt-file-name)))

(defcustom org-done:default-conf-str (concat "set terminal png\nset output " org-done:default-graph-file-name "\nplot " org-done:default-graph-data-file-name)
  "this is a document")

(defun org-done:submit-gnuplot (file-name graph-name)
  (start-process "emacs-wgnuplot" "*wgnuplot*" "wgnuplot" "load" file-name))

(defvar org-done:default-graph-name "org-done-graph.png")
(defcustom org-done:graph-name org-done:default-graph-name
  "this is a document")

(defvar org-done:default-graph-file-name)

(defun org-done:insert-graph (graph-name)
  (insert-img (create-image graph-name)))




(provide 'org-done:count)
;;; org-done-count.el ends here


