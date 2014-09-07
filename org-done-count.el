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
(defvar org-done:day-list '("日" "月" "火" "水" "木" "金" "土"))
(defvar org-done:default-graph-file-name "org-done-graph.png")
(defvar org-done:default-plt-conf-str ""
  "this is a document")
(defvar org-done:default-plt-file-name "org-done-plt.plt")
(defvar org-done:default-graph-data-file-name "org-done-tmp-data.txt")
(defvar org-done:default-plt-const ""
  "this is a document")
(defvar org-done:default-plt-option " w l title \"\""
  "this is a document")

(defcustom org-done:graph-file-name org-done:default-graph-name
  "this is a document")
(defcustom org-done:plt-file-name org-done:default-plt-file-name
  "this is a document")
;; (defcustom org-done:plt-conf-str org-done:default-plt-conf-str
;;   "this is a document")
(defcustom org-done:graph-data-file-name org-done:default-graph-data-file-name
  "this is a document")
(defcustom org-done:plt-const org-done:default-plt-const
  "this is a document")
(defcustom org-done:plt-option org-done:default-plt-option
  "this is a document")
(defcustom org-done:gnuplot-command "wgnuplot"
  "this is a document")


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

(defun org-done:make-graph (tc-list)
  "時間と度数のコンスセルのリストから、グラフを生成する。"
  (let ((gdata-file org-done:graph-data-file-name)
        (gpic-file org-done:graph-file-name)
        (plt-file org-done:plt-file-name))
    (org-done:make-graph-data-file tc-list gdata-file)
    (org-done:submit-gnuplot gdata-file gpic-file plt-file)
    (org-done:insert-graph gpic-file)))

(defun org-done:make-graph-data-file (tc-list gdata-file)
  "tc-listをgdata-fileに書き込む"
  (let ((str ""))
    (dolist (point tc-list)
      (let ((x (car point))
            (y (cdr point)))
        (setq str (concat (number-to-string x) " " (number-to-string y) "\n" str))))
    (write-region str nil gdata-file)))

(defun org-done:make-plt-file (gdata-file gpic-file plt-file)
  "pltファイルの作成"
  (let ((plt-conf (org-done:make-plt-conf gdata-file
                                          gpic-file 
                                          org-done:plt-const
                                          org-done:plt-option))) ;文字列の作成
    (write-region plt-conf nil plt-file)))

(defun org-done:make-plt-conf (gdata-file gpic-file plt-const plt-option)
  "pltファイルに書き込む文字列の作成"
  (let ((extention (cadr (split-string gpic-file "\\."))))
    (let ((first (concat "set terminal " extention))
          (second (concat "set output \"" gpic-file "\""))
          (third plt-const)
          (fourth (concat "plot \"" gdata-file "\"" plt-option)))
      (concat first "\n" second "\n" third "\n" fourth))))

(defun org-done:submit-gnuplot (gdata-file gpic-file plt-file)
  "引数からpltファイルを生成し、gnuplotに実行させる"
  (org-done:make-plt-file gdata-file gpic-file plt-file)
  (start-process "emacs-wgnuplot" nil org-done:gnuplot-command plt-file))

(defun org-done:insert-graph (gpic-name)
  "gnuplotで生成した画像を挿入"
  (insert "\n")
  (insert-image (create-image gpic-name))
  (insert "\n"))

(provide 'org-done:count)
;;; org-done-count.el ends here


