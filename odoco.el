;;; odoco.el --- 

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
;;org-done-count => odoco.el

;; 
;;TODO

;;1. make graph colar able to change.
;;2. insert and update a table and a graph at a certain point.
;;3. display a table and a graph at another window or frame.
;;4. make a data for a table or graph output with all kinds of period.
;;5. make some kinds of string for search regexp

;;; Code:

(require 'cl)
(require 'time-date)

(require 'gnuplotwin)

;; variables

(defvar odoco:time-list nil)
(defvar odoco:default-graph-file-name "odoco-graph.png")
(defvar odoco:default-plt-conf-str ""
  "this is a document")
(defvar odoco:default-plt-file-name "odoco-plt.plt")
(defvar odoco:default-graph-data-file-name "odoco-tmp-data.txt")
(defvar odoco:default-plt-const ""
  "this is a document")
(defvar odoco:default-plt-option " w l notitle"
  "this is a document")

(defcustom odoco:graph-file-name odoco:default-graph-file-name
  "this is a document")
(defcustom odoco:plt-file-name odoco:default-plt-file-name
  "this is a document")
(defcustom odoco:graph-data-file-name odoco:default-graph-data-file-name
  "this is a document")
(defcustom odoco:plt-const odoco:default-plt-const
  "this is a document")
(defcustom odoco:plt-option odoco:default-plt-option
  "this is a document")
(defcustom odoco:gnuplot-command "wgnuplot"
  "this is a document")

;; interactive functions
(defun odoco:table ()
  (interactive)
  (odoco:make-table 'day 'week))

(defun odoco:graph (&optional interval period)
  "graphの生成"
  (interactive)
  (or interval (setq interval 'day))
  (or period (setq period 'week))
  (let* ((count-data-list (odoco:make-count-data interval period)))
    (odoco:insert-graph count-data-list interval period)))

;; function to manage interactiove option

(defun odoco:make-table (&optional interval period)
  ;; make-table interval刻みの表を、period期間分作成して挿入する。
  (or interval (setq interval 'day))
  (or period (setq period 'week))
  (let* ((count-data-list (odoco:make-count-data interval period)))
    (odoco:insert-table count-data-list interval period)))

(defun odoco:make-graph (tc-list)
  "時間と度数のコンスセルのリストから、グラフを生成する。"
  (let ((gdata-file odoco:graph-data-file-name)
        (gpic-file odoco:graph-file-name)
        (plt-file odoco:plt-file-name))
    (odoco:make-graph-data-file tc-list gdata-file)
    (odoco:submit-gnuplot gdata-file gpic-file plt-file)
    (odoco:insert-graph gpic-file)))

;; functions from count-data to table

(defun odoco:insert-table (data-count-list interval period)
  ""
  (dolist (data period-dcl)
    (let ((day (odoco:format-time-with-interval (car data) interval))
          (count (cdr data)))
      (insert (concat day " " (number-to-string count) "\n")))))

;; functions from time-list to count-data

(defun odoco:make-count-data (interval period)
  ""
  (let* ((time-list (odoco:make-time-list))
         (today (time-to-days (current-time)))
         (days-list (mapcar #'(lambda (x) (- today x)) '(0 1 2 3 4 5 6))))

    (loop for x in days-list
          collect (cons (days-to-time (- x (time-to-days '(0 0))))
                        (loop for y in time-list
                              count (eq x (time-to-days y))))
          into result
          finally return result)))


;; (defun odoco:filter-with-period (data-count-list interval period)
;;   "filter data-count-list with period.
;; When period is 'week, return data-count-list of only this week."
;;   ;; I am looking for new way of this function.
;;   (cond ((and (eq interval 'day) (eq period 'week))
;;          (let* ((today (time-to-days (current-time)))
;;                 (days-list (mapcar #'(lambda (x) (- today x)) '(0 1 2 3 4 5 6)))
;;                 (result-dcl (loop for x in days-list
;;                                   if (equal x (time-to-days (caar data-count-list))) 
;;                                   collect (pop data-count-list) into result-dcl
;;                                   else 
;;                                   collect (cons (days-to-time (- x (time-to-days '(0 0)))) 0) into result-dcl
;;                                   finally return result-dcl)))
;;            result-dcl))))

;; functions from buffer to time-list

(defun odoco:make-time-list ()
  "Search \"DONE ... CLOSED ...\", and add global time-list"
  ;; bufferから正規表現で検索。time-listを作成する。listの各要素もリスト。
  ;; 各要素は、(0 41 11 7 9 2014 0 nil 32400)てな感じの、decode-timeの返り値と同じ形式。
  ;; だから、返り値は、((0 41 11 7 9 2014 0 nil 32400) (0 14 22 5 9 2014 5 nil 32400))みたいな感じ。
  (let (time-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(* DONE \\)\\(.+\\)\\(\n *CLOSED: \\)\\(.*\\)"
                                nil
                                t)
        (let ((time (odoco:encode-time (match-string 4))))
          (push time time-list))))
    (sort time-list 'time-less-p)))

(defun odoco:encode-time (date)
  "Encode the time from buffer to a list format.
 [2014/09/20 日 11:24] -> '(21532 58688)"
  (let ((year (string-to-number (substring date 1 5)))
        (month (string-to-number (substring date 6 8)))
        (day (string-to-number (substring date 9 11)))
        (hour (string-to-number (substring date 14 16)))
        (min (string-to-number (substring date 17 19)))
        (sec 0)
        (dow (let ((d (substring date 12 13)))
               (cond ((equal d "日") 0)
                     ((equal d "月") 1)
                     ((equal d "火") 2)
                     ((equal d "水") 3)
                     ((equal d "木") 4)
                     ((equal d "金") 5)
                     ((equal d "土") 6)))))
    (apply 'encode-time (list sec min hour day month year dow nil 32400))))

(defun odoco:format-time-with-interval (time interval)
  (cond ((eq interval 'day)
         (format-time-string "%m/%d" time))))

(defun odoco:equal-time (time1 time2 interval)
  "confirm time1 and time2 equall in terms of interval.
For example, 2014/08/31 22:11 is equal to 2014/08/31 11:39 in terms of 'day.
2014/08/31 22:11 is equal to 2014/08/01 11:11 in terms of 'month."
  (cond ((eq interval 'day)
         (let ((td1 (time-to-days time1))
               (td2 (time-to-days time2)))
           (eq time1 time2)))
        (t (error "this is an error in odoco:equal-time"))))


;;
(defun odoco:make-graph-data-file (tc-list gdata-file)
  "tc-listをgdata-fileに書き込む"
  (let ((str ""))
    (dolist (point tc-list)
      (let ((x (car point))
            (y (cdr point)))
        (setq str (concat (number-to-string x)
                          " "
                          (number-to-string y)
                          "\n"
                          str))))
    (write-region str nil gdata-file)))

(defun odoco:make-plt-file (gdata-file gpic-file plt-file)
  "pltファイルの作成"
  (let ((plt-conf (odoco:make-plt-conf gdata-file
                                          gpic-file 
                                          odoco:plt-const
                                          odoco:plt-option))) ;文字列の作成
    (write-region plt-conf nil plt-file)))

(defun odoco:make-plt-conf (gdata-file gpic-file plt-const plt-option)
  "pltファイルに書き込む文字列の作成"
  (let ((extention (cadr (split-string gpic-file "\\."))))
    (let ((first (concat "set terminal " extention))
          (second (concat "set output \"" gpic-file "\""))
          (third plt-const)
          (fourth (concat "plot \"" gdata-file "\"" plt-option)))
      (concat first "\n" second "\n" third "\n" fourth))))

(defun odoco:submit-gnuplot (gdata-file gpic-file plt-file)
  "引数からpltファイルを生成し、gnuplotに実行させる"
  (odoco:make-plt-file gdata-file gpic-file plt-file)
  (start-process "emacs-wgnuplot" nil odoco:gnuplot-command plt-file))

(defun odoco:format-data-for-graph (count-data-list)
  (mapcar #'(lambda (x) (cons (format-time-string "%m/%d" (car x)) (cdr x))) count-data-list))

(defun odoco:insert-graph (count-data-list interval period)
  "insert graph"
  (gw:draw-line-graph (odoco:format-data-for-graph (odoco:filter-with-period count-data-list interval period))))

(provide 'odoco:count)
;;; odoco.el ends here

