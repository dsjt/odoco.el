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


(defun odoco:add-time-list (time-list time)
  "Add time to time-list."
  (cons time time-list))

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
          (setq time-list (odoco:add-time-list time-list time)))))
    (odoco:sort-with-time time-list)))

(defun odoco:encode-time (dtime)
  "Encode the time from buffer to a list format.
 [2014/09/20 日 11:24] -> '(0 24 11 20 9 2014 0 nil 32400)"
  ;; バッファから抜き出したorg形式の文字列を、decode-timeの返り値と同じ形に。返り値は、そのまま、(apply 'encode-time ここ)　につっこめる。
  ;; これ、make-table内でしか使わないだろうから、なくしてもいい。
  (let ((year (string-to-number (substring dtime 1 5)))
        (month (string-to-number (substring dtime 6 8)))
        (day (string-to-number (substring dtime 9 11)))
        (hour (string-to-number (substring dtime 14 16)))
        (min (string-to-number (substring dtime 17 19)))
        (sec 0)
        (dow (let ((d (substring dtime 12 13)))
               (cond ((equal d "日") 0)
                     ((equal d "月") 1)
                     ((equal d "火") 2)
                     ((equal d "水") 3)
                     ((equal d "木") 4)
                     ((equal d "金") 5)
                     ((equal d "土") 6)))))
    (list sec min hour day month year dow nil 32400)))

;; (defun odoco:decode-time (etime)
;;   "'(0 24 11 20 9 2014 0 nil 32400) -> [2014/09/20 日 11:24]"
;;   ;; encode-timeを作ったし、対応するdecodeも作ったほうがいいかなって。でもいらないね。
;;   (let ((dow (cond ((equal (nth 6 etime) 0) "日")
;;                     ((equal (nth 6 etime) 1) "月")
;;                     ((equal (nth 6 etime) 2) "火")
;;                     ((equal (nth 6 etime) 3) "水")
;;                     ((equal (nth 6 etime) 4) "木")
;;                     ((equal (nth 6 etime) 5) "金")
;;                     ((equal (nth 6 etime) 6) "土")))
;;         (str (format-time-string "[%Y/%m/%d buf %R]"
;;                                  (apply 'encode-time etime))))
;;     (replace-regexp-in-string "buf" dow str)))

(defun odoco:sort-with-time (time-list)
  (sort time-list 'odoco:compare-time))

(defun odoco:compare-time (time1 time2)
  "compare time1 time2.
time1 and time2 is encoded (by encode-time function) and  one for example '(21518 5001)."
  (let ((etime1 (apply 'encode-time time1))
        (etime2 (apply 'encode-time time2)))
    (odoco:compare-any-list etime1 etime2)))

(defun odoco:compare-any-list (list1 list2)
  "Compare list1 and list2.
Compare car of list1 and car of list2. 
When former is bigger than latter, return t. When former is less than latter return nil. Otherwise start to compare cadr of list1 and cadr of list2 recursively. If all factor of list1 and list2 is equall, return t.
Even if length are different, this function do not error."
  (cond ((and (null list1) (null list2)) t)
        ((or (null list1) (null list2)) nil)
        (t       
         (let ((item1 (car list1))
               (item2 (car list2)))
           (cond ((> item1 item2) t)
                 ((< item1 item2) nil)
                 (t (odoco:compare-any-list (cdr list1) (cdr list2))))))))

(defun odoco:make-count-data (time-list interval)
  ;; ((0 41 11 7 9 2014 0 nil 32400) (0 14 22 5 9 2014 5 nil 32400))みたいなtime-listから、time-count-dataを作成する。時間とその度数を対にしたもの。
  ;; ただし、その時間は、intervalに適したものに変換する。
  (let (result)
    (dolist (time time-list result)
      (let ((curr (odoco:format-with-interval time interval)))
        (if (null result)
            (setq result (list (cons curr 1)))
          (let ((before (odoco:format-with-interval (caar result) interval)))
            (if (equal curr before)
                (setq result (odoco:add-count-data (cons before
                                                         (1+ (cdar result)))
                                                   (cdr result)))
              (setq result (odoco:add-count-data (cons curr 1)
                                                 result)))))))))

(defun odoco:equal-time (time1 time2 interval)
  "confirm time1 and time2 equall in terms of interval.
For example, 2014/08/31 22:11 is equal to 2014/08/31 11:39 in terms of 'day.
2014/08/31 22:11 is equal to 2014/08/01 11:11 in terms of 'month."
  (let ((format-time1 (odoco:format-with-interval time1 interval))
        (format-time2 (odoco:format-with-interval time1 interval)))
    (odoco:compare-any-list time1 time2)))

(defun odoco:format-with-interval (time interval)
  ""
  (cond ((eq interval 'day)
         ))
)

(defun odoco:filter-time-list (time interval)
  "timeから、シンボルintervelに合わせて文字列を作成"
  (let ((time-pair (apply 'encode-time time)))
    (cond ((equal interval 'day) (format-time-string "%m/%d" time-pair))
          (t (format-time-string "%m/%d" time-pair)))))

(defun odoco:add-count-data (item data)
  (cons item data))

(defun odoco:insert-table (done-data period)
  (let ((period-data (odoco:make-period-data period)))
    (dolist (data done-data)
      (let ((day (car data))
            (count (cdr data)))
        (insert (concat day " " (number-to-string count) "\n"))))))

(defun odoco:make-period-data (done-data period)
  "filter done-data with period.
When period is 'week, return done-data of only this week."
;; assume done data is list of data. ex. (year month day)
  (let* ((today (current-time))
         (days-before (cond ((eq period 'week) (- today (- 7 1)))
                            (t (- today (- 7 1)))))
         result-data)
    (dolist (data done-data result-data)
      (if (and (odoco:compare-time today data)
               (odoco:compare-time data days-before))
          (cons data result-data)))))

(defun odoco:table ()
  (interactive)
  (odoco:make-table))

(defun odoco:make-table (&optional interval period)
  ;; make-table interval刻みの表を、period期間分作成して挿入する。
  (or interval (setq interval 'day))
  (or period (setq period 'week))
  (let (time-list)
    (setq time-list (odoco:make-time-list))
    (let ((done-data (odoco:make-count-data time-list interval)))
      (setq aa done-data)
      ;; (odoco:insert-table done-data period)
      )))

(defun odoco:graph (&optional interval)
  "graphの生成"
  (interactive)
  (or interval (setq interval 'week))
  (setq odoco:time-list (odoco:make-time-list odoco:time-list))
  (let ((done-data (odoco:make-count-data odoco:time-list)))
    (odoco:make-graph done-data)))

(defun odoco:make-graph (tc-list)
  "時間と度数のコンスセルのリストから、グラフを生成する。"
  (let ((gdata-file odoco:graph-data-file-name)
        (gpic-file odoco:graph-file-name)
        (plt-file odoco:plt-file-name))
    (odoco:make-graph-data-file tc-list gdata-file)
    (odoco:submit-gnuplot gdata-file gpic-file plt-file)
    (odoco:insert-graph gpic-file)))

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

(defun odoco:insert-graph (gpic-name)
  "gnuplotで生成した画像を挿入"
  (insert "\n")
  (insert-image (create-image gpic-name))
  (insert "\n"))

(provide 'odoco:count)
;;; odoco.el ends here

