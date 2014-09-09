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
  (odoco:make-table))
(defun odoco:graph (&optional interval)
  "graphの生成"
  (interactive)
  (or interval (setq interval 'week))
  (setq odoco:time-list (odoco:make-time-list odoco:time-list))
  (let ((done-data (odoco:make-count-data odoco:time-list)))
    (odoco:make-graph done-data)))

;;

(defun odoco:make-count-data (time-list interval)
  ;; ((0 41 11 7 9 2014 0 nil 32400) (0 14 22 5 9 2014 5 nil 32400))みたいなtime-listから、time-count-dataを作成する。時間とその度数を対にしたもの。
  ;; ただし、その時間は、intervalに適した形で比較する。periodの計算のために、構造は変えず、不必要なところを0で統一する。
  (let (result)
    (dolist (time time-list result)
      (let ((curr (odoco:format-with-interval time interval)))
        (if (null result)
            (setq result (list (cons curr 1)))
          (let ((before (odoco:format-with-interval (caar result) interval)))
            (if (equal curr before)
                (setq result (cons (cons before
                                         (1+ (cdar result)))
                                   (cdr result)))
              (setq result (cons (cons curr 1)
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
  ;; timeは(0 41 11 7 9 2014 0 nil 32400)な感じのもの。
  ;; これを、intervalに適した形に変換する。
  ;; intervalが何にも当てはまらない場合は、error
  (cond ((equal interval 'day)
         (append '(0 0 0)
                 (cdddr time)))
        (t (error "error in odoco:format-with-interval 1"))))

(defun odoco:filter-time-list (time interval)
  "timeから、シンボルintervelに合わせて文字列を作成"
  (let ((time-pair (apply 'encode-time time)))
    (cond ((equal interval 'day) (format-time-string "%m/%d" time-pair))
          (t (format-time-string "%m/%d" time-pair)))))

(defun odoco:insert-table (done-data interval period)
  ""
  ;; done-data は　'(((0 0 0 7 9 2014 0 nil 32400) . 2) ((0 0 0 7 9 2014 0 nil 32400) . 3))みたいな感じのはず。
  ;; periodはシンボル。
  ;; ここから、period分だけ抽出する。
  (let ((period-data (odoco:format-with-period interval period)))
    (dolist (data done-data)
      (let ((day (car data))
            (count (cdr data)))
        (insert (concat day " " (number-to-string count) "\n"))))))

(defun odoco:format-with-period (done-data interval period)
  "filter done-data with period.
When period is 'week, return done-data of only this week."
;; assume done data is list of data. ex. (year month day)
  ;; done-dataは　'(((0 0 0 7 9 2014 0 nil 32400) . 2) ((0 0 0 7 9 2014 0 nil 32400) . 3))みたいな感じ。
  ;; periodは'weekとか。
  (let ((today (decode-time (odoco:format-with-interval (decode-time (current-time)) interval))))
    (cond ((eq period 'week)
           (let ((days-before (odoco:sub-day today 6)))
             
             )))
    (days-before (cond ((eq period 'week) (- today (- 7 1)))
                       (t (- today (- 7 1)))))
    result-data
    (dolist (data done-data result-data)
      (if (and (odoco:compare-time today data)
               (odoco:compare-time data days-before))
          (cons data result-data)))))

(defun odoco:sub-day (day num)
  (let ((nt (days-to-time num))
        (dt (apply 'encode-time day)))                ;ntは(hi . lo)
    (decode-time (time-subtract dt nt))))

(defun odoco:make-table (&optional interval period)
  ;; make-table interval刻みの表を、period期間分作成して挿入する。
  (or interval (setq interval 'day))
  (or period (setq period 'week))
  (let (time-list)
    (setq time-list (odoco:make-time-list))
    (let ((done-data (odoco:make-count-data time-list interval)))
      (odoco:insert-table done-data interval period))))

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


;;

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


(provide 'odoco:count)
;;; odoco.el ends here

