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
;;やるべきことはたくさんあります。
;;*グラフの色の設定を可能にする。
;;＊決まった場所にグラフや表を挿入、更新できるように。
;;グラフや表を別ウインドウに表示できるように。
;;表やグラフのデータを、日毎、週ごと、月ごと、n日毎に出力できるように
;;; Code:



(defvar odoco:done-time-list ())
(defvar odoco:day-list '("日" "月" "火" "水" "木" "金" "土"))
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
;; (defcustom odoco:plt-conf-str odoco:default-plt-conf-str
;;   "this is a document")
(defcustom odoco:graph-data-file-name odoco:default-graph-data-file-name
  "this is a document")
(defcustom odoco:plt-const odoco:default-plt-const
  "this is a document")
(defcustom odoco:plt-option odoco:default-plt-option
  "this is a document")
(defcustom odoco:gnuplot-command "wgnuplot"
  "this is a document")


(defun odoco:add-time-list (time-list time)
  "時間リストに時間を追加して返す。"
  (cons time time-list))

(defun odoco:search-done ()
  "バッファの先頭から、DONE~CLOSEDを探して、時間リストに追加する"
  (let (time-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(* DONE \\)\\(.+\\)\\(\n *CLOSED: \\)\\(.*\\)"
                                nil
                                t)
        (let ((time (string-to-number (odoco:format-time (match-string 4)))))
          (setq time-list (odoco:add-time-list time-list time)))))
    time-list))

(defun odoco:format-time (time)
  (let ((len (length time)))
    ;; no need next line
    ;; (set-text-properties 0 (length time) nil time)
    (let ((time-str (split-string (substring time 1 (- len 1)) "[- :]")))
      (odoco:delete-day time-str)
      (apply 'concat time-str))))

(defun odoco:delete-day (time-split-list)
  (dolist (day odoco:day-list)
    (delete day time-split-list)))

(defun odoco:sort-with-time (time-list)
  (sort time-list '>))

(defun odoco:make-done-data (time-list)
  (let ((result ()))
    (dolist (item time-list result)
      (if (or (null result) (not (eq (odoco:filter-with-day item) (caar result))))
          (setq result (cons (cons (odoco:filter-with-day item) 1) result))
        (setq result (cons (cons (caar result) (1+ (cdar result))) (cdr result)))))))

(defun odoco:filter-with-day (time)
  (string-to-number (substring (number-to-string time) 0 8)))

(defun odoco:insert-table (done-data)
  (dolist (data done-data)
    (let ((day (odoco:format-day-from-number (car data)))
          (count (cdr data)))
      (insert (concat day " " (number-to-string count) "\n")))))

(defun odoco:format-day-from-number (day)
  (let* ((str (number-to-string day))
         (year (substring str 0 4))
         (month (substring str 4 6))
         (day (substring str 6 8)))
    (concat year " " month "/" day)))

(defun odoco:table ()
  (interactive)
  (odoco:make-table odoco:done-time-list))

(defun odoco:make-table (time-list)
  (setq time-list (odoco:sort-with-time (odoco:search-done)))
  (let ((done-data (odoco:make-done-data time-list)))
    (odoco:insert-table done-data)))

(defun odoco:graph ()
  "graphの生成"
  (interactive)
  (setq odoco:done-time-list (odoco:search-done odoco:done-time-list))
  (setq odoco:done-time-list (odoco:sort-with-time odoco:done-time-list))
  (let ((done-data (odoco:make-done-data odoco:done-time-list)))
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
        (setq str (concat (number-to-string x) " " (number-to-string y) "\n" str))))
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
;;; odoco-count.el ends here


