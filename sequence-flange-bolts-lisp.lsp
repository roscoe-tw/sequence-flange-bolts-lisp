;;; -*- coding:big5 -*-
(defun c:RRR ()
  (load "sequence-flange-bolts-lisp.lsp")
  (alert "Loaded!"))

(defun xx:MyError (st)
  (if (not (member st (list "Function cancelled" "quit/ exit abort")))
      (vl-bt))
  (princ))

(defun dxf (i l)
  (cdr (assoc i l)))

;;; 螺栓鎖固順序

(defun screw-sequence (OO r n / *error* blockname rr points)
  (setq *error* xx:MyError)
  ;; (setq n 4)
  (setq ;; OO (getpoint "set OO point\n")
	;; r 15
	blockname "螺絲編號"
	rr (+ r 10))
  (command "_.circle" OO rr)
  (setq points (insert-point OO n r))
  (mapcar '(lambda (x)
	    (command "_.insert" blockname (cdr x) "" "" "" (car x)))
	  points)
  (princ))

;;; 圖塊插入點列表，附上螺栓編號
(defun insert-point (OO n r / list1 radian1 n1 num-l)
  (setq radian1 (* pi (/ (/ 360.0 n) 180.0)))
  (setq list1 nil
	n1 0)
  (cond ((= n 4)
	 (setq num-l '("3" "1" "4" "2")))
	((= n 8)
	 (setq num-l '("3" "5" "1" "8" "4" "6" "2" "7")))
	((= n 12)
	 (setq num-l '("3" "9" "5" "1" "12" "8" "4" "10" "6" "2" "11" "7")))
	((= n 16)
	 (setq num-l '("3" "13" "9" "5" "1" "16" "12" "8" "4" "14" "10" "6" "2"
		       "15" "11" "7")))
	((= n 20)
	 (setq num-l '("3" "17" "13" "9" "5" "1" "20" "16" "12" "8" "4"
		       "18" "14" "10" "6" "2" "19" "15" "11" "7")))
	((= n 24)
	 (setq num-l '("3" "21" "17" "13" "9" "5" "1" "24" "20" "16" "12"
		       "8" "4" "22" "18" "14" "10" "6" "2" "23" "19" "15"
		       "11" "7")))
	(t (setq num-l nil)))
  (if num-1
      nil
      (repeat n
	      (progn
		(setq list1 (append list1
				    (list (cons (nth n1 num-l)
						(polar OO (+ 0 (* radian1 n1)) r)))))
		(setq n1 (1+ n1)))))
  list1)

;;; 繪製螺栓固鎖順序圖
(defun c:screw-sequence (/ *error* old_env-set
			 OO list1)
  (setq *error* xx:MyError)
  (setq old_env-set (get-old_env-set))
  (command "_.undo" "m" "")

  (set-env (list '(cmdecho . 0)
		 '(clayer . "0")
		 '(osmode . 0)
		 '(attdia . 0)))
  (setq OO (getpoint "設定一個定點")
	list1 '((4 15) (8 35) (12 55) (16 75) (20 95) (24 115)
		))
  (mapcar '(lambda (x)
	    (screw-sequence OO (cadr x) (car x)))
	  list1)

  (set-env old_env-set)
  (princ))

;;; 取得執行程式前的環境
(defun get-old_env-set (/ old_env)
  (setq old_env (list (cons 'cmdecho (getvar "cmdecho"))
		      (cons 'clayer (getvar "clayer"))
		      (cons 'osmode (getvar "osmode"))
		      (cons 'attdia (getvar "attdia"))))
  old_env)

;;; 設定程式執行環境
(defun set-env (env-set)
  (mapcar '(lambda (x)
	    (cond ((equal (car x) 'cmdecho)
		   (setvar "cmdecho" (cdr x)))
		  ((equal (car x) 'clayer)
		   (setvar "clayer" (cdr x)))
		  ((equal (car x) 'osmode)
		   (setvar "osmode" (cdr x)))
		  ((equal (car x) 'attdia)
		   (setvar "attdia" (cdr x)))
		  (t nil)))
	  env-set)
  (princ))
