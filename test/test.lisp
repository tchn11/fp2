(load "~/quicklisp/setup.lisp")
(load "src/AVLDict.lisp")
(ql:quickload :lisp-unit)

(defun create_big_tree ()
	(let ((tree nil))
		(dotimes (i 50)
			(setq tree (insert_tree tree i i)))
		tree))

(lisp-unit:define-test tree_create
		       (lisp-unit:assert-equal '(nil nil "Head" 1) 
			   							(insert_tree nil "Head" 1)))

(lisp-unit:define-test left_right_insert_test
		       (lisp-unit:assert-equal '((NIL NIL "a_Left" 1) (NIL NIL "c_Right" 1) "b_Head" 1) 
			   							(insert_tree (insert_tree (insert_tree nil "c_Right" 1) "a_Left" 1) "b_Head" 1)))

(lisp-unit:define-test rotate_test
		       (lisp-unit:assert-equal '((NIL NIL "a_Head" 1) (NIL NIL "c_Right1" 1) "a_Right2" 1)
			   							(insert_tree (insert_tree (insert_tree nil "a_Head" 1) "a_Right2" 1) "c_Right1" 1)))

(lisp-unit:define-test get_value_test
		       (lisp-unit:assert-equal  10
			   							(get_value_tree (create_big_tree) 10)))

(lisp-unit:define-test set_value_test
		       (lisp-unit:assert-equal  15
			   							(get_value_tree (set_value_tree (create_big_tree) 10 15) 10)))

(lisp-unit:define-test remove_test
		       (lisp-unit:assert-equal  nil
			   							(get_value_tree (remove_tree (create_big_tree) 10) 10)))

(lisp-unit:define-test filter_test
		       (lisp-unit:assert-equal '(((NIL ((NIL (NIL NIL 12 12) 11 11) (NIL NIL 14 14) 13 13) 10 10)
										((((NIL NIL 16 16) (NIL NIL 18 18) 17 17)
											((NIL NIL 20 20) (NIL NIL 22 22) 21 21) 19 19)
										(((NIL NIL 24 24) (NIL NIL 26 26) 25 25)
											((NIL NIL 28 28) (NIL NIL 30 30) 29 29) 27 27)
										23 23)
										15 15)
										((((NIL NIL 32 32) (NIL NIL 34 34) 33 33)
										((NIL NIL 36 36) (NIL NIL 38 38) 37 37) 35 35)
										(((NIL NIL 40 40) (NIL NIL 42 42) 41 41)
										(((NIL NIL 44 44) (NIL NIL 46 46) 45 45) (NIL (NIL NIL 49 49) 48 48) 47 47)
										43 43)
										39 39)
										31 31)
			   							(filter_tree (create_big_tree) (lambda (key val) (< key 10)))))

(lisp-unit:define-test map_test
		       (lisp-unit:assert-equal '((((((NIL NIL 0 0) (NIL NIL 2 1) 1 1/2) ((NIL NIL 4 2) (NIL NIL 6 3) 5 5/2) 3
											3/2)
										(((NIL NIL 8 4) (NIL NIL 10 5) 9 9/2)
											((NIL NIL 12 6) (NIL NIL 14 7) 13 13/2) 11 11/2)
										7 7/2)
										((((NIL NIL 16 8) (NIL NIL 18 9) 17 17/2)
											((NIL NIL 20 10) (NIL NIL 22 11) 21 21/2) 19 19/2)
										(((NIL NIL 24 12) (NIL NIL 26 13) 25 25/2)
											((NIL NIL 28 14) (NIL NIL 30 15) 29 29/2) 27 27/2)
										23 23/2)
										15 15/2)
										((((NIL NIL 32 16) (NIL NIL 34 17) 33 33/2)
										((NIL NIL 36 18) (NIL NIL 38 19) 37 37/2) 35 35/2)
										(((NIL NIL 40 20) (NIL NIL 42 21) 41 41/2)
										(((NIL NIL 44 22) (NIL NIL 46 23) 45 45/2) (NIL (NIL NIL 49 49/2) 48 24) 47
											47/2)
										43 43/2)
										39 39/2)
										31 31/2)
			   							(map_tree (create_big_tree) (lambda (val) (/ val 2)))))

(lisp-unit:define-test reduce_test_left
		       (lisp-unit:assert-equal 	1225
			   							(reduce_tree_left (create_big_tree) (lambda (val1 val2) (+ val1 val2)) 0)))

(lisp-unit:define-test reduce_test_right
		       (lisp-unit:assert-equal 	1225
			   							(reduce_tree_right (create_big_tree) (lambda (val1 val2) (+ val1 val2)) 0)))

(lisp-unit:define-test summ_tree_int
		       (lisp-unit:assert-equal '((NIL NIL "a" 2) (NIL NIL "c" 1) "b" 2)
			   							(summ_tree (insert_tree (insert_tree nil "a" 1) "b" 1) (insert_tree (insert_tree (insert_tree nil "a" 1) "b" 1) "c" 1))))

(lisp-unit:define-test summ_tree_str
		       (lisp-unit:assert-equal '((NIL NIL "a" "ab") (NIL NIL "c" "b") "b" "ab")
			   							(summ_tree (insert_tree (insert_tree nil "a" "a") "b" "a") (insert_tree (insert_tree (insert_tree nil "a" "b") "b" "b") "c" "b"))))

; Property - based

(lisp-unit:define-test summ_tree_zero1
		       (lisp-unit:assert-equal (create_big_tree)
			   							(summ_tree (create_big_tree) nil)))

(lisp-unit:define-test summ_tree_zero2
		       (lisp-unit:assert-equal (create_big_tree)
			   							(summ_tree nil (create_big_tree))))

(lisp-unit:define-test poly_test
		       (lisp-unit:assert-equal '((NIL NIL "a" 1) (NIL NIL "c" NIL) "b" "a")
			   							(insert_tree (insert_tree (insert_tree nil "c" nil) "b" "a") "a" 1)))

(lisp-unit:define-test poly_key_test1
		       (lisp-unit:assert-equal 	1
			   							(get_value_tree (insert_tree (insert_tree (insert_tree nil "c" nil) "b" "a") "a" 1) "a")))

(lisp-unit:define-test poly_key_test2
		       (lisp-unit:assert-equal 	1
			   							(get_value_tree (insert_tree (insert_tree (insert_tree nil 1.5 nil) 2 "a") 2.5 1) 2.5)))

(lisp-unit:run-tests)
