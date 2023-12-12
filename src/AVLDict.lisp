(defun create_node (key value)
  (cons nil (cons nil (cons key (cons value nil)))))

(defun set_left (node_parent node_child)
  (setf (car node_parent) node_child)
  node_parent)

(defun set_right (node_parent node_child)
  (setf (cadr node_parent) node_child)
  node_parent)

(defun set_left_right (node_parent node_child_l node_child_r)
  (set_left node_parent node_child_l)
  (set_right node_parent node_child_r)
  node_parent)

(defun get_left (node)
  (car node))

(defun get_right (node)
  (cadr node))

(defun get_key (node)
  (caddr node))

(defun get_value (node)
  (caddr (cdr node)))

(defun set_key (node new_key)
  (setf (caddr node) new_key)
  node)

(defun set_value (node new_value)
  (setf (caddr (cdr node)) new_value)
  node)

(defun height (node)
  (if (not node)
    0
    (max (+ (height (get_left node)) 1) (+ (height (get_right node)) 1))))

(defun bfactor (node)
  (- (height (get_right node)) (height (get_left node))))

(defun print_node (node)
  (write (get_key node))
  (write-string ": ")
  (write (get_value node))
  (terpri))

(defun print_tree (node n)
  (if (not (not node))
  (progn 
    (print_tree (get_right node) (+ n 1))
    (dotimes (i n)
      (write-string "      "))
    (print_node node)
    (print_tree (get_left node) (+ n 1)))))

(define-condition types_not_equal (error)
  ())

(defun type_equal (t1 t2)
  (and (subtypep t1 t2)
       (subtypep t2 t1)))

(defun universal< (n1 n2)
  (cond
    ((type_equal 'n1 'n2)
      (make-condition 'types_not_equal))
    ((typep n1 'string) 
      (string< n1 n2))
    (t 
      (< n1 n2))))

(defun universal= (n1 n2)
  (cond
    ((type_equal 'n1 'n2)
      (make-condition 'types_not_equal))
    ((typep n1 'string) 
      (string= n1 n2))
    (t 
      (= n1 n2))))

(defun rotate_rigth (p)
  (let ((q (get_left p)))
  (set_left p (get_right q))
  (set_right q p)
  q))

(defun rotate_left (q)
  (let ((p (get_right q)))
  (set_right q (get_left p))
  (set_left p q)
  p))

(defun balance (p)
  (cond
    ((= (bfactor p) 2) (progn
      (if (< (bfactor (get_right p)) 0)
        (set_right p (rotate_rigth (get_right p))))
      (rotate_left p)))
    ((= (bfactor p) -2) (progn
      (if (> (bfactor (get_left p)) 0)
        (set_left p (rotate_left (get_left p))))
      (rotate_rigth p)))
    (t p)))
  
(defun insert (node key value)
  (cond
    ((not node) (create_node key value))
    ((universal< key (get_key node)) (balance (set_left node (insert (get_left node) key value))))
    (t (balance (set_right node (insert (get_right node) key value))))))

(defun get_value_tree (node key)
  (cond
    ((not node) nil)
    ((universal= (get_key node) key) (get_value node))
    ((universal< key (get_key node)) (get_value_tree (get_left node) key))
    (t (get_value_tree (get_right node) key))))

(print_tree (insert (insert (insert (insert (insert nil 5 5) 4 4) 3 3) 2 2) 1 1) 0)
(terpri)
(print (get_value_tree (insert (insert (insert (insert (insert nil 5 5) 4 4) 3 3) 2 2) 1 1) 3))
