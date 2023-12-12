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

(defun universal> (n1 n2)
  (cond
    ((type_equal 'n1 'n2)
      (make-condition 'types_not_equal))
    ((typep n1 'string) 
      (string> n1 n2))
    (t 
      (> n1 n2))))

(defun universal= (n1 n2)
  (cond
    ((type_equal 'n1 'n2)
      (make-condition 'types_not_equal))
    ((typep n1 'string) 
      (string= n1 n2))
    (t 
      (= n1 n2))))

(defun universal+ (n1 n2)
  (cond
    ((type_equal 'n1 'n2)
      (make-condition 'types_not_equal))
    ((typep n1 'string) 
      (concatenate 'string n1 n2))
    (t 
      (+ n1 n2))))

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
  
(defun insert_tree (node key value)
  (cond
    ((not node) (create_node key value))
    ((universal< key (get_key node)) (balance (set_left node (insert_tree (get_left node) key value))))
    (t (balance (set_right node (insert_tree (get_right node) key value))))))

(defun get_value_tree (node key)
  (cond
    ((not node) nil)
    ((universal= (get_key node) key) (get_value node))
    ((universal< key (get_key node)) (get_value_tree (get_left node) key))
    (t (get_value_tree (get_right node) key))))

(defun set_value_tree (node key value)
  (cond
    ((not node) nil)
    ((universal= (get_key node) key) (set_value node value))
    ((universal< key (get_key node)) (set_left node (set_value_tree (get_left node) key value)))
    (t (set_right node (set_value_tree (get_right node) key value)))))

(defun find_min (node)
  (if (not (get_left node))
    node
    (find_min (get_left node))))

(defun remove_min (node)
  (if (not (get_left node))
    (get_right node)
    (balance (set_left node (remove_min (get_left node))))))

(defun remove_tree (node key)
  (balance (cond
    ((not node) nil)
    ((universal< key (get_key node)) (set_left node (remove_tree (get_left node) key)))
    ((universal> key (get_key node)) (set_right node (remove_tree (get_right node) key)))
    (t (let ((q (get_left node)) (r (get_right node)))
        (if (not r) 
          q
          (let ((min (find_min r)))
            (set_right min (remove_min r))
            (set_left min q)
            (balance min))))))))

(defun filter_tree (node fun)
  (if (not node)
  nil
  (progn
    (set_left node (filter_tree (get_left node) fun))
    (set_right node (filter_tree (get_right node) fun))
    (if (funcall fun (get_key node) (get_value node))
      (remove_tree node (get_key node))
      (balance node)))))

(defun filter_tree (node fun)
  (if (not node)
  nil
  (progn
    (set_left node (filter_tree (get_left node) fun))
    (set_right node (filter_tree (get_right node) fun))
    (if (funcall fun (get_key node) (get_value node))
      (remove_tree node (get_key node))
      (balance node)))))

(defun map_tree (node fun)
  (if (not node)
    nil
    (progn
      (set_left node (map_tree (get_left node) fun))
      (set_right node (map_tree (get_right node) fun))
      (set_value node (funcall fun (get_value node))))))

(defun reduce_tree_left (node fun zero)
  (if (not node)
    zero
    (funcall fun 
      (reduce_tree_left (get_left node) fun zero) 
      (funcall fun 
        (reduce_tree_left (get_right node) fun zero) 
        (get_value node)))))

(defun reduce_tree_right (node fun zero)
  (if (not node)
    zero
    (funcall fun 
      (get_value node)
      (funcall fun 
        (reduce_tree_right (get_right node) fun zero) 
        (reduce_tree_right (get_left node) fun zero)))))

(defun is_key_in_tree (node key)
  (cond 
    ((not node) 0)
    ((universal= key (get_key node)) 1)
    (t  (+ (is_key_in_tree (get_left node) key) (is_key_in_tree (get_right node) key)))))

(defun summ_tree (node1 node2)
  (cond
    ((not node2) node1)
    ((not node1) node2)
    ((= (is_key_in_tree node1 (get_key node2)) 1)
      (let ((node1_tmp (summ_tree (summ_tree node1 (get_right node2)) (get_left node2))))
      (set_value_tree
        node1_tmp
        (get_key node2) (universal+ (get_value_tree node1_tmp (get_key node2)) (get_value node2)))))
    (t
      (insert_tree 
        (summ_tree (summ_tree node1 (get_right node2)) (get_left node2))
        (get_key node2) (get_value node2)))))
