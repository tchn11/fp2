(load "src/utils.lisp")

(defun insert_tree (node key value)
  (cond
   ((not node) (create_node key value))
   ((universal< key (get_key node)) (balance (set_left (copy_node node) (insert_tree (get_left node) key value))))
   (t (balance (set_right (copy_node node) (insert_tree (get_right node) key value))))))

(defun get_value_tree (node key)
  (cond
   ((not node) nil)
   ((universal= (get_key node) key) (get_value node))
   ((universal< key (get_key node)) (get_value_tree (get_left node) key))
   (t (get_value_tree (get_right node) key))))

(defun set_value_tree (node key value)
  (cond
   ((not node) nil)
   ((universal= (get_key node) key) (set_value (copy_node_with_child node) value))
   ((universal< key (get_key node)) (set_left (copy_node node) (set_value_tree (get_left node) key value)))
   (t (set_right (copy_node node) (set_value_tree (get_right node) key value)))))

(defun remove_tree (node key)
  (balance (cond
	    ((not node) nil)
	    ((universal< key (get_key node)) (set_left (copy_node node) (remove_tree (get_left node) key)))
	    ((universal> key (get_key node)) (set_right (copy_node node) (remove_tree (get_right node) key)))
	    (t (let ((new_node (copy_node_with_child node))) (let ((q (get_left new_node)) (r (get_right new_node)))
		 (if (not r)
		     q
		   (let ((min (find_min r)))
		     (set_right min (remove_min r))
		     (set_left min q)
		     (balance min)))))))))

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
      (set_value (copy_node node) (funcall fun (get_value node))))))

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
   ((not node2) (copy_node node1))
   ((not node1) (copy_node node2))
   ((= (is_key_in_tree node1 (get_key node2)) 1)
    (let ((node1_tmp (summ_tree (summ_tree (copy_node node1) (get_right node2)) (get_left node2))))
      (set_value_tree
       node1_tmp
       (get_key node2) (universal+ (get_value_tree node1_tmp (get_key node2)) (get_value node2)))))
   (t
    (insert_tree
     (summ_tree (summ_tree node1 (get_right node2)) (get_left node2))
     (get_key node2) (get_value node2)))))
