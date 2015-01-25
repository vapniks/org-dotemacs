
;; test first `eval-buffer'   then use C-x C-e at the end of next line
;; (insert "\n" (show-cycle '(block1 block2 block3 block4 block5 block1) ))

;; if there is no error occur ,it would be like this :

;;    --------------------------------------------
;;    |                                          |
;;    |                                          V
;; block2 <-- block3 <-- block4 <-- block5 <-- block1

;; then you can use [return] at the block name to jump the define of block
;; for now ,it only as a test

(defun mystr (&rest args)
  "like `make-string' with more args "
  (with-temp-buffer
    (let ((i nil) )
	(mapc
	 (lambda (it)
	   (cond
	    ((numberp it)(setq i it))
	    ((and (stringp it) (numberp i))
	     (insert (make-string i (string-to-char it)))
	     (setq i nil))
	    ((stringp it ) (insert it))))
	 args))
    (buffer-string)))

(defvar  cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"
      (lambda ()
	(interactive)
	(let* ((file (get-text-property (point) 'file))
	       (name (thing-at-point 'symbol))
	       (buf (find-buffer-visiting file)))
	  (if buf
	      (switch-to-buffer buf)
	    (find-file file)
	    )
	  (goto-char 1)
	  ;;find the named block
	  (progn
	    (message "finding the position of named block: %s" name))
	  )
	))
    map)
  "you can use [return] jump to the block define position")

(defun show-cycle (a-list &optional direct)
  ""
  (let* ((file "~/.test.org")
	 (tail (cdr a-list))
	(last-name (if (stringp (car (last tail)))
		       (car (last tail))
		     (symbol-name (car (last tail)))))
	(first-name (if (stringp (car tail))
		       (car tail)
		     (symbol-name (cadr a-list))))	
	(pre (/ (length first-name) 2))
	(suf (/ (length last-name) 2)))
    (with-temp-buffer
      (let (list-str)
	(setq list-str
	      (mapconcat
	       (lambda (item)
		 (let ((name (if (stringp item)
				 item
			       (symbol-name item))))
		   (propertize name 'file file 'local-map cycle-map)
		   ))
	       tail (if direct " --> " " <-- ")))
	(print list-str)
	(setq mid-len (- (length list-str) pre suf))
	(insert (mystr  pre " " mid-len "-" "\n")
		(mystr  pre " " "|" (- mid-len 2) " " "|" "\n")
		(mystr  pre " " (if direct "V" "|") (- mid-len 2) " "
			(if direct "|" "V") "\n"))
	(insert list-str "\n")
	(buffer-string)))))
  
;; test

;;(topological-sort '((a .(b  d))(b . (c))(c . (a)) (d . (c))))
;;(setq chong-debug-p nil)


(defvar chong-debug-p t
  "when set it to `t' ,it will print some debug info
otherwise do nothing")

(defmacro chong-debug (&rest body)
  `(if chong-debug-p
       (progn
	 ,@body)
     nil))


(defun dependecy-view (hash-table-graph)
  "用一个用户友好的方式显示元素之间的依赖关系
用不用首先排序一下呢？ 先不想那么复杂了吧"
  (let ((finish nil) root dep-list  cur cur-be-deps)
    ;; set `cur' with one of hash-table keys
    (let ((max 0)  be-deps)
      (maphash
       (lambda (k v)
	 (setq be-deps (cdr v))		;得到被依赖 找最大被依赖
	 (setcar v 1)			;将他用来做回溯点
	 (when (> (length be-deps) max)
	   (setq cur k			;得到最大被依赖 并用他做起点
		 max (length be-deps)))
	 )
       hash-table-graph)
      (chong-debug
       (message "the `cur' is: %s  and items be depent are" cur )
       (print (gethash cur hash-table-graph)))
      )
    (setq root cur)			;设置指针
    (push cur dep-list)			;得到被依赖
    (setq cur-be-deps (gethash cur hash-table-graph))
    (setq cur (nth (car cur-be-deps) cur-be-deps)) ;得到首依赖
    (when (equal (length (cdr cur-be-deps)) 0)
      (error "the graphy no dependecy")) ;用指针指向下一个单元
    (setcar cur-be-deps (1+ (car cur-be-deps)))
    ;; 关键的是我该如何的回溯
    (while (not finish) 
      (when (null cur) (error "pushed nil")) ;; 当有错误的时候 就停止打印
      (push cur dep-list)

      (chong-debug
       (message "pushed %s" cur)
       (print dep-list))

      (setq cur-be-deps (gethash cur hash-table-graph))
      (if (equal 0 (length (cdr cur-be-deps))) ; 说明他已经遇到了空尾 应该回溯了
	(progn
	  (pop dep-list)			; 遇到了空尾的时候就弹出自己 并将先前的也弹出 那cur是谁呢？
	  (setq cur (pop dep-list))
	)
	(setq cur (nth (car cur-be-deps) cur-be-deps)) ;正常的时候是得到被依赖 并继续
	(if (not (null cur))			       ;如果能够继续的找到下一个节点 就改变指针
	    (setcar cur-be-deps (1+ (car cur-be-deps)))
	  ;; 如果他没有下一个节点可供查询了怎么办 回溯啊
	  ;; 一般的时候 会回溯 但是 要是root也要回溯的话 就说明完结了
	  (if (equal root (car dep-list)) ;; 当root得到的也是nil的时候
	      (setq finish t)
	    (pop dep-list)		; 首先将我弹出
	    (setcar cur-be-deps 1)	; 然后将我的指针变成 0
	    (setq cur (pop dep-list))		; 将我的前任也弹出  会继续处理我的前任的
	    )))
      (chong-debug
       (message "befor judge")
       (print cur)
       (print dep-list))
      (when (member cur dep-list)	;当出现重复的时候 就应该
	(print "找到了一个序列 尝试找到下一个点")
	(print dep-list)(print cur)
	(setq cur (nth (car cur-be-deps) cur-be-deps))
	(setcar cur-be-deps (1+ (car cur-be-deps))) ;设置下一个节点
	(while (and cur (member cur dep-list))
	   (print "找到了一个序列 尝试找到下一个点")
	   (princ dep-list)(print cur)
	  )
	(when (null cur) (setq cur (pop dep-list)))
	))))


;;(topological-sort '((a .(b  d))(b . (c))(c . (a)) (d . (c))))
;;(setq chong-debug-p nil)


  (defun* topological-sort (graph &key (test 'eql))
    "Returns a list of packages to install in order.
  Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
    (let* ((entries (make-hash-table :test test))
           ;; avoid obsolete `flet' & backward-incompatible `cl-flet'
           (entry (lambda (v)
                    "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
                    (or (gethash v entries)
                        (puthash v (cons 0 '()) entries)))))
      ;; populate entries initially
      (dolist (gvertex graph)
        (destructuring-bind (vertex &rest dependencies) gvertex
          (let ((ventry (funcall entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (funcall entry dependency)))
                (unless (funcall test dependency vertex)
                  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      (chong-debug
       (message "the new graphy generated by topological-sort :")
       (print entries)
       )
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (funcall entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (funcall entry dependant))))
                (push dependant S)))))
	(chong-debug
	 (message "new graphy after deleted zero dependecy :")
	 (print entries))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
	   (unless all-sorted-p
	    (message "new graphy with ummete dependency :")
	    (print entries)
	    (dependecy-view entries)
	    )
          (values (nreverse L)   all-sorted-p
                  (unless all-sorted-p entries))))))
