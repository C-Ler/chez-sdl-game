
(define-sdf-property gobj:坐标ls
  坐标
  'predicate complex?
  'default-value 0
  )

(define-type 坐标对象 () (gobj:坐标ls))

(define (创建坐标对象 坐标c)
  (make-坐标对象 '坐标 坐标c))

(define 坐标更新!
  (most-specific-generic-procedure '坐标更新! 2
				   (constant-generic-procedure-handler #f)))

(定义匹配度优先广义过程 get-x坐标 1 '默认x坐标)
(定义匹配度优先广义过程 get-y坐标 1 '默认y坐标)
(广义过程扩展 get-x坐标 ((坐标对象? obj))
	      (坐标x (get-坐标 obj)))
(广义过程扩展 get-y坐标 ((坐标对象? obj))
	      (坐标y (get-坐标 obj)))
