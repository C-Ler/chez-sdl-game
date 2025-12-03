;;; 星组移动,敌机移动,奖励点移动,背景移动,所有这些都需要x,y
;;; 大都还在list中,只有小部分单独出现
;;; 移动这个过程,x,y总是出现,是否需要单独抽象?
;;; 函数式会占用内存,但是可以并行化
(require-nongenerative-clause #t)	;大多时候不需要重复定义rtd并覆盖,这样设置成默认非生成式可以提高效率

(define-record-type 游戏对象		;背景还是最好打包一下
  (fields (mutable 物理组件) (mutable 图形组件)))

(define (get-坐标 obj)			;临时搞成这样
  (cond ((number? obj) obj)
	(else
	 (let ((组件 (游戏对象-物理组件 obj)))
	   (if (number? 组件)
	       组件
	       (物理组件-坐标 组件))))))

(define (set-坐标! obj v)			
  (let ((组件 (游戏对象-物理组件 obj)))
    (if (number? 组件)
	(游戏对象-物理组件-set! obj v)
	(物理组件-坐标-set! 组件 v))))
