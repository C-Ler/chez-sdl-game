;;; 计时器类型
;; 生成敌机的这种可以换成协程 2024年7月29日00:45:22

(define-sdf-property gobj:启动标记			;初始化sdl可能也需要  2024年8月10日16:06:48
  启动标记
  'predicate boolean?
  'default-value #f
  )

(define-sdf-property gobj:暂停标记
  暂停标记
  'predicate boolean?
  'default-value #f
  )

(define-sdf-property gobj:不计时间隔
  不计时间隔
  'predicate real?
  'default-value 0)

(define-sdf-property gobj:累积时间
  累积时间
  'predicate real?
  'default-value 0
  )

(define-type 计时器 () (gobj:启动标记 gobj:暂停标记 gobj:不计时间隔 gobj:累积时间))


(定义匹配度优先广义过程 启动 1 (lambda (x)  (display x) (printf "不支持启动的对象!~%"))) ;这个默认的过程应该单独抽象一下 2024年3月22日20:08:13

(广义过程扩展 启动 ((计时器? 计时器))
	      (begin (set-不计时间隔! 计时器 (sdl-get-ticks)) 
		     (set-启动标记! 计时器 #t)
		     (set-暂停标记! 计时器 #f)))

(定义匹配度优先广义过程 停止 1 (lambda (x) (display x) (printf "不支持停止的对象!~%")))

(广义过程扩展 停止 ((计时器? 计时器))
	      (begin (set-不计时间隔! 计时器 0)
		     (set-计时时间! 计时器 0)
		     (set-启动标记! 计时器 #f)
		     (set-暂停标记! 计时器 #f)))

(定义匹配度优先广义过程 暂停 1 (lambda (x) (display x) (printf "不支持暂停的对象!~%")))

(define (计时器运行? 计时器)
  (and (get-启动标记 计时器) (not (get-暂停标记 计时器))))

(广义过程扩展 暂停 ((计时器? 计时器))
	      (when (计时器运行? 计时器)
		(set-暂停标记! 计时器 #t)
		(set-计时时间! 计时器 (- (sdl-get-ticks) (get-不计时间隔 计时器))) ;tp1 - ts 计时的acc
		)
	      )

(define (计时器暂停? 计时器)
  (and (get-启动标记 计时器) (get-暂停标记 计时器)))

(定义匹配度优先广义过程 取消暂停 1 (lambda (x) (display x) (printf "不支持取消暂停的对象!~%")))

(广义过程扩展 取消暂停 ((计时器? 计时器))
	      (when (计时器暂停? 计时器)
		(set-暂停标记! 计时器 #f)
		(set-不计时间隔! 计时器 (- (sdl-get-ticks) (get-计时时间 计时器))) ;;tup1 - (tp1 - ts) 考虑暂停期间没有计时的时间需要在未来通过sdl-get-ticks返回的累积运行时间(ms)计算计时时间时被计入不计时间隔
		)
	      )

(定义匹配度优先广义过程 获取时间戳 1 (lambda (x) (display x) (printf "不支持获取时间戳的对象!~%")))

(广义过程扩展 获取时间戳 ((计时器? 计时器))
	      (cond ((get-启动标记 计时器)
		     (cond ((get-暂停标记 计时器) (get-计时时间 计时器))
			   (else (- (sdl-get-ticks) (get-不计时间隔 计时器)))))
		    (else
		     0))
	      )
