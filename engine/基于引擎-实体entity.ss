;;; Base object type
(define-sdf-property object:name name)

(define-sdf-property object:description description
  'default-to-property object:name)	;这个十分特殊,用宏展开和别的不一样,不需要加'号

(define-type object () (object:name object:description))

(define (find-object-by-name name objects)
  (find (lambda (object)
          (eqv? name (get-name object)))
        objects))

(define-generic-procedure-handler tagged-data-representation ;这两个可能起到某种作用,比如pp 示例中的各种obj  2024年2月19日20:56:38
  ;; 扩展了tagging中的广义过程,返回一条继承链上的名称 2024年8月10日16:12:15
  (match-args object?)
  (lambda (super object)
    (append (super object)
            (list (get-name object)))))

;;; rect
;; (define-sdf-property gobj:w
;;   w
;;   'predicate integer?
;;   'default-value 0
;;   )

;; (define-sdf-property gobj:h
;;   h
;;   'predicate integer?
;;   'default-value 0
;;   )
;;; 小心调用的文件定义属性的宏展开之后覆盖了已有的广义过程 ....2025年2月25日20:10:13

;;; engine中坐标定义了
;; (定义匹配度优先广义过程 get-x 1 (constant-generic-procedure-handler #f))
;; (定义匹配度优先广义过程 get-y 1 (constant-generic-procedure-handler #f))


(定义匹配度优先广义过程 get-w 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-h 1 (constant-generic-procedure-handler #f))

(广义过程扩展 get-x ((sdl-rect? obj))
	      (sdl-rect-x obj))

(广义过程扩展 get-y ((sdl-rect? obj))
	      (sdl-rect-y obj))

(广义过程扩展 get-w ((sdl-rect? obj))
	      (sdl-rect-w obj))

(广义过程扩展 get-h ((sdl-rect? obj))
	      (sdl-rect-h obj))

(定义匹配度优先广义过程 set-x! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-y! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-w! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-h! 1 (constant-generic-procedure-handler #f))

;;; color 
(定义匹配度优先广义过程 get-r 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-g 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-b 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-a 1 (constant-generic-procedure-handler #f))

(广义过程扩展 get-r ((sdl-color? obj))
	      (sdl-color-r obj))

(广义过程扩展 get-g ((sdl-color? obj))
	      (sdl-color-g  obj))

(广义过程扩展 get-b ((sdl-color? obj))
	      (sdl-color-b  obj))

(广义过程扩展 get-a ((sdl-color? obj))
	      (sdl-color-a  obj))

(define 白色 (make-sdl-color 255 255 255 0))

;;; UI
(define-sdf-property gobj:color		;渲染必备颜色
  color
  'predicate sdl-color?
  'default-value 白色 ;这里引用了共享的部分 2024年9月20日16:40:39
  )

(define-type UI类型 ()
  (gobj:color)	       
  )

(广义过程扩展 get-r ((UI类型? obj))
	      (sdl-color-r (get-color obj)))

(广义过程扩展 get-g ((UI类型? obj))
	      (sdl-color-g (get-color obj)))

(广义过程扩展 get-b ((UI类型? obj))
	      (sdl-color-b (get-color obj)))

(广义过程扩展 get-a ((UI类型? obj))
	      (sdl-color-a (get-color obj)))

;;; 含坐标的UI
(define-type 含坐标UI (UI类型? 坐标对象?)		;大部分被渲染对象都需要坐标和规格
  ()
  )

;;; 含矩形的UI
(define-sdf-property gobj:rect		;大部分对象都需要xywh
  rect
  'predicate sdl-rect?
  'default-value (make-sdl-rect 0 0 0 0)
  )

(define-type 含rectUI (UI类型?)		;大部分被渲染对象都需要坐标和规格
  (gobj:rect)
  )

(define 含rectobj? 含rectUI?)

(广义过程扩展 get-x ((含rectUI? obj))
	      (sdl-rect-x (get-rect obj)))

(广义过程扩展 get-y ((含rectUI? obj))
	      (sdl-rect-y (get-rect obj)))

(广义过程扩展 get-w ((含rectUI? obj))
	      (sdl-rect-w (get-rect obj)))

(广义过程扩展 get-h ((含rectUI? obj))
	      (sdl-rect-h (get-rect obj)))

(广义过程扩展 set-x! ((含rectUI? obj) (real? v))
	      (sdl-rect-x-set! (get-rect obj) v))

(广义过程扩展 set-y! ((含rectUI? obj) (real? v))
	      (sdl-rect-y-set! (get-rect obj) v))

;;; 关联纹理UI,类似下面的关联实体的UI 2025年6月28日11:31:18
;; (define-sdf-property gobj:纹理		;大部分对象都需要xywh
;;   纹理
;;   'predicate s纹理?
;;   'default-value '默认纹理
;;   )

(define-type 关联纹理UI (UI类型?)		;大部分被渲染对象都需要坐标和规格
  (gobj:纹理)
  )

;;; 坐标纹理UI
(define-type 坐标纹理UI (关联纹理UI? 含坐标UI?)		;大部分被渲染对象都需要坐标和规格
  ()
  )

;;; 文本
(define (构造空游戏字符串)
  (创建字符串-ctcr ""))

(define-sdf-property UI:显示的文本
  显示的文本
  'predicate 游戏字符串?
  'default-supplier 构造空游戏字符串
  )

(define-type 含文本UI (含rectUI?)
  (UI:显示的文本)
  )

(定义匹配度优先广义过程 set-仅字符串! 2 (lambda (a b) (error 'set-仅字符串 "无法设置a的字符串为b" (a b))))

(广义过程扩展 set-仅字符串! ((含文本UI? 含文本UI实例) (string? str))
	      (set-字符串扩展字符-ctcr! (get-显示的文本 含文本UI实例) str))

(广义过程扩展 set-仅字符串! ((含文本UI? 含文本UI实例) (symbol? str))
	      (set-仅字符串! 含文本UI实例 (symbol->string str))
	      )

(define (构造含文本UI name string x y w h color)
  (make-含文本UI 'name name
		 '显示的文本 (创建字符串-ctcr string)
		 'rect (make-sdl-rect x y w h)
		 'color color))

;;; 矩形框的文本
(define-type 文本-矩形UI (含文本UI?) ())

;;;UI可以直接用显示的字符串当name
;; (define 无名UI类型?
;;   ;; 这个没什么用,名称还容易引起混乱 2025年2月28日20:17:18
;;   (conjoin UI类型? (complement object?)))
(广义过程扩展 get-name ((含文本UI? 含文本UI实例))
	      (get-字符串 (get-显示的文本 含文本UI实例)))

;;;按功能增加一层,方便修改和替换
;;; 关联实体object的UI,便于分离逻辑和渲染的部分,如果只有实体的部分,可以通过指令在REPL交互
(define-sdf-property gobj:object
  entity
  'predicate (lambda (x) (or (symbol? x) (object? x)))
  'default-value '未关联实体
  )

(define (实体未关联? 关联实体的对象)
  (and (关联实体的UI? 关联实体的对象)
       (symbol? (get-entity 关联实体的对象)))
  )

(define-type 关联实体的UI (UI类型?)	;这种将其它类型作为属性的方式,会导致要另外实现一套接口来穿透... 2024年10月24日22:52:57
  (gobj:object))

(define 无名关联实体的UI?
  (conjoin 关联实体的UI? (complement object?)))

(广义过程扩展 get-name ((无名关联实体的UI? obj))
	      (get-name (get-entity obj)))

(广义过程扩展 get-description ((关联实体的UI? obj))
	      (get-description (get-entity obj)))

(define-type 文本-矩形关联实体UI (文本-矩形UI? 关联实体的UI?) ())

(广义过程扩展 get-name ((文本-矩形关联实体UI? obj)) ;目前关联实体的UI主要就是装备栏,最好还是直接取ui的文本作为名字 2025年2月28日21:06:46
	      (string->symbol (get-字符串 (get-显示的文本 obj)))) ;转成符号,同其它get-name一致 2025年3月1日10:05:42

;;; 容器
(define-sdf-property 容器:实体
  (实体 adder remover)
  'predicate (is-list-of object?)
  'default-value '())

(define-type 容器 (object?) (容器:实体))

(define adder-modifier (lambda (value values)
			 (lset-adjoin eqv? values value)))

(define remover-modifier (lambda (value values)
			   (delv value values)))

(定义匹配度优先广义过程 clear! 1 (constant-generic-procedure-handler #f))
(广义过程扩展 clear! ((容器? obj))
	      (set-实体! obj '()))

;;; 渲染
(定义链式广义过程 render! 1 (constant-generic-procedure-handler '没有定义渲染方式))
(定义匹配度优先广义过程 render-匹配! 1 (constant-generic-procedure-handler '没有定义的匹配优先渲染方式))

;;; 颜色设置
(广义过程扩展 render! super ((UI类型? obj)) ;渲染必然需要颜色
	      (super obj)
	      (sdl-set-render-draw-color! (current-renderer) (get-r obj) (get-g obj) (get-b obj) (get-a obj))
	      )

;;; 矩形渲染
(广义过程扩展 render! super ((含rectUI? obj)) ;渲染必然需要颜色
	      (super obj)
	      (sdl-render-draw-rect (current-renderer) (get-rect obj))
	      )
;;; 渲染容器
(define-type 被渲染的容器 (容器?) ())

(property-modifier-extend 容器:实体 被渲染的容器? 文本-矩形UI? 实体-adder adder-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 文本-矩形UI? 实体-remover remover-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 容器? 实体-adder adder-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 容器? 实体-remover remover-modifier)

(广义过程扩展 render! super ((被渲染的容器? obj))
	      (super obj)
	      (for-each (lambda (obj)
			  (render! obj))
			(get-实体 obj)))
;;; 文本渲染
(广义过程扩展 render! super ((含文本UI? obj))
	      (super obj)
	      (let ((w (get-w obj))
		    (h (get-h obj))
		    (显示的文本 (get-显示的文本 obj)))
		(矩形区域渲染字符串 (get-game-字体 显示的文本) (current-renderer) (get-字符串 显示的文本)
				    (舍入取整 (+ (get-x obj) (中点 w))) (舍入取整 (get-y obj))
				    (get-r obj) (get-g obj) (get-b obj) (get-a obj)
				    '居中 w h 0 8 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE))
	      )

(广义过程扩展 render-匹配! ((含文本UI? obj)) ;如果不渲染外面的矩形框,这里应该用最匹配的模式,而不是链式 2025年2月24日22:33:33
	      (sdl-set-render-draw-color! (current-renderer) (get-r obj) (get-g obj) (get-b obj) (get-a obj))
	      (let ((w (get-w obj))
		    (h (get-h obj))
		    (显示的文本 (get-显示的文本 obj)))
		(矩形区域渲染字符串 (get-game-字体 显示的文本) (current-renderer) (get-字符串 显示的文本)
				    (舍入取整 (+ (get-x obj) (中点 w))) (舍入取整 (get-y obj))
				    (get-r obj) (get-g obj) (get-b obj) (get-a obj)
				    '居中 w h 0 8 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)))

;;; 更新的局部状态不止是逻辑相关的,还有渲染相关的,冒险游戏把渲染的同别的割裂开了.
;;; 带框文本渲染
(广义过程扩展 render! super ((文本-矩形UI? obj))
	      (super obj)
	      )



;;; 方便print和debug用
(define 计数器 1)
(define 跳出k #f)

(define 标签数据? (disjoin object? UI类型?))
;; (register-predicate! 标签数据? '标签数据)

(define-generic-procedure-handler tagged-data-description
  ;; 扩展了tagging中的广义过程,返回描述 2024年8月10日16:12:15
  (match-args 标签数据?)
  (lambda (object)
    (let ((instance-data (tagged-data-data object)))
      (map (lambda (property)
	     (list (property-name property)
		   ;; (tagged-data-description) 属性之间会循环引用,这样递归展开会死循环...2025年2月28日09:58:05
		   ((instance-data-binding property
					   instance-data))))
	   (instance-data-properties instance-data)))))

(定义匹配度优先广义过程 标签数据->tree 2
			(lambda (data n)
			  data))

(广义过程扩展 标签数据->tree ((标签数据? object) (number? n))
	      (cond ((= n 0) (tagged-data-description object))
		    (else
		     (let ((instance-data (tagged-data-data object)))
		       (map (lambda (property)
			      (list (property-name property)
				    (标签数据->tree ((instance-data-binding property
									    instance-data)) (- n 1))))
			    (instance-data-properties instance-data))))))
