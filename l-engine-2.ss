(library-directories 
 '("D:\\lib" "D:\\lib\\thunderchez-trunk"
   "D:\\lib" "D:\\lib\\scheme-lib\\packages"
   ))

(import
 (chez-sdl lib sdl)
 ;; (srfi s1 lists)
 (srfi s14 char-sets)			;似乎不支持u8
 (srfi s113 sets)			;需要s128,但是依然有问题 set过程配合comparator结果不行 2023年9月3日15:39:07
 (srfi s128 comparators)
 (srfi s27 random-bits)
 (srfi s1 lists)
 (base-ex simple-ex)
 )

(include "D://code//aim4//game-uk//sdf-adven-core.scm")

;;; 位运算扩展
(define (bitwise-or a b)
  (bitwise-xor (bitwise-and a b) (bitwise-xor a b)))

;;; 数学扩展
(define π 3.1415926575)
(define π/2 (/ π 2))			;下
(define -π (- π))			;
(define -π/2 (- π/2))			;上

(define NULL)

(define (弧度->角度 弧度)
  ;; 渲染器构造直接调用了这个...  2023年5月11日22:48:09
  (* 180.0 (/ 弧度 π)))

;;; 初始化
(define SDL已初始化标志 #b00001)

;;; 初始化用的闭包
(define (manager-mk)
  ;; 用来控制初始化 sdl image ... 及 退出 sdl image ...
  (let ((系统初始化标志 0))
    (letrec ((return (lambda (msg)
		       (case msg
			 [(sdl-init)
			  (lambda (flag)
			    (sdl-library-init ;; "SDL2.dll"
			     )
			    (sdl-set-main-ready!)
			    (printf "sdl初始化结果:~d~n" (sdl-init flag)) 	;-1 是有问题
			    (set! 系统初始化标志 #b00001))
			  ]
			 [(image-init)
			  ;; 判断sdl-init是否成功,如果没有的话就先 init-sdl
			  (lambda (flag)
			    (case (bitwise-and 系统初始化标志 #b00001)
			      [(#b00000) 
			       (return 'sdl-init)])
			    (printf "image初始化结果:~d~n"   (begin (sdl-image-library-init)
								    (img-init flag)))
			    (set! 系统初始化标志 (bitwise-or #b00010 系统初始化标志)))
			  ]
			 [(ttf-init)
			  (case (bitwise-and 系统初始化标志 #b00001)
			    [(0) 
			     (return 'sdl-init)])
			  (sdl-ttf-library-init)
			  (ttf-init)
			  (set! 系统初始化标志 (bitwise-or #b00100 系统初始化标志))
			  ]
			 [(mix-init)
			  (lambda (flag)
			    (case (bitwise-and 系统初始化标志 #b00001)
			      [(0) 
			       (return 'sdl-init)])
			    (sdl-mix-library-init)
			    (mix-init flag)
			    (set! 系统初始化标志 (bitwise-or #b01000 系统初始化标志)))
			  ]
			 [(net-init)
			  (case (bitwise-and 系统初始化标志 #b00001)
			    [(0) 
			     (return 'sdl-init)])
			  (sdl-net-library-init)
			  (sdl-net-init)
			  (set! 系统初始化标志 (bitwise-or #b10000 系统初始化标志))
			  ]
			 [(quit)
			  (when (not (= 0 (bitwise-and 系统初始化标志 #b00010)))
			    (img-quit) ;只有image被加载后才能使用,不然no entry
			    )
			  (when (not (= 0 (bitwise-and 系统初始化标志 #b00100)))
			    (ttf-quit) 
			    )
			  (when (not (= 0 (bitwise-and 系统初始化标志 #b01000)))
			    (mix-quit) 
			    )
			   (when (not (= 0 (bitwise-and 系统初始化标志 #b10000)))
			    (sdl-net-quit) 
			    )
			  (sdl-quit)]
			 [else
			  (assertion-violation 'manager-foo "不支持的过程!~s" msg)]))))
      return)))

(定义匹配度优先广义过程 init 2 (constant-generic-procedure-handler #f))	;可能需要链式调用 2024年3月26日22:55:44
(定义匹配度优先广义过程 get-启动标记 1 (constant-generic-procedure-handler #f))

(define gobj:启动标记
  (make-property '启动标记
                 'predicate boolean?
                 'default-value #f)
  )

(define base控制台?
  (make-type 'base控制台 (list gobj:启动标记)))

(define 构造base控制台
  (type-instantiator base控制台?))

;; (广义过程扩展 get-启动标记 base控制台? getter gobj:启动标记
;; 	      ;; (property-getter 'base控制台? gobj:启动标记)
;; 	      )

(define get-启动标记 
  (property-getter gobj:启动标记 base控制台?)
  )

(set-predicate<=! base控制台? object?)

(广义过程扩展 init ((base控制台? obj) (base控制台? 依赖控制台))
	      (when (not (get-启动标记 依赖控制台))
		(begin
		  (sdl-library-init "SDL2.dll")
		  (sdl-set-main-ready!)
		  (printf "sdl初始化结果:~d~n" (sdl-init SDL-INIT-EVERYTHING))))) 	;-1 是有问题)

(define (mg-init manager flag)
  ;; 应该加个sicp 按类型分派,这样一个init过程就可以应用于各种类型的参数而不用手写了
  ((manager 'sdl-init) flag))
(define (mg-quit-sdl manager)
  (manager 'quit))
(define (mg-img-init manager flag)
  ((manager 'image-init) flag))
(define (mg-ttf-init manager)
  (manager 'ttf-init))
(define (mg-mix-init manager flag)
  ((manager 'mix-init) flag))
(define (mg-net-init manager)
  (manager 'net-init))

;;; 窗口类型
(define sdl:window
  (make-property 'window
		 'predicate (lambda (p) (ftype-pointer? SDL_Window p))))

(define sdl:surface		;property
  (make-property 'surface
                 'predicate (lambda (p) (ftype-pointer? SDL_Surface p))))

(define sdl:renderer
  (make-property 'renderer
                 'predicate (lambda (p) (ftype-pointer? SDL_Renderer p))))


(define-type 窗口对象 () (sdl:window ;; sdl:surface
				   sdl:renderer))

(define (创建窗口 titlestr x y w h)
  (let* ((win (sdl-create-window titlestr x y w h))
	 (winrender (sdl-create-renderer win -1
					 SDL-RENDERER-ACCELERATED	;硬件加速,使用GPU
					 SDL-RENDERER-PRESENT-VSYNC)))
    (make-窗口对象 'window win
		   'renderer winrender)))

(定义匹配度优先广义过程 get-表面 1 '默认表面)
(广义过程扩展 get-表面 ((窗口对象? obj))
	      (sdl-get-window-surface (get-window obj)))

;;; 游戏循环
;;; 由于需要调用 类型的值,所以这部分直接写成库文件的一部分  -- 230117
;;; 见 chez-sdl sdl-extend.sls
(define (游戏循环 game-obj core-foo)
  ;; 为了避免出现1的那个版本,每套局部都要对各个事件对应的hash-table进行赋值的情况;
  ;; 同时为了避免closure中每增添一个事件就要修改源代码的情况
  ;; 意识到了事件和状态的本质都是谓词之后,通过给事件循环的构造器返回一个传入函数,将传入的函数应用到事件类型上得到了现在的2.0  -- 2023年2月15日21:03:31
  ;; 因为图形拖动遇到了问题,意识到了原作者的一堆事件谓词之后,不需要自己手工访问 SDL_Event,有了现在的3.0--- 2023年2月16日20:11:20
  ;; 由于昨天的进步,不需要放进库文件了... -- 2023年2月17日17:25:40
  ;; 固定更新时间步长,动态渲染?游戏设计模式一书提到的方式  2024年7月28日16:09:10
  ;; (let ((上一时间戳 (sdl-get-ticks))
  ;; 	(时间间隔 0)
  ;; 	(累积时间间隔 0)))
  (let loop ((随便什么 1))
    ;; (set! 时间间隔 (- 时间戳 上一时间戳))
    ;; (set! 上一时间戳 时间戳)
    ;; (set! 累积时间间隔 (+ 累积时间间隔 时间间隔))		;会在game-eval中减去每帧时长然后赋值给自己...
    (sdl-poll-event)
    (if (sdl-event-quit?)
	(begin
	  (printf "游戏循环结束!~n")
	  ((get-quit-foo game-obj)))
	(begin
	  ;; (game-read )
	  (core-foo 'msg)
	  ;; (game-print )
	  (loop 1)
	  ))
    )
    )

;;; 游戏类型
(define gobj:窗口对象
  (make-property '窗口对象
                 'predicate 窗口对象?))

(define gobj:每帧毫秒
  (make-property '每帧毫秒
                 'predicate real?))

(define gobj:quit-foo
  (make-property 'quit-foo
                 'predicate procedure?))

;; (define )

(define-type game () (gobj:窗口对象 gobj:每帧毫秒 gobj:quit-foo))

(define (创建game sdl-flag img-flag mix-flag 每帧时长 标题str 窗口x 窗口y 窗口宽 窗口高)
  ;; 这里还是用了闭包 2024年7月28日21:42:18
  (let ((manager (manager-mk)))
    ;; 这一堆init应该单独分开,
    (mg-init manager sdl-flag)
    (mg-img-init manager img-flag)
    (mg-ttf-init manager)
    (mg-mix-init manager mix-flag)
    (mg-net-init manager)
    (make-game '窗口对象 (创建窗口 标题str 窗口x 窗口y 窗口宽 窗口高)
	       '每帧毫秒 每帧时长
	       'quit-foo (lambda () (collect)
				 (sdl-free-garbage)
				 (mg-quit-sdl manager)))
    )
  )

(define (game-window-get game)
  (get-window (get-窗口对象 game)))

(define (game-render-get game)		;之后换成广义过程的版本 2024年7月29日00:44:53
  (get-renderer (get-窗口对象 game)))

(define (game-surface-get game)
  (sdl-get-window-surface (get-窗口对象 game)))

(define (game无事件时的过程-set! game 无事件时过程)
  ((game 'game默认过程-set!) 无事件时过程))

;;; 计时器类型
;; 生成敌机的这种可以换成协程 2024年7月29日00:45:22
(define gobj:暂停标记
  (make-property '暂停标记
                 'predicate boolean?
                 'default-value #f)
  )

(define gobj:不计时间隔
  (make-property '不计时间隔
                 'predicate real?
                 'default-value 0))

(define gobj:累积时间
  (make-property '累积时间
                 'predicate real?
                 'default-value 0)
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
;;; 一部分不依赖sdl的,可以分离成物理,输入,渲染等部分的内容
(define (sdl-map foo ls)
  (cond ((null? ls) #t)
	(else
	 (foo (car ls))
	 (sdl-map foo (cdr ls)))))

(define (rct多矩形碰撞? rcdlsa rcdlsb)
  (call/cc
   (lambda (k)	 ;; (sound (mix-load-wav 贪吃蛇音效path))
     (let loop ((lsa rcdlsa))
       (cond ((null? lsa) #f)
	     (else
	      (let loop2 ((lsb rcdlsb))
		(cond ((null? lsb) (loop (cdr lsa)))
		      ((let ((sprite-a (car lsa))
			     (sprite-b (car lsb)))
			 (rct矩形碰撞? (make-sdl-rect (rcdx坐标 sprite-a) (rcdy坐标 sprite-a) (rcd渲染宽度 sprite-a) (rcd渲染高度 sprite-b))
				     (make-sdl-rect (rcdx坐标 sprite-b) (rcdy坐标 sprite-b) (rcd渲染宽度 sprite-b) (rcd渲染高度 sprite-b))))
		       (k #t))
		      (else
		       (loop2 (cdr lsb))))))))))) 

(define (rct矩形碰撞? sdl-recta sdl-rectb)
  (let ((ax (sdl-rect-x sdl-recta))
	(aw (sdl-rect-w sdl-recta))
	(bx (sdl-rect-x sdl-rectb))
	(bw (sdl-rect-w sdl-rectb))
	(ay (sdl-rect-y sdl-recta))
	(ah (sdl-rect-h sdl-recta))
	(by (sdl-rect-y sdl-rectb))
	(bh (sdl-rect-h sdl-rectb))
	)
    ;; 使用<=是因为贪吃蛇的边界重合了....使用顺延坐标的同时改变移动速度即可,增大一个像素
    (碰撞? ax ay aw ah bx by bw bh)))

(define (碰撞0? x1 y1 w1 h1 x2 y2 w2 h2)
  (cond ((or (< (+ x1 w1) x2)
	      (< (+ x2 w2) x1)
	      (< (+ y1 h1) y2)
	      (< (+ y2 h2) y1)
	      ) #f)
	(else #t)))

(define (碰撞help a1 a1增量 a2 a2增量)
  (< (max a1 a2) (min (+ a1 a1增量) (+ a2 a2增量))))

(define (碰撞? x1 y1 w1 h1 x2 y2 w2 h2)
  (and (碰撞help x1 w1 x2 w2)
       (碰撞help y1 h1 y2 h2)
       ))

(define (一多碰撞? x1 y1 w1 h1 xywhls)
  (cond ((null? xywhls) #f)
	(else
	 (or
	  (apply (lambda (x2 y2 w2 h2)
		   (碰撞? x1 y1 w1 h1 x2 y2 w2 h2))
		 (car xywhls)
		 )
	  (一多碰撞? x1 y1 w1 h1 (cdr xywhls))))))

(define (多矩形碰撞? xywhlsa xywhlsb)
  (cond ((null? xywhlsa) #f)
	(else
	 (or
	  (apply (lambda (x y w h)
		   (一多碰撞? x y w h xywhlsb))
		 (car xywhlsa))
	  (多矩形碰撞? (cdr xywhlsa) xywhlsb))))) 

(define (渲染器 thunk render r g b a)
  ;; prepareScene
  (sdl-set-render-draw-color! render r g b a)
  (sdl-render-clear render)
  (thunk)
  ;; presentScene
  (sdl-render-present render)
  )

;;;但凡要显示,必然有x,y
(define gobj:坐标ls
  (make-property '坐标
                 'predicate complex?
                 'default-value 0)
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

;;;
(define (纹理信息-w 纹理信息)
  (cond ((null? 纹理信息)
	 (assertion-violation '纹理信息-w "纹理信息-w获取失败!" 纹理信息))
	(else
	 (caddr 纹理信息))))

(define (纹理信息-h 纹理信息)
  (cond ((null? 纹理信息)
	 (assertion-violation '纹理信息-h "纹理信息-h获取失败!" 纹理信息))
	(else
	 (cadddr 纹理信息))
	))

(define gobj:纹理
  (make-property '纹理
		 'predicate any-object?		;因为目前没有实现判断C的指针是某个类型的方法,这里需要一个对任何对象都返回#t的谓词
		 'default (lambda () '无效的情况)))

(define-type 纹理 () (gobj:纹理))

(define (s纹理-mk render path)
  (make-纹理 '纹理 (img-load-texture render path)))

(define (s纹理-加载图片! 纹理 render path)
  (set-纹理! 纹理 (img-load-texture render path)))

(define (s纹理-纹理信息get 纹理)
  (sdl-query-texture (get-纹理 纹理)))

(定义匹配度优先广义过程 s纹理-按纹理规格blit  4 (constant-generic-procedure-handler #f))
(广义过程扩展 s纹理-按纹理规格blit ((any-object? render) (纹理? 纹理) (integer? x坐标) (integer? y坐标))
	      (let ((纹理信息 (s纹理-纹理信息get 纹理)))
		(sdl-render-copy render (get-纹理 纹理) NULL (make-sdl-rect x坐标 y坐标 (纹理信息-w 纹理信息) (纹理信息-h 纹理信息)))))	;用下面这个替换这个的内容 2024年4月24日19:58:31

(定义匹配度优先广义过程 框选纹理blit 4 (constant-generic-procedure-handler #f))
(广义过程扩展 框选纹理blit ((any-object? render) (坐标对象? obj) (纹理? 纹理) (sdl-rect? 取用框))
	      (sdl-render-copy render (get-纹理 纹理) 取用框
			       (make-sdl-rect (get-x坐标 obj) (get-y坐标 obj) (sdl-rect-w 取用框) (sdl-rect-h 取用框))))

(define (字串纹理-mk ttf-path  fontsize)
  ;; 即便是生成了纹理,颜色依然随机  2023年3月16日22:36:17
  ;; 在3月21解决了颜色随机问题,解决思路可能在某个注释  2023年8月9日19:50:18
  ;; 字体的路径是第一个大范围,其次是某个区域内连续的文本,只要属性(字号,颜色)一致,就可以单独变成对象
  ;; 按愿望思维,应该有个构造器,还有一堆析取器提取需要的部分(fong,surface还有texture),提取出来的东西传入texture进行渲染,还需要留出给内部构造set!的部分.
  (let* ((字体 (ttf-open-font ttf-path fontsize))
	 (字体表面 '字体表面) ;这个初始化可能没有意义,不如渲染前先set!  2023年4月5日21:14:34
	 (字体宽  '字体宽)
	 (字体高 '字体高)
	 (字体纹理 '字体纹理))
    (letrec ((res (lambda (m)
		    (cond
		     [(equal? m '字体表面-set)
		      (lambda (ttf-render-fun str sdl-color)
			(set! 字体表面 (ttf-render ttf-render-fun 字体 str sdl-color))
			(set! 字体宽 (ftype-ref SDL_Surface (w) 字体表面))
			(set! 字体高 (ftype-ref SDL_Surface (h) 字体表面))
			)]
		     [(equal? m '字体纹理-set)
		      (lambda (render)
			(if 字体表面
			    (set! 字体纹理 (sdl-create-texture-from-surface render 字体表面))
			    ))]
		     [(equal? m '字体纹理-get) 字体纹理]
		     [(equal? m '字体纹理宽) 字体宽]
		     [(equal? m '字体纹理高) 字体高]
		     [(equal? m '改变字体大小)
		      ;; 可能可以通过缩放实现  2023年8月23日11:59:19
		      (ttf-close-font 字体)
		      
		      ]
		     ((equal? m 'str更新到texture)
		      ;; 这种方法每次渲染不同的字符串,都需要重新从font构造surface以及texture
		      ;; 按照另一种思路,将每个字符单独bilt,然后构造图集,保存索引框,渲染的时候按照索引框访问渲染.  2023年8月23日23:11:05
		      (lambda (str ttf-render-fun render r g b a)
			((res '字体表面-set) ttf-render-fun str (make-sdl-color r g b a))
			((res '字体纹理-set) render)
			))
		     [(equal? m '渲染字体)
		      (lambda (render x y)
			(sdl-render-copy-ex render 字体纹理 NULL (make-sdl-rect x y 字体宽 字体高) 0.0 NULL SDL_FLIP_NONE)
			)]
		     [else
		      (assertion-violation 字串纹理 "无效的消息" m)
		      ])
		    )
		  ))
      res
      )
    )
  )

(define (字串纹理-字串更新! 字串纹理 str ttf-render-fun render r g b a)
  ((字串纹理 'str更新到texture) str ttf-render-fun render r g b a))

(define (字串纹理-字体高 字串纹理)
  (字串纹理 '字体纹理高))

(define (字串纹理-字体宽 字串纹理)
  (字串纹理 '字体纹理宽))

(define (字串纹理渲染 字串纹理 render x y)
  ((字串纹理 '渲染字体) render x y))

(define (编码区间->个数 区间)
  (+ 1 (apply - (reverse 区间))))

(define (编码区间ls->字符个数 编码区间ls)
  (fold-left (lambda (num 区间)
	       (+ num (编码区间->个数 区间)))
	     0 编码区间ls))

(define (能覆盖全部字符的正方形边长 字符个数)
  (ceiling (sqrt 字符个数)))

(define ASCII码区间 '(0 127))
(define 中文区间 '(#x4e00 #x9FA5))
(define 六十四卦区间 '(#x4DC0 #x4DFF))
;;; parallelrealities的文字渲染方法
(define (sttf-管理 color-depth)
  ;; 字体是一个维度,从ttf到surface,背景透明处理,将每个字符blit到大的surface之后再转texture的 集邮册 是另一个维度,存的值是 图集中字符对应图像的坐标
  ;; 宽度不同的处理,涉及unicode字符的处理.
  ;; 本质上是构造数据结构和对应的构造过程,修改过程,析取过程,应该会涉及到部分局部环境
  ;; 需要一个二维的ht,维度分别是font,字符
  ;; 对比将全部字符存入一个矩形,ht只保存坐标的情形 和 将字符的矩形存入ht 两种情况占用的内存,初始化的消耗,修改的消耗,读取的消耗
  ;; 其实可以在原来思路的基础上,如果遇到了字符数量 x size 大于surface的情况,可以构造一个新的,然后将旧的blit上去,只在ht中保存坐标,毕竟逐个保存surface,会有一堆重复信息占用内存  2023年8月19日12:28:23
  ;; 这东西的本质就是EOPL.CH2的env,构造器和观察器,构造器也是分成两个,空的情形,还有按font和char加入的情形.观察器目前只有取出的情形,输入env font char 得到 val
  ;; font-texture-size 决定了字符图集的规格,初始化成一个方块
  ;; 最新问题是使用中扩展,会读取已有的集邮册texture,但是无法转换成surface,然后blit新的字符上去 2024年6月23日21:55:42
  (let* ((fontht (make-hashtable string-hash string=?))	;二维hashtable,存储不同字体对应的图集ht
	 )
    (lambda (m)
      (case m
	[(glyphs-extendbycode)
	 (lambda (fontpath code-range-ls render size)	;这个size扔到初始化比较好
	   ;; 确保输入的str没有重复的 (lset-difference string=? (string->list str) (hashtable-keys glyphsht))
	   (let* ((font (ttf-open-font fontpath size))
		  (whls (ttf-size-text-m font "0" 'utf8))
		  (边长 (begin (测试输出 whls) (exact (能覆盖全部字符的正方形边长 (编码区间ls->字符个数 code-range-ls)))))
		  (图集宽 (* 2 边长 (car whls)))
		  (图集高 (* 2 边长 (cadr whls)) )
		  (surface (sdl-create-rgb-surface 0 图集宽 图集高 color-depth 0 0 0 #xff))	;这个规格要么根据字符数量毛估,要么超过范围后扩大surface,blit已有的上去 2024年6月24日13:12:13
		  ;; 毛估需要得到字符的wh信息
		  (图集-glyphsht-ls (hashtable-ref fontht fontpath (list (sdl-create-rgb-surface 0 图集宽 图集高 color-depth 0 0 0 #xff)
									 (make-hashtable equal-hash equal?))))	;每个字体对应不同的hash,默认返回一个空白图形集surface、字符和格子的ht
		 
		  (glyphsht (cadr 图集-glyphsht-ls))
		  )
	     (when (< 0 (sdl-set-color-key! surface SDL-TRUE (sdl-map-rgba (ftype-ref SDL_Surface (format) surface) 0 0 0 0)))
	       (error 'sttf-扩展 "图集颜色key设置失败")) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
	     (set! surface (sdl-convert-surface surface
						(ftype-ref SDL_Surface (format)
							   (ttf-render ttf-render-utf8-blended font "0" (make-sdl-color 0 0 0 #xff)))
						0)) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
	     (let loop0 ((区间ls code-range-ls)
			 (新区间x 0)
			 (新区间y 0)
		)
	       (cond ((null? 区间ls)
		      ;; (> 字符编码 (cadr 字符编码ls))
		      (begin
			;; (sdl-save-bmp surface "字符集.bmp")
			(hashtable-set!
			 fontht fontpath
			 (list (sdl-create-texture-from-surface
				render
				surface
				)
			       glyphsht
			       ))))
		     (else
		      (let ((上限 (cadar 区间ls)))
			(let loop ((字符编码 (caar 区间ls))
				   (索引框x 新区间x)
				   (索引框y 新区间y))
			  (cond
			   ((> 字符编码 上限)
			    (loop0 (cdr 区间ls)
				   索引框x
				   索引框y)
			    )
			   (else
			    (begin
			      (let* ((字符 (integer->char 字符编码))
				     (text (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff)))  ;这个颜色应该也可以参数化
				     (whls (ttf-size-text-m font (string 字符) 'utf8))
				     (字符w (car whls))
				     (字符h (cadr whls))
				     (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
				     )
				;; (display whls)
				;; (newline)
				;; (sdl-save-bmp text (string 字符))
				(sdl-blit-surface text (make-sdl-rect 0 0 字符w 字符h)
						  surface 索引框)	;集邮的过程,text保存下来都是背景透明的字,但是blit到surface再保存,就是空白
				(hashtable-set! glyphsht 字符 索引框)	;按字符取得索引框,然后框出来对应的texture  2023年8月26日22:25:45
				(let ((x+ (+ 索引框x 字符w)))
				  (if (>=  x+ 图集宽) ;换行
				      (let ((y+ (+ 索引框y 字符h 1)))
					(if (>= (+ y+ 字符h) 图集高) ;超出下限
					    (error 'sttf "字符图集高度不足" (list y+ 图集高))	;必要时更新surface,将原有图集B到一个更大的空图集上  2023年8月26日23:16:35
					    (loop (+ 字符编码 1)
						  0
						  y+)))
				      (loop (+ 1 字符编码) ;不换行的情况 
					    x+
					    索引框y))))))))))))
	     ))]
	[(glyphs-extend)
	 (lambda (fontpath str render size)	;这个size扔到初始化比较好
	   ;; 确保输入的str没有重复的 (lset-difference string=? (string->list str) (hashtable-keys glyphsht))
	   (let* (
		  (surface (sdl-create-rgb-surface 0 图集规格 图集规格 color-depth 0 0 0 #xff))
		  (图集-glyphsht-ls (hashtable-ref fontht fontpath (list (sdl-create-rgb-surface 0 图集规格 图集规格 color-depth 0 0 0 #xff) (make-hashtable equal-hash equal?) (list 0 0))))	;每个字体对应不同的hash,默认返回一个空白图形集surface、第一个空白格子的坐标、字符和格子的ht
		  ;; (surface (car 图集-glyphsht-ls))
		  (第一个空白格子的坐标 (caddr 图集-glyphsht-ls))
		  (glyphsht (cadr 图集-glyphsht-ls))
		  )
	     (let ((字符池增量 (lset-difference char=? (集合化 (string->list str)) (hashtable-keys glyphsht))))
	       (if (null? 字符池增量)	;防止频繁加载font 2024年6月23日22:47:27
		   '无需扩展
		   (let ((font (ttf-open-font fontpath size)))
		     ;; (测试输出 1)
		     ;; (测试输出 surface)
		     (when (< 0 (sdl-set-color-key! surface SDL-TRUE (sdl-map-rgba (ftype-ref SDL_Surface (format) surface) 0 0 0 0)))
		       (error 'sttf-扩展 "图集颜色key设置失败")) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
		     ;; (测试输出 2)
		     ;; (测试输出 surface)
		     (when (equal? 第一个空白格子的坐标 '(0 0))
		       (set! surface (sdl-convert-surface surface
							  (ftype-ref SDL_Surface (format)
								     (ttf-render ttf-render-utf8-blended font "0" (make-sdl-color 0 0 0 #xff)))
							  0))) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
		     (let loop ((字符池增量 字符池增量)
				(索引框x (car 第一个空白格子的坐标))
				(索引框y (cadr 第一个空白格子的坐标)))
		       (cond ((null? 字符池增量)
			      (begin
				;; (sdl-save-bmp surface "字符集.bmp")
				(hashtable-set!
				 fontht fontpath
				 (list (sdl-create-texture-from-surface
					render
					surface
					)
				       glyphsht
				       (list 索引框x 索引框y)
				       ))))
			     (else
			      (begin
				(let* ((字符 (car 字符池增量))
				       (text (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff)))  ;这个颜色应该也可以参数化
				       (whls (ttf-size-text-m font (string 字符) 'utf8))
				       (字符w (car whls))
				       (字符h (cadr whls))
				       (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
				       )
				  ;; (display whls)
				  ;; (newline)
				  ;; (sdl-save-bmp text (string 字符))
				  (sdl-blit-surface text (make-sdl-rect 0 0 字符w 字符h)
						    surface 索引框)	;集邮的过程,text保存下来都是背景透明的字,但是blit到surface再保存,就是空白
				  (hashtable-set! glyphsht 字符 索引框)	;按字符取得索引框,然后框出来对应的texture  2023年8月26日22:25:45
				  (let ((x+ (+ 索引框x 字符w)))
				    (if (>=  x+ 图集规格) ;换行
					(let ((y+ (+ 索引框y 字符h 1)))
					  (if (>= (+ y+ 字符h) 索引框y) ;超出下限
					      (error 'sttf "字符图集高度不足" 图集规格)	;必要时更新surface,将原有图集B到一个更大的空图集上  2023年8月26日23:16:35
					      (loop (cdr 字符池增量)
						    0
						    y+)))
					(loop (cdr 字符池增量)
					      x+
					      索引框y)))))))))))
	     ))]
	[(glyphs-get)
	 (lambda (fontpath char)
	   (hashtable-ref (hashtable-ref fontht fontpath (make-hashtable equal-hash equal?)) char #f))]
	[(glyphs-图集索引框ht-get)
	 (lambda (fontpath)
	   (hashtable-ref fontht fontpath (cons '() '())))]))))

(define (sttf-字体字符图集初始化 sttf fontpath code-range-ls render size)
  ((sttf 'glyphs-extendbycode) fontpath code-range-ls render size))

(define (sttf-扩展 sttf fontpath str render 字体加载尺寸)
  ((sttf 'glyphs-extend)  fontpath str render 字体加载尺寸)
  )

(define (sttf-图集-get sttf fontpath )
  ((sttf 'glyphs-图集索引框ht-get) fontpath))

(define (sttf字符串渲染 sttf render font string init-x init-y
			r g b
			对齐方式 字符框宽度 字符x间距系数 字符y间距系数 显示方式
			w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag)
  ;; 为了实现不同的对齐方式,目前必须要遍历两次
  ;; 如果不是按行渲染,而是斜着渲染,会有很大的不同
  
  (let ((text-htp (sttf-图集-get sttf font)))
    (when (null? (car text-htp))
      (error '字符串渲染 "图集为空" font))
    (let* ((text (car text-htp))
	   (索引框ht (cadr text-htp))
	   (字符框规格 (sttf字符框规格get sttf string font)) ;这个过程计算了整个字符串的宽和不换行的长 2024年6月3日12:22:47
	   (字符框宽 (car 字符框规格))
	   (x (case 对齐方式
		      [(居中) (- init-x (/ 字符框宽 2))] ;x是中间位置的坐标
		      [(右对齐) (- init-x 字符框宽)]
		      [else init-x]
		      ))
	   (渲染函数 (case 显示方式
		      [(仅w) (lambda (render 纹理 索引框 sdl矩形 旋转角度 中心点 镜像flag) '())]
		      [(字符渲染) sdl-render-copy-ex]
		      [else sdl-render-copy-ex])))
      (or (= 0 (sdl-set-texture-color-mod! text r g b))
	  (error 'sttf字符串渲染 "颜色设置失败"))
      (let loop ((charls (string->list string))
		 (w-acc 0)
		 (y init-y)) 
	(cond ((null? charls) (list (+ x w-acc) y))	 ;字符串是否为空
	      ((> y 窗口高) (error 'sttf字符串渲染 "渲染文本框h超过窗口h" y)) ;防止y因为单个字符宽度超过限制而无限增大. 2023年9月3日15:51:05
	      (else
	       ;; 在ht存矩形有个2好处,1是可以直接被这里调用,2是不用来回访问获取纹理和set-color-mod,减少计算开销
	       (let* ((当前字符 (car charls))
		      (索引框 (hashtable-ref 索引框ht 当前字符 #f)))
		 ;; (when (not 索引框)
		 ;;   (error 'sttf字符串渲染 "未检索到的字符" char))
		 (let ((w (* w缩放系数 (sdl-rect-w 索引框))) ;w可以通过显示foo的返回值得到,但是为了在循坏外只判定一次同时不重复实现 单字符渲染的代码--不行,要判断宽度是否越界...
		       (h (* h缩放系数 (sdl-rect-h 索引框))))
		   (cond ((< (* w缩放系数 (+ w-acc w)) 字符框宽度) ;不换行的情况,第一个字符就超过宽度限制应当报错,最好在循环开始前就判断一下,目前这样会导致无限循环下去  2023年9月3日15:49:33
			  (渲染函数 render text 索引框 (make-sdl-rect (+ x (* 字符x间距系数 w-acc)) y w h) 字符旋转角度 中心点 镜像flag)
			  (loop (cdr charls)
				(+ w-acc w)
				y))
			 ;; ((null? (cdr charls))	;最后一个字符的渲染,感觉是为了计算返回值搞出来的畸形,其实不需要. 2024年6月3日12:06:04
			 ;;  (let ((减一列区域宽度 (* 字符x间距系数 w-acc)))
			 ;;    (map + (list 减一列区域宽度 y)
			 ;; 	 (sttf字符渲染 (car charls) 索引框ht  render text (+ x 减一列区域宽度) y w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag 渲染函数))
			 ;;    )
			 ;;  )
			 (else
			  (loop charls	;换行之后再渲染
				0
				(+ y (* 字符y间距系数 (sdl-rect-h 索引框)))))))))))
      )
    ))

;; (define (sttf字符渲染 字符 索引框ht render 纹理 x y w缩放系数 h缩放系数 旋转角度 中心点 镜像flag 渲染函数)
;;   ;; 这里因为涉及两个版本,需要返回过程
;;   (let* ((索引框 (hashtable-ref 索引框ht 字符 #f))))
 
;;   (let ((w (* w缩放系数 (sdl-rect-w 索引框)))
;; 	(h (* h缩放系数 (sdl-rect-h 索引框))))
    
;;     (渲染函数 render 纹理 索引框 (make-sdl-rect x y w h) 旋转角度 中心点 镜像flag)
;;     )
;;   (list w h))

(define (sttf字符串框渲染 sttf render font str x y r g b 对齐方式 最大宽度)
  (case 最大宽度
    [(0) (sttf字符串单行渲染 sttf render font str x y r g b 对齐方式)]
    [else
     (sttf字符串换行渲染 sttf render font str x y r g b 对齐方式 最大宽度)]))

(define 一般单行字符宽度上限 100000)

(define (sttf字符渲染进矩形区域 sttf render font str init-x init-y r g b 对齐方式 字符框宽度 字符旋转角度 中心点 镜像flag)
  ;; (sttf-扩展 sttf 字体path str render 32) ;放到这里试试 2024年6月17日20:02:48 每次渲染都要扩展一遍,卡到爆炸 2024年6月17日20:03:40
  (let* ((whls (sttf字符串渲染 sttf render font str init-x init-y r g b 对齐方式 一般单行字符宽度上限 1 1 '仅w
			       1 1 字符旋转角度 中心点 镜像flag))
	 (字符串宽度 (car whls)))
    ;; (display whls)
    ;; (newline)
    (cond ((>  字符串宽度 字符框宽度)
	   (测试输出 (list str 字符串宽度))
	   (let ((w缩放系数 (/ 字符框宽度 字符串宽度)))
	     (sttf字符串渲染 sttf render font str init-x init-y r g b 对齐方式 字符框宽度 w缩放系数 w缩放系数 '字符渲染
			     w缩放系数 w缩放系数 字符旋转角度 中心点 镜像flag)))
	  (else
	   (sttf字符串渲染 sttf render font str init-x init-y r g b 对齐方式 字符框宽度 1 1 '字符渲染
			     1 1 字符旋转角度 中心点 镜像flag))))
  )

(define (sttf字符框规格get sttf str font)
  (let ((索引框ht (cadr (sttf-图集-get sttf font))))
    (when (null? 索引框ht)
      (error '字符框规格get "font下图集为空" font))
    (let loop ((charls (string->list str))
	       (w 0)
	       (h 0))
      (cond ((null? charls) (list w h))
	    (else
	     (let* ((rect (hashtable-ref 索引框ht (car charls) #f)))
	       (when (not rect)
		 (error 'sttf字符串渲染 "未检索到的字符" (car charls)))
	       (let ((charw (sdl-rect-w rect))
		     (charh (sdl-rect-h rect)))
		 (loop (cdr charls)
		       (+ w charw)
		       (max h charh)))))))
    ))
;;; 

;;; 44100 AUDIO_S16LSB 2 2048
(define (s音频-mk 音乐pathls 音效pathls 频率 编码flag 频道数量 chunksize 初始音量)
  (mix-open-audio 频率 编码flag 频道数量 chunksize)
  (let* (
	 (音乐音量 初始音量)
	 (music (mix-load-mus (car 音乐pathls)))
	 (sound (mix-load-wav (car 音效pathls))) ;单独分开运行库代码就可以加载这个函数,但是放到这里就提示sdl未提供
	 (播放位置 0)
	 (上一播放位置 播放位置)
	 (列表长度 (length 音乐pathls)))
    (sdl-guardian music)
    (sdl-guardian sound)
    (lambda (m)
      (cond
       ((equal? m 'bgm启动/暂停)
	(cond ((= (mix-playing-music) 0)
	       (mix-play-music music -1))
	      ((= (mix-paused-music) 1)
	       (mix-resume-music))
	      (else
	       (mix-pause-music)))
	)
       ((and (equal? m 'bgm音量增加)
	     (<= 音乐音量 128))
	(set! 音乐音量 (+ 1 音乐音量)))
       ((and (equal? m 'bgm音量降低)
	     (>=  音乐音量 0))
	(set! 音乐音量 (-  音乐音量 1)))
       ((equal? m 'bgm下一首)
	(set! 播放位置 (mod (+ 播放位置 1) 列表长度))
	)
       ((equal? m 'bgm上一首)
	(set! 播放位置 (mod (- 播放位置 1) 列表长度))
	)
       ((equal? m '音效播放)
	(mix-play-channel-timed -1 sound 0 -1))
       ((sdl-event-quit?)
	;; 这个直接通过事件管理不知道有没有效果,确实提供了新的思路.
	;; (mix-free-music music)
	;; (mix-free-chunk sound)
	'()
	)
       (else
	(assertion-violation '音频管理 "接受了不支持的参数!" m)))
      (mix-volume-music 音乐音量)
      (cond ((not (= 上一播放位置 播放位置))
	     (mix-halt-music)
	     ;; (mix-free-music music)
	     (set! music (mix-load-mus (list-ref 音乐pathls 播放位置)))
	     (mix-play-music music -1)
	     (set! 上一播放位置 播放位置)
	     )
	    (else '()))
      )
    )
  )

(define (音频-bgm启停 音频)
  (音频 'bgm启动/暂停))
(define (音频-bgm音量增加 音频)
  (音频 'bgm音量增加))
(define (音频-bgm音量降低 音频)
  (音频 'bgm音量降低))
(define (音频-bgm下一首 音频)
  (音频 'bgm下一首))
(define (音频-音效播放 音频)
  (音频 '音效播放))

;;; 音频部分
(define gobj:music
  (make-property '音乐
		 'predicate any-object?		;
		 'default '无效的音乐))


(define-type s音乐 () (gobj:music))
(define (创建音乐 path)
  (make-s音乐 '音乐 (mix-load-mus path)))

(定义匹配度优先广义过程 播放/暂停 1 (constant-generic-procedure-handler #f))
(广义过程扩展 播放/暂停 ((s音乐? obj))
	      (let ((音乐 (get-s音乐 obj)))
		(cond ((= (mix-playing-music) 0)
		       (mix-play-music 音乐 -1))
		      ((= (mix-paused-music) 1)
		       (mix-resume-music))
		      (else
		       (mix-pause-music)))))

(define gobj:sound
  (make-property '音效
		 'predicate any-object?
		 'default '无效的音效))

(define-type s音效 () (gobj:sound))
(define (创建音效 path)
  (make-s音效 '音效 (mix-load-wav path))) ;遇到多个音效的情况,这样还不够 2024年5月9日19:31:53


(define gobj:音效ht
  (make-property '音效__
		 'predicate hashtable?
		 'default (make-eqv-hashtable)))

;;; 
(define 表面显示
  ;; 应该传个窗口,从窗口得到窗口表面,而不是把下一道和前一道都扔进来
  (case-lambda
    [(图像s表面 窗口s表面 窗口)
     (表面显示 图像s表面 0 0 窗口s表面 0 0 窗口)]
     [(图像s表面 图像遮罩x 图像遮罩y 窗口s表面 显示x 显示y 窗口)
     (let ((矩形a (make-sdl-rect 图像遮罩x 图像遮罩y (s表面-w 图像s表面) (s表面-h 图像s表面)))
	   (矩形b (make-sdl-rect 显示x 显示y (s表面-w 窗口s表面) (s表面-h 窗口s表面))))
       ;; 下面这句由于像素修改的需要,窗口表面不使用转码前的初始表面的话,会什么都不显示,原因不明  2023年5月7日23:26:28
       (表面显示 (s表面-表面 图像s表面) 矩形a (s表面-init表面 窗口s表面) 矩形b 窗口)
       )]
    [(图像表面 矩形a 窗口表面 矩形b 窗口)
     (sdl-blit-surface 图像表面 矩形a 窗口表面 矩形b)
     (sdl-update-window-surface 窗口)]
    )
  )

(define (像素值numls->str ls)
  (map (lambda (num)
	 (number->string num 16))
       ls))

(define (s表面mk init表面)
  ;; 必须要是指针对象才能修改像素值,直接ftype-set! ftype-ref的返回值不行
  (let* ((表面 (sdl-convert-surface-format init表面 SDL_PIXELFORMAT_RGBA32 0))  ;由于传入的表面 pixelformat是24位的,修改像素有些麻烦,所以最后还是用了去年的策略,先转换格式  2023年5月7日20:34:24
	 (像素指针 (make-ftype-pointer unsigned-32 (ftype-ref SDL_Surface (pixels) 表面))))
    ;; (sdl-guardian init表面)
    ;; (sdl-guardian 表面)
    (sdl-guardian 像素指针)
    (letrec ((res
	      (lambda (m)
		(cond 
		 [(equal? m 'w)
		  (ftype-ref SDL_Surface (w) 表面)]
		 [(equal? m 'h)
		  (ftype-ref SDL_Surface (h) 表面)]
		 [(equal? m 'pixels)
		  (ftype-ref SDL_Surface (pixels) 表面)]
		 [(equal? m 'format)
		  (ftype-ref SDL_Surface (format) 表面)]
		 [(equal? m '像素格式)
		  (ftype-ref SDL_PixelFormat (format) (res 'format))]
		 [(equal? m 'init表面)
		  init表面]
		 [(equal? m '表面)
		  表面]
		 [(equal? m '像素)
		  (lambda (偏移量)
		    (ftype-ref unsigned-32 () 像素指针 偏移量))
		  ]
		 [(equal? m 'pixels->ls)
		  (lambda (长度)
		    (let loop ((i 0)
			       (acc '()))
		      (cond ((= i 长度) (reverse acc))
			    (else
			     (loop (+ i 1)
				   (cons (ftype-ref unsigned-32 () 像素指针 i) acc))))
		      ))]
		 [(equal? m 'pixels->bytels)
		  (lambda (长度)
		    (let loop ((i 0)
			       (acc '()))
		      (cond ((= i 长度) (reverse acc))
			    (else
			     (loop (+ i 1)
				   (cons (foreign-ref 'unsigned-8 (ftype-ref SDL_Surface (pixels) 表面) i) acc))))
		      ))]
		 [(equal? m 'pitch)
		  (ftype-ref SDL_Surface (pitch) 表面)]
		 [(equal? m '像素set!)
		  (lambda (偏移量 像素值)
		    (ftype-set! unsigned-32 () 像素指针 偏移量 像素值))]
		 [(equal? m '像素map-set!)
		  (lambda (偏移量 r g b a)
		    (ftype-set! unsigned-32 () 像素指针 偏移量
				(sdl-map-rgba (res 'format)
					      r g b a)))]
		 [else
		  (error 's表面 "不支持的过程~n" m)]))))
      res
      )
    )
  )

(define (s表面-表面 表面)
  (表面 '表面))

(define (s表面-init表面 表面)
  (表面 'init表面))

(define (s表面-w 表面)
  (表面 'w))

(define (s表面-h 表面)
  (表面 'h))

(define (s表面-像素 表面 偏移量)
  ((表面 '像素) 偏移量))

(define (s表面-format 表面)
  (表面 'format))

(define (s表面-像素格式 表面)
  (表面 '像素格式))

(define (s表面-pitch 表面)
  (表面 'pitch))

(define (s表面-pixels->ls 表面 pixels长度)
  ((表面 'pixels->ls) pixels长度))

(define (s表面-pixels->bytels 表面 pixel长度)
  ((表面 'pixels->bytels) (* pixel长度 (/ (s表面-pitch 表面) 8))))

(define (s表面-像素set! 表面 偏移量 像素值)
  ((表面 '像素set!) 偏移量 像素值))

(define (s表面-像素map-set! 表面 偏移量 r g b a)
  ((表面 '像素map-set!) 偏移量 r g b a))


(define (坐标x 坐标)
  (cond ((null? 坐标)
	 (assertion-violation '坐标x "坐标为'()" 坐标))
	(else
	 (car 坐标))))

(define (坐标y 坐标)
  (cond ((< (length 坐标) 2)
	 (assertion-violation '坐标y "坐标维度不足" 坐标)
	 )
	((null? 坐标)
	 (assertion-violation '坐标y "贪吃蛇关节为'()" 坐标))
	(else
	 (cadr 坐标))))
;; (define (文本渲染测试 ttf-path ttf-render-fun render string fontsize r g b a)
;;   (字串纹理渲染 (字串纹理-mk ttf-path ttf-render-fun render string fontsize r g b a) 0 0))

;;; 纹理测试
;; (define (纹理测试 render 纹理path)
;;   (let* ((纹理 (s纹理-mk 纹理path render))
;; 	 (渲染器 (s纹理-渲染器构造 纹理 0.0 0.0 纹理宽 纹理高)))
;;     (lambda (m)
;;       (cond ((sdl-event-quit?)
;; 	     (s纹理-quit 纹理))
;; 	    (else
;; 	     (sdl-set-render-draw-color! render 0 0 0 #xFF)
;; 	     (sdl-render-clear render)
;; 	     ;; (sdl-render-copy-ex render (s纹理-纹理-get 纹理) (make-sdl-rect 0 0 20 19) (make-sdl-rect 500 500 20 19) 0.0 NULL SDL_FLIP_NONE) ;ok
;; 	     (渲染器 '(500 300) 20 19 0.0 NULL SDL_FLIP_NONE)
;; 	     (sdl-render-present render))))))

;; (define (事件测试 render)
;;   (let ((R 0))
;;     (lambda (m)
;;       (cond ((sdl-event-quit?)
;; 	     'over
;; 	     )
 ;; 	    ((sdl-event-key-down? SDLK-UP) (set! R (+ R 1))))
;;       (sdl-set-render-draw-color! render R 0 0 #xFF)
;;       (sdl-render-clear render)
;;       (sdl-render-present render))))

;; (define (纹理测试-run)
;;   (let ((game (game-mk (/ 1000 帧率) "纹理测试" SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高)))
;;     (run-game game (纹理测试 (game-render-get game) 贪吃蛇素材path))
;;     (game-quit game)))

;; (define (事件测试-run)
;;   (let ((game (game-mk (/ 1000 帧率) "事件测试" SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高)))
;;     (run-game game (事件测试 (game-render-get game)))
;;     (game-quit game)))


;; (define (音效测试)
;;   (let ((音频 (s音频-mk 音乐pathls 音效pathls 44100 AUDIO_S16LSB 2 2048 8)))
;;     (lambda (m)
;;       (音频-音效播放 音频))))

;; (define (音频测试-run)
;;   (let ((game (game-mk (/ 1000 帧率) "音效测试" SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高)))
;;     (run-game game (音效测试))
;;     (game-quit game)))

;; (音频测试-run)

(define (铰链拉动末端向量 坐标ls1 坐标ls2 铰链长度)
  ;; 这里有点绕,本来应该是计算位移,然后给坐标ls2加上位移,让它移动过去的
  ;; 但是因为移动的模值计算一步会麻烦,所以变成了把点1在计算出的方向上移动模的长度
  ;; 这个位移同点2到点1的位移是反向的
  (let* ((dx (- (坐标x 坐标ls2) (坐标x 坐标ls1)))
	 (方向偏移 (if (> dx 0) 0 π))
	 (1-2位移方向 (atan (容零除
			     (- (坐标y 坐标ls2) (坐标y 坐标ls1))
			     dx))))
    (list (位置更新-极坐标 坐标ls1 铰链长度 (+ 1-2位移方向 方向偏移) 1)
	  (+ 1-2位移方向 方向偏移))))

(define (容零除 a b)
  (if (= b 0)
      (* a b +inf.0)
      (/ a b)))

(define (两点方向 始点坐标ls 终点坐标ls)
  ;; 这里有点绕,本来应该是计算位移,然后给坐标ls2加上位移,让它移动过去的
  ;; 但是因为移动的模值计算一步会麻烦,所以变成了把点1在计算出的方向上移动模的长度
  ;; 这个位移同点2到点1的位移是反向的
  (let* ((dx (- (坐标x 终点坐标ls) (坐标x 始点坐标ls)))
	 (方向偏移 (if (> dx 0) 0 π)))
    (+ 方向偏移 (atan (容零除
		       (- (坐标y 终点坐标ls) (坐标y 始点坐标ls))
		       dx)))
    ))

(define (极坐标->直角坐标 模 幅角)
  (map (lambda (系数)
	 (* 模 系数)) (list (cos 幅角) (sin 幅角))))

(define (object->string format-string x)
  (call-with-string-output-port
   (lambda (p) (format p format-string x))))


(define (charls->u8string charls)
  (multibyte->string 'cp-acp (u8-list->bytevector (map char->integer charls))))
