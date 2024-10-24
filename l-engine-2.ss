(library-directories 
 '("D:\\lib" "D:\\lib\\thunderchez-trunk"
   "D:\\lib" "D:\\lib\\scheme-lib\\packages"
   ))

(import
 (chez-sdl lib sdl)
 (srfi s1 lists)
 ;; (srif s3 list-set)
 (srfi s13 strings)
 (srfi s14 char-sets)			;似乎不支持u8
 (srfi s113 sets)			;需要s128,但是依然有问题 set过程配合comparator结果不行 2023年9月3日15:39:07
 (srfi s128 comparators)
 (srfi s27 random-bits)
 (base-ex simple-ex)			;提供了测试输出
 (pregexp pregexp)

 (srfi s158 generators-and-accumulators) 
 					;Generators and Accumulators ,141得惰性也能实现,这个的本质是轻量级按需求值
 )

(include "D://code//aim4//game-uk//sdf-adven-core.scm")
(include "D://code//aim4//game-uk//数学math.ss")
;; (include "D://code//aim4//game-uk//entity.ss")

;;; 下面这部分代码无法用include 分离出去,会提示没定义...
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


(define NULL)

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

;; (定义匹配度优先广义过程 init 2 (constant-generic-procedure-handler #f))	;可能需要链式调用 2024年3月26日22:55:44
;; (定义匹配度优先广义过程 get-启动标记 1 (constant-generic-procedure-handler #f))

(define-sdf-property gobj:启动标记			;计时器也用到了  2024年8月10日16:06:48
  启动标记
  'predicate boolean?
  'default-value #f
  )

;; (define base控制台?
;;   (make-type 'base控制台 (list gobj:启动标记)))

;; (define 构造base控制台
;;   (type-instantiator base控制台?))

;; ;; (广义过程扩展 get-启动标记 base控制台? getter gobj:启动标记
;; ;; 	      ;; (property-getter 'base控制台? gobj:启动标记)
;; ;; 	      )

;; (define get-启动标记 
;;   (property-getter gobj:启动标记 base控制台?)
;;   )

;; (set-predicate<=! base控制台? object?)

;; (广义过程扩展 init ((base控制台? obj) (base控制台? 依赖控制台))
;; 	      (when (not (get-启动标记 依赖控制台))
;; 		(begin
;; 		  (sdl-library-init "SDL2.dll")
;; 		  (sdl-set-main-ready!)
;; 		  (printf "sdl初始化结果:~d~n" (sdl-init SDL-INIT-EVERYTHING))))) 	;-1 是有问题)

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
(define-sdf-property sdl:window
  window
  'predicate (lambda (p) (ftype-pointer? SDL_Window p)))

(define-sdf-property sdl:surface		;property
  surface
  'predicate (lambda (p) (ftype-pointer? SDL_Surface p)))

(define-sdf-property sdl:renderer
  renderer
  'predicate (lambda (p) (ftype-pointer? SDL_Renderer p)))


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
(define-sdf-property gobj:窗口对象
  窗口对象
  'predicate 窗口对象?)

(define-sdf-property gobj:每帧毫秒
  每帧毫秒
  'predicate real?)

(define-sdf-property gobj:quit-foo
  quit-foo
  'predicate procedure?)

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
  (sdl-get-window-surface (game-window-get game)))

;;; 计时器类型
;; 生成敌机的这种可以换成协程 2024年7月29日00:45:22
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
;;; 一部分不依赖sdl的,可以分离成物理,输入,渲染等部分的内容
(define (sdl-map foo ls)
  (cond ((null? ls) #t)
	(else
	 (foo (car ls))
	 (sdl-map foo (cdr ls)))))

(define (渲染器 thunk render r g b a)
  ;; prepareScene
  (sdl-set-render-draw-color! render r g b a)
  (sdl-render-clear render)
  (thunk)
  ;; presentScene
  (sdl-render-present render)
  )

(define-syntax 渲染器构造
  (syntax-rules ()
    [(_ render r g b a e ...)
     (lambda ()
       (sdl-set-render-draw-color! render r g b a)
       (sdl-render-clear render)
       e ...
       (sdl-render-present render))]))

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

(define-sdf-property gobj:纹理
  纹理
  'predicate any-object?		;因为目前没有实现判断C的指针是某个类型的方法,这里需要一个对任何对象都返回#t的谓词
  'default (lambda () '无效的情况))

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

;;; 属性没办法放进被include的文件 以下是sdf-ttf部分 
(define-sdf-property gobj:font
  font
  'predicate any-object?)

(define-sdf-property gobj:图集表面
  图集表面
  'predicate any-object?)

(define-sdf-property gobj:图集纹理
  图集纹理
  'predicate any-object?
  'default-value '没有纹理)

(define-sdf-property gobj:图集规格
  图集规格
  'predicate integer?
  'default-value 480)

(define-sdf-property gobj:扩展次数
  扩展次数
  'predicate integer?
  'default-value 1)

(define-sdf-property gobj:字符-取用框ht
  字符-取用框ht
  'predicate hashtable?
  'default-value (make-hashtable equal-hash equal?))

(define-sdf-property gobj:空白格坐标
  第一个空白格坐标ls
  'predicate list?
  'default-value '(0 0))

(define-sdf-property gobj:ASCII字符宽度
  ASCII字符宽度
  'predicate integer?
  'default-value 0)

(define-sdf-property gobj:ASCII字符高度
  ASCII字符高度
  'predicate integer?
  'default-value 0)

(define-type 游戏字体 () (gobj:font gobj:图集表面 gobj:图集纹理 gobj:图集规格 gobj:扩展次数 gobj:字符-取用框ht gobj:空白格坐标 gobj:ASCII字符宽度 gobj:ASCII字符高度))

(define-sdf-property gobj:字符串
  字符串
  'predicate string?
  'default-value "")

(define-sdf-property gobj:字体
  game-字体
  'predicate 游戏字体?)

(define-type 游戏字符串 () (gobj:字体 gobj:字符串))

(include "D://code//aim4//game-uk//文本渲染-sdf-ttf.ss")

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
(define-sdf-property gobj:music
  音乐
  'predicate any-object?		;
  'default '无效的音乐)

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

(define-sdf-property gobj:sound
  音效
  'predicate any-object?
  'default '无效的音效)

(define-type s音效 () (gobj:sound))

(define (创建音效 path)
  (make-s音效 '音效 (mix-load-wav path))) ;遇到多个音效的情况,这样还不够 2024年5月9日19:31:53


(define-sdf-property gobj:音效ht
  音效__
  'predicate hashtable?
  'default (make-eqv-hashtable))

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
;;; 表面
(define-sdf-property gobj:表面
  表面
  'predicate any-object?)

(define-sdf-property gobj:像素指针
  像素指针
  'predicate (lambda (x) (ftype-pointer? x)))

(define-type 游戏表面 () (gobj:表面 gobj:像素指针))

(define (创建游戏表面 surface)
  (let* ((表面 (sdl-convert-surface-format surface SDL_PIXELFORMAT_RGBA32 0)) ;由于传入的表面 pixelformat是24位的,修改像素有些麻烦,所以最后还是用了去年的策略,先转换格式  2023年5月7日20:34:24
	 )
    (make-游戏表面 '表面 表面 '像素指针 (make-ftype-pointer unsigned-32 (ftype-ref SDL_Surface (pixels) 表面)))))

(定义匹配度优先广义过程 get-w 1 0)
(定义匹配度优先广义过程 get-h 1 0)

(广义过程扩展 get-w ((游戏表面? obj))
	      (ftype-ref SDL_Surface (w) (get-表面 obj)))

(广义过程扩展 get-h ((游戏表面? obj))
	      (ftype-ref SDL_Surface (h) (get-表面 obj)))

(定义匹配度优先广义过程 get-像素 2 0)

(广义过程扩展 get-像素 ((游戏表面? obj) (integer? 偏移量))
	      (ftype-ref unsigned-32 () (get-像素指针 obj) 偏移量))

(定义匹配度优先广义过程 get-format 1 0)

(广义过程扩展 get-format ((游戏表面? obj))
	      (ftype-ref SDL_Surface (format) (get-表面 obj)))

(定义匹配度优先广义过程 set-像素! 4 0)

(广义过程扩展 set-像素! ((游戏表面? obj) (integer? 偏移量) (integer? 像素值))
	      (ftype-set! unsigned-32 () (get-像素指针 obj) 偏移量 像素值))

(定义匹配度优先广义过程 set-map-像素! 6 0)
(广义过程扩展 set-map-像素! ((游戏表面? obj) (integer? 偏移量) (integer? r) (integer? g) (integer? b) (integer? a))
	      (ftype-set! unsigned-32 () (get-像素指针 obj) 偏移量 (sdl-map-rgba (get-format obj)
										 r g b a)))

;;; atlas
(include "D://code//aim4//game-uk//图集atlas.ss")

;;; physics
(include "D://code//aim4//game-uk//物理physics.ss")

;;;更新方法
(define (obj更新 objls 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls)		
  (cond ((null? objls) '())
	;; hp归零的状态变化和飞机飞出屏幕的变化不同,关键在于副作用 2024年5月13日21:43:32
	((新增/移除谓词-act映射 (car objls) 移除谓词-actls #t) (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls))
	;; ((新增/移除谓词-act映射 (car objls) 新增谓词-actls #t) (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls))
	(else
	 (cons (谓词-act映射foo (car objls) 谓词-actls) ;先并后串或者相反的顺序问题 2024年5月13日10:11:28
	       (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls)))))

(define (二元objls更新 objls 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls objls2)
  ;; 考虑到碰撞检测造成的更新,需要扩张维度,传入构造函数会导致游戏循环增加开销 2024年9月9日21:30:33
  (cond ((null? objls) '())
	((新增/移除谓词-act映射 (car objls) 移除谓词-actls #t) ;这里显然是需要增加一个参数,而且修改这个结构了.... 2024年9月9日21:31:26
	 (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls objls2))
	;; ((新增/移除谓词-act映射 (car objls) 新增谓词-actls #t) (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls))
	(else
	 (cons (谓词-act映射foo (car objls) 谓词-actls) ;先并后串或者相反的顺序问题 2024年5月13日10:11:28
	       (二元objls更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls)))))

(define (新增/移除谓词-act映射 obj 谓词-actls 存在t全部f)
  (cond ((null? 谓词-actls) #f)
	(else
	 (or (and ((caar 谓词-actls) obj)	;只有在#t的情况下才产生副作用 2024年5月13日21:48:47
		  (begin (map (lambda (foo)
				(foo obj)) (cdar 谓词-actls))
			 存在t全部f))			;返回#f,让or继续递归 2024年5月13日21:49:04 锤子,返回#t,直接跳出递归,只要成功一次移除判定就好 2024年5月14日15:45:42
	     (新增/移除谓词-act映射 obj (cdr 谓词-actls) 存在t全部f)))))
;;;
(define (谓词-act串联映射 obj 谓词-actls)
  (cond ((null? 谓词-actls) obj)
	(else
	 (谓词-act串联映射  (if ((caar 谓词-actls) obj)
				(fold-left (lambda (obj foo)
					     (foo obj)) obj (cdar 谓词-actls))
				obj)
			    (cdr 谓词-actls)))))

(define (谓词-act并联映射 obj 谓词-actls)
  (cond ((null? 谓词-actls) obj)
	(else
	 (when ((caar 谓词-actls) obj)
	   (map (lambda (foo)
		  (foo obj)) (cdar 谓词-actls)))
	 (谓词-act并联映射 obj (cdr 谓词-actls)))))
