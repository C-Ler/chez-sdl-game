(load "D://code//aim4//game-uk//engine//l-engine-2.ss")


#|
么得,sdf-udp各个部分互相引用,难以lib化,太恶心.... 2024年1月29日21:49:21
|#
(include "./飞机大战-res.ss")

;;; 依赖sdf
;;;坐标
;; (define-sdf-property gobj:坐标ls
;;   坐标
;;   'predicate complex?
;;   'default-value 0
;;   )

;; (define-type 坐标对象 () (gobj:坐标ls))

;; (define (创建坐标对象 坐标c)
;;   (make-坐标对象 '坐标 坐标c))

;; (定义匹配度优先广义过程 坐标更新! 2 (constant-generic-procedure-handler #f))


(define 世界 (make-parameter (make-eq-hashtable)))

(define (初始化世界)
  (map (lambda (k v)
	 (hashtable-set! (世界) k v))))

(define game (创建game SDL-INIT-EVERYTHING IMG_INIT_EVERYTHING (bitwise-ior MIX_INIT_FLAC MIX_INIT_MP3 MIX_INIT_OGG)
		       (/ 1000 帧率) wintitle SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高))

(define 时间间隔  (get-每帧毫秒 game))	;dt,大部分更新过程都需要引用
(define 计时器0 (make-parameter (make-计时器)))	;游戏循环也需要用到

(define 更新对象 (make-parameter '()))	;保存需要更新的对象的引用,同时这样还需要对过程按传入的参数进行分派.
(define 渲染对象 (make-parameter '()))	;保存需要渲染的对象
(define 背景颜色 (make-sdl-color 0 0 0 0))

(define (更新! dt)
  (map (lambda (obj)
	 ((匹配过程get '更新 obj) dt))
       (更新对象)))

(define (渲染! 估计dt)
  ;; 和上面的过程看起来可以进一步抽象一下
  (sdl-set-render-draw-color! (current-renderer) (sdl-color-r 背景颜色) (sdl-color-g 背景颜色) (sdl-color-b 背景颜色) (sdl-color-a 背景颜色))
  (sdl-render-clear (current-renderer))
  (map (lambda (obj)
	 ((匹配过程get '渲染 obj) 估计dt)
	 )
       (渲染对象))
  (sdl-render-present (current-renderer)))

(define (标题状态)
  ;; 看起来似乎能在这里使用let,但是考虑到控制台和编辑器,还是外部存储算了.... 2025年11月19日10:37:13
  (mix-open-audio 频率 编码flag 声道数量 chunksize)
  (hashtable-set! (世界) '游戏状态 '标题状态)
  
  (hashtable-set! (世界) '星组lol (map (lambda (ls 数量)
					 (新星加入星组 ls 数量 初次创建星星))
				       (make-list 星等数 '())
				       (构造总和固定的随机数集合 星星总数 星等数)  ;单纯用复数表示星星
				       ))

  (hashtable-set! (世界) '星等ls (cdr (iota (+ 1 星等数)))) ;星等ls总是和星组lol一起出现,可以放到一起 

  (匹配过程put
   (lambda (dt) ;可以考虑用vec
     (hashtable-set! (世界) '星组lol
		     (map (lambda (星组)
			    (星组更新 星组 时间间隔))
			  (hashtable-ref (世界) '星组lol 'ref星组lol失败)))
     
     )
   '更新 '星组lol)

  (匹配过程put
   (lambda (估计dt) 
     (渲染星组ls (current-renderer)
		 (hashtable-ref (世界) '星组lol 'ref星组lol失败)
		 (hashtable-ref (世界) '星等ls '星组ls获取失败)))
   '渲染 '星组lol)

  (更新对象 (cons '星组lol (更新对象)))
  (渲染对象 (cons '星组lol (渲染对象)))

  
  (let ((背景纹理 (img-load-texture (current-renderer) 背景path)))
    (hashtable-set! (世界) '背景 (make-游戏对象 0 背景纹理)))

  (let ((背景 (hashtable-ref (世界) '背景 '背景索引失败)))
    (匹配过程put
     (lambda (dt)
       (if (< (+ (real-part (游戏对象-物理组件 背景)) 窗口宽) 0)
	   (游戏对象-物理组件-set! 背景 0)
	   (移动! 背景 背景移动速度 时间间隔)
	   ))
     '更新 '背景)
    
    (更新对象 (cons '背景 (更新对象)))
    
    (匹配过程put
     (lambda (估计dt)
       (背景滚动渲染2 (current-renderer) (游戏对象-图形组件 背景) (游戏对象-物理组件 背景)))
     '渲染 '背景)
    )

  (current-font (创建字体 字体path 24 100 颜色位深))

  (字体扩展字符! (current-font) (string->list (string-append 按下开始str 飞机大战str chezstr 得分榜str 得分榜提示str 录入提示str 录入确认str "0123456789"
							      "当前分:" "最高#\". "))
		 (current-renderer))

  (let* ((按下开始str布局 (创建文本布局信息 (current-font) (current-renderer) 按下开始str 窗口中轴线x 820
					   #xFB #xDA #x41 0
					   '居中 800 +inf.0 0 0 1 1 0.0 NULL SDL_FLIP_NONE))
	 (标题str布局ls
	  (cons 按下开始str布局
		(map (lambda (str y坐标)
		       (创建文本布局信息 (current-font) (current-renderer) str 窗口中轴线x y坐标
					 #xFB #xDA #x41 0
					 '居中 460 +inf.0 0 0  1 1 0.0 NULL SDL_FLIP_NONE)
		       )
		     (list 飞机大战str chezstr)
		     (list 200 300)))))
    
    (匹配过程put
     (lambda (估计dt)
       ;; (sdl-set-render-draw-color! (current-renderer) 96 128 255 255)
       ;; (when (< (mod 状态切换计数器 40) 20))
       ;; (矩形区域渲染字符串 (current-font) (current-renderer) 按下开始str 窗口中轴线x 820  #xFB #xDA #x41 0
       ;; 			   '居中 800 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)
       (map (lambda (str布局)
	      (按布局渲染文本 str布局 (current-renderer)))
	    标题str布局ls)
       ;; (sdl-blit-surface (get-图集表面 (current-font)) (make-sdl-rect 0 0 400 400) (current-surface) (make-sdl-rect 0 0 400 400))
       ;; (sdl-update-window-surface (get-window game))

       ;; (sdl-render-copy-ex (current-renderer) (get-图集纹理 (current-font)) (make-sdl-rect 0 0 200 200)
       ;; 			 (make-sdl-rect 0 0 100 100)
       ;; 			 0.0 NULL SDL_FLIP_NONE)
       
       ;; (sdl-render-copy (current-renderer) (get-图集纹理 (current-font)) (make-sdl-rect 0 0 400 400)
       ;; 		      (make-sdl-rect 400 400 400 400))
       ;; (单字符渲染 #\飞 (current-font) (current-renderer) 500 400 255 255 255 0 1 1 0.0 NULL SDL_FLIP_NONE)
       )
     '渲染 '标题文本))
  

  (渲染对象 (cons '标题文本 (渲染对象))) 
  (渲染对象 (cons '背景 (渲染对象)))	;一定要最后渲染背景,不然会盖住 2025年12月3日11:24:44
  (lambda (k)
    (mix-pause-music)
    ;; (cond ((<= 状态切换计数器 0)
    ;; 	   (set! 状态切换计数器 (* 帧率 5))
    ;; 	   (set! 游戏状态 '(结束状态))) ;计数超过后切换到高分榜界面
    ;; 	  ((sdl-event-key-down? ;; 退格键的处理,chez-sdl的代码这里有问题,值不对,#\b应该是#\backspace 2024年6月20日19:48:51 
    ;; 	    SDLK-RETURN)
    ;; 	   (set! 游戏状态 '(过渡状态))
    ;; 	   (set! 状态切换计数器 (* 帧率 5))
    ;; 	   )
    ;; 	  (else
    ;; 	   (set! 状态切换计数器 (- 状态切换计数器 1)))
    ;; 	  )
    ;; ;; 下面这部分代码有点重复 2024年7月2日19:20:51
        
    (更新! 时间间隔)
    
    ;; (set! 当前渲染器 
    ;; 	  (渲染器构造 render 96 128 255 255 
 
    ;; 			   )))
    ))

(define (初次创建星星)
  (+ (random 窗口宽) (* 0+i (random 窗口高))))

(define (星组更新 星组 dt)		;同时处理了新增和移除
  (let loop ((星组 星组)
	     (acc '()))
    (if (null? 星组)
	acc
	(let ((当前星 (+ (car 星组) (* 星星速度 dt))))
	  (cond ((抵达屏幕左端-nr? 当前星) (loop (cdr 星组)
						 (cons (+ 窗口宽 (* 0+i (random 窗口高))) acc)))
		(else
		 (loop (cdr 星组)
		       (cons 当前星 acc))))))))

(define (渲染星星 render 星星 星等)
  (let* ((星星亮度 (* 星等 32))
	 ;; (星星坐标 (get-坐标 星星))
	 (x坐标 (exact (round (real-part 星星))))  ;吃类型的C语言接口封装溢出了....2024年3月29日21:02:55
	 (y坐标 (exact (round (imag-part 星星))))
	 )
    
    (sdl-set-render-draw-color! render 星星亮度 星星亮度 星星亮度 255)
    (sdl-render-draw-line render x坐标 y坐标 (+ x坐标 3) y坐标)
    )
  )

(define (渲染星组 render 星组 星等)
  (map (lambda (星星)
	 (渲染星星 render 星星 星等)) 星组))

(define (渲染星组ls render 星组lol 星等ls)
  (map (lambda (星组 星等)
	 (渲染星组 render 星组 星等))
       星组lol
       星等ls
       ))

;;;没有使用接口进行抽象层次隔离的方式,减少了每次取出的消耗
(define (触达谓词构造器-nr 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj)
    (判定谓词 (坐标提取过程 (real-part 坐标obj)) 边界值)))

(define (考虑偏移的触达谓词构造器-nr 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj 对象长)
    (判定谓词 (+ (坐标提取过程 (imag-part 坐标obj)) 对象长) 边界值)))

;;; 脑袋碰到
(define 抵达屏幕左端-nr? (触达谓词构造器-nr <= 0 坐标x))
(define 抵达屏幕右端-nr? (考虑偏移的触达谓词构造器-nr >= 窗口宽 坐标x))
(define 抵达屏幕上端-nr? (触达谓词构造器-nr <= 0 坐标y))
(define 抵达屏幕下端-nr? (考虑偏移的触达谓词构造器-nr >= 窗口高 坐标y))
;;; 屁股不见
(define 移出屏幕左端-nr? (考虑偏移的触达谓词构造器-nr <= 0 坐标x))
(define 移出屏幕右端-nr? (触达谓词构造器-nr >= 窗口宽 坐标x))
(define 移出屏幕上端-nr? (考虑偏移的触达谓词构造器-nr <= 0 坐标y))
(define 移出屏幕下端-nr? (触达谓词构造器-nr >= 窗口高 坐标y))

(define (移动! obj v t)
  (set-坐标! obj (+ (get-坐标 obj) (* v t))))

(define (背景滚动渲染2 render 背景s纹理 背景)
  (let* ((x (real-part (get-坐标 背景)))
	 )
    (map (lambda (x)
	   (sdl-render-copy render 背景s纹理
			    NULL
			    (make-sdl-rect x 0 窗口宽 窗口高)))
	 
	 (list x (+ 窗口宽 x))
	 )))

;;; 脚本 script
 (define (run)
   (let* ((时间间隔 (get-每帧毫秒 game))
	  (脉冲间隔 0)
	  (render (current-renderer))
	  (累积时间间隔 0)
	  ;; 
	  (我方飞机s纹理 (s纹理-mk render 我方素材path)
			 ;; (img-load-texture render 我方素材path)
			 )
	  (飞机纹理信息 (s纹理-纹理信息get 我方飞机s纹理))
	  (敌机s纹理 (s纹理-mk render 敌机素材path))
	 
	  (我方飞机w (纹理信息-w 飞机纹理信息))
	  (我方飞机h (纹理信息-h 飞机纹理信息))
	 
	  (我方子弹s纹理 (s纹理-mk render 我方子弹素材path))

	  (我方子弹纹理信息 (s纹理-纹理信息get 我方子弹s纹理))

	  (我方飞机速度 5)

	  (我方子弹h (纹理信息-h 我方子弹纹理信息))

	  (我方子弹w (纹理信息-w 我方子弹纹理信息))

	  (敌机纹理信息 (s纹理-纹理信息get 敌机s纹理))

	  (敌机w (纹理信息-w 敌机纹理信息))
	  (敌机h (纹理信息-h 敌机纹理信息))
	  (敌机数量上限 20)

	  (敌方子弹s纹理 (s纹理-mk render 敌方子弹素材path))
	  (敌方子弹纹理信息 (s纹理-纹理信息get 敌方子弹s纹理))
	  (敌方子弹h (纹理信息-h 敌方子弹纹理信息))
	  (敌方子弹w (纹理信息-w 敌方子弹纹理信息))
	 
	  (我方飞机 (list (make-可移动可破坏obj '坐标 100+100i 'hp 我方飞机初始hp))) ;应该使用create再次封装一下
	  (我方子弹 '())
	  (敌方飞机 '())
	  (敌方子弹 '())
	  (敌机更新计时器 (make-计时器))

	  ;; 目前有一个方向上不用判断碰撞,还是应该搞个类型分派或者消息传递之类的. 2023年9月25日21:59:44
	  (碰撞foo? (lambda (子弹 敌机) (可移动对象碰撞? 子弹 敌机 我方子弹w 我方子弹h 敌机w 敌机h)))
	  (我方-敌弹碰撞foo? (lambda (我方 敌弹) (可移动对象碰撞? 我方 敌弹 我方飞机w 我方飞机h 敌方子弹w 敌方子弹h)))

	  (计时器0 (make-计时器))
	  (重置计时器 (make-计时器))

	  (失败flag #f)


	  (爆炸特效ls '())
	  (爆炸特效s纹理 (s纹理-mk render 爆炸特效素材path))

	  (碎片lol '())
	  (取用框ls (map list->sdl-rect (矩形阵列 (list 敌机w 敌机h) 碎片纹理分割数)))
	 
	  (奖励点ls '())
	  (奖励点s纹理 (s纹理-mk render 奖励点path))
	  (奖励点纹理信息 (s纹理-纹理信息get 奖励点s纹理))
	  (奖励点w (纹理信息-w 奖励点纹理信息))
	  (奖励点h (纹理信息-h 奖励点纹理信息))

	  (我方发弹音效 (mix-load-wav 我方发弹音效path))
	  (敌方发弹音效 (mix-load-wav 敌方发弹音效path))
	  (我方爆炸音效 (mix-load-wav 我方爆炸音效path))
	  (敌方爆炸音效 (mix-load-wav 敌方爆炸音效path))
	  (bgm (mix-load-mus bgmpath))

	  (当前分 0)
	  (最高分 0)

	  ;; (sttf (sttf管理 颜色位深))
	  (whls (list (get-ASCII字符宽度 字体) (get-ASCII字符高度 字体))) ;为了获取光标的宽和高 2024年6月20日20:51:28

	  (得分榜 (make-list 得分榜条目数 (list "匿名" 0)))
	  (当前记录获得者 "匿名")
	  (光标闪烁计数器 0)

	  (游戏状态 '(标题状态))
	  (当前渲染器 (lambda () '()))
	  (状态切换计数器 (* 帧率 5))
	 
	  (按下开始str "按下enter玩游戏")
	  (飞机大战str "飞机大战")
	  (chezstr "Chezscheme")
	  (得分榜str  "得分榜")
	  (得分榜提示str "按下右侧Ctrl键继续")
	  (录入提示str "恭喜获得最高分,请输入你的昵称:")
	  (录入确认str "请敲回车键确认")
	 
	  (奖励点移除谓词-actls (list (cons hp归零? (list (lambda (当前奖励点)
							    (set! 当前分 (+ 当前分 1))
							    (set! 最高分 (max 当前分 最高分))
							    ;; 加分音效 
							    )))))

	  ;; 下面这段的对称感,给人一种抽象不足的感觉 2024年6月1日10:49:17
	  ;; 如果再加入单位实数这个参数 基本一模一样 2024年6月1日10:49:55
	  (奖励点谓词-actls (list (cons 抵达屏幕左端? (list (lambda (当前奖励点)
							      (set-坐标! 当前奖励点 (* 单位虚数 (坐标y (get-坐标 当前奖励点))))
							      (set-速度! 当前奖励点 (y镜像 (get-速度 当前奖励点))))))
				  (cons 抵达屏幕上端? (list (lambda (当前奖励点)
							      (set-坐标! 当前奖励点 (坐标x (get-坐标 当前奖励点)))
							      (set-速度! 当前奖励点 (x镜像 (get-速度 当前奖励点))))))
				  (cons (lambda (obj)
					  (抵达屏幕右端? obj 奖励点w))
					(list (lambda (当前奖励点)
						(set-坐标! 当前奖励点 (+ (- 窗口宽 奖励点w) (* 单位虚数 (坐标y (get-坐标 当前奖励点)))))
						(set-速度! 当前奖励点 (y镜像 (get-速度 当前奖励点))))))
				  (cons (lambda (obj)
					  (抵达屏幕下端? obj 奖励点h))
					(list (lambda (当前奖励点)
						(set-坐标! 当前奖励点 (+ (坐标x (get-坐标 当前奖励点)) (* 单位虚数 (- 窗口高 奖励点h))))
						(set-速度! 当前奖励点 (x镜像 (get-速度 当前奖励点))))))
				  (cons any?
					(list (lambda (当前奖励点)
						(set-hp! 当前奖励点 (- (get-hp 当前奖励点) 1))
						(移动! 当前奖励点 (get-速度 当前奖励点) 时间间隔))))
				  )) ;为了避免出现奖励点碰到右边后加入音效什么的又要修改代码,还是用这个方式好了  2024年6月1日10:20:24

	  (敌机新增谓词-actls '())

	  (敌机谓词-actls (list (cons (lambda (敌方飞机)
					(and (not (null? 我方飞机)) (开火? 敌方飞机)))
				      (list (lambda (敌方飞机)
					      (set-普攻冷却时间! 敌方飞机 (+ 敌机普攻冷却时间基数 (random 敌机普攻冷却时间区间))) ;重置没起作用
					      (set! 敌方子弹 (cons (创建敌方子弹 (左侧中点 敌方飞机 敌机h 敌方子弹w 敌方子弹h)
										 敌弹初始hp
										 (敌方子弹速度构造 (car 我方飞机) 敌方飞机 敌弹速度))
								   敌方子弹))
					      (mix-play-channel-timed -1 敌方发弹音效 0 -1)
					      )))
				(cons any?
				      (list (lambda (敌机)
					      (敌机计时器状态更新! 敌机 时间间隔) ;又开始和时间缠斗了....2024年5月17日17:46:50
					      (移动! 敌机 敌机速度 时间间隔))
					    ))
				)
			  )

	  (敌机移除谓词-actls (list (cons 抵达屏幕左端? '())
				    (cons hp归零? (list (lambda (当前敌机)
							  (set! 爆炸特效ls (append (新增爆炸特效 (get-坐标 当前敌机) (+ 单次爆炸特效数下限 (random 单次爆炸特效数随机区间))) 爆炸特效ls)))
							(lambda (当前敌机)
							  (set! 碎片lol (cons (创建碎片ls 当前敌机 敌机w 敌机h) 碎片lol)))
						       
							(lambda (当前敌机)
							  (mix-play-channel-timed -1 敌方爆炸音效 0 -1)
							  )

							(lambda (当前敌机)
							  (set! 奖励点ls (cons (创建奖励点 当前敌机 敌机w 敌机h 奖励点w 奖励点h (+ (random 奖励点速度最大值)
																   (* 单位虚数
																      (random-in 奖励点速度y最小值 奖励点速度最大值 random))))
									       奖励点ls)))))))

	 
	  (我方飞机移除谓词-actls (list (cons (lambda (我方飞机)
						(not (存活? 我方飞机)))
					      (list (lambda (我方飞机)
						      (启动 重置计时器)
						      (mix-play-channel-timed -1 我方爆炸音效 0 -1)
						      (format #t "~s~%" 当前分)
						     
						      (format #t "~s~%" 得分榜)
						      (set! 游戏状态 '(结束状态)) ;游戏状态转换
						      )))))
	 
	  (我方飞机谓词-actls (list (cons (lambda (我方飞机)
					    (事件->开火状态))
					  (list (lambda (我方飞机)
						  (set! 我方子弹 (cons (make-可移动可破坏obj '坐标 (右侧中点 我方飞机 我方飞机w 我方飞机h 我方子弹h)
											     'hp 我方飞机初始hp)
								       我方子弹))
						  (mix-play-channel-timed -1 我方发弹音效 0 -1))))
				    (cons any?
					  (list (lambda (obj)
						  (set! 我方飞机速度 (速度幅角更新))
						  (移动! obj 我方飞机速度 时间间隔))
						))
				    ))

	  (敌方子弹移除谓词-actls (list (cons 敌弹清除?
					      (list nothing))))

	  (我方子弹移除谓词-actls (list (cons 子弹清除?
					      (list nothing))))

	  (我方子弹谓词-actls (list ;; (cons (lambda (我方子弹)
			       ;; 	   (set! 敌方飞机 (obj更新 敌方飞机 敌机移除谓词-actls 敌机新增谓词-actls 谓词-act并联映射 敌机谓词-actls))
			       ;; 	   ;; 单纯一个set! 是没有返回值的,子弹的赋值会遇到问题...2024年9月8日12:13:22
			       ;; 	   (可移动对象碰撞? 我方子弹 敌方飞机)
			       ;; 	   )
			       ;; 	 (list (lambda (obj)
			       ;; 		 ))
			       ;; 	 )
			       (cons any?
				     (list (lambda (obj)
					     (移动! obj 子弹速度 时间间隔))
					   ))))

	  (敌弹谓词-actls (list (cons any?
				      (list (lambda (obj)
					      (移动! obj (get-速度 obj) 时间间隔))))))

	  (星组移除谓词-actls (list ;; (cons 抵达屏幕左端?
			       ;; 	 (list (lambda (objls)
			       ;; 		 '()))
			       ;; 	 )
			       ))
	 
	  (星组谓词-actls (list (cons any?
				      (list (lambda (objls)
					      (remp 抵达屏幕左端? objls))
					    (lambda (objls)
					      (对象ls-移动! objls 星星速度 时间间隔)
					      objls)) ;需要返回值 2024年5月17日18:42:44)
				      )))

	  (游戏状态谓词-actls
	   (list
	    (cons
	     (lambda (状态) (equal? 状态 '主要状态))
	     (list (lambda (当前状态)
		     ;; (测试输出 时间间隔)
		     (mix-resume-music)
		     (两组碰撞? 我方子弹 敌方飞机 hp-1! hp-1! 碰撞foo? hp归零?)
		     (两组碰撞? 我方飞机 敌方子弹 hp-1! hp-1! 碰撞foo? hp归零?)
		     (两组碰撞? 我方飞机 奖励点ls (lambda (x) x) hp归零! 碰撞foo? hp归零?)
		    
		     (objls移动消散! 爆炸特效ls 爆炸特效消散率 时间间隔)
		     (objlol移动消散! 碎片lol 碎片消散率 时间间隔)
		    
		     (set! 爆炸特效ls (remp 死亡? 爆炸特效ls))
		     (set! 碎片lol (remp objls死亡? 碎片lol))
		    
		     (set! 敌方飞机 (obj更新 敌方飞机 敌机移除谓词-actls 敌机新增谓词-actls 谓词-act并联映射 敌机谓词-actls))

		     (set! 敌方子弹 (obj更新 敌方子弹 敌方子弹移除谓词-actls '() 谓词-act并联映射 敌弹谓词-actls))
		    
		    
		     (set! 星组lol (obj更新 星组lol 星组移除谓词-actls '() 谓词-act串联映射 星组谓词-actls))

		     ;; (format #t "后~s~%" (car 游戏状态)) ;会进入一次结束状态,然后下一个循环又回到主要状态-
		    
		     (set! 我方飞机 (obj更新 我方飞机 我方飞机移除谓词-actls '() 谓词-act并联映射 我方飞机谓词-actls))

		     (set! 我方子弹 (obj更新 我方子弹 我方子弹移除谓词-actls '() 谓词-act并联映射 我方子弹谓词-actls))

		     (set! 奖励点ls (obj更新 奖励点ls 奖励点移除谓词-actls '() 谓词-act并联映射 奖励点谓词-actls))
		    
		     (when (and (< (length 敌方飞机) 敌机数量上限)
				(> (获取时间戳 敌机更新计时器) (+ 敌机生成时间间隔 (random 敌机生成时间间隔))))
		       (set! 敌方飞机 (敌机新增 敌方飞机 敌机更新计时器)))
		    
		     (if (< (+ (坐标x (get-坐标 背景)) 窗口宽) 0)
			 (set-坐标! 背景 0)
			 (移动! 背景 背景移动速度 时间间隔)
			 )
		    
		     ;; (printf "坐标:~d 时间间隔:~d~%" (map (lambda (ls)
		     ;; 					      (map get-坐标 ls)) 星组lol) 时间间隔)

		     (let ((当前星星数 (apply + (map length 星组lol))))
		       (when (< 当前星星数 星星总数)
			 (set! 星组lol (map (lambda (ls 数量)
					      (新星加入星组 ls 数量 循环中创建星星))
					    星组lol (构造总和固定的随机数集合 (- 星星总数 当前星星数)
									      星等数)
					    ))))

		     (set! 当前渲染器 (渲染器构造 render 96 128 255 255
						  (背景滚动渲染2 render 背景s纹理 背景)
						  (渲染星组ls render 星组lol 星等ls)

						  (when (not (null? 我方飞机)) ;一样的风格,只是从发出音效变成了渲染图像 2024年5月17日17:21:00
						    (s纹理-按纹理规格blit render 我方飞机s纹理 (坐标x (get-坐标 (car 我方飞机))) (坐标y (get-坐标 (car 我方飞机)))))
						 
						  (map (lambda (objls s纹理)
							 (同纹理多实体渲染 (map get-坐标 objls) render s纹理))
						       (list 敌方飞机 敌方子弹 我方子弹 ;; 奖励点ls
							     )
						       (list 敌机s纹理 敌方子弹s纹理 我方子弹s纹理 ;; 奖励点s纹理
							     ))

						  (map (lambda (obj)
							 (when (or (> (get-hp obj) 奖励点闪烁临界值)
								   (< (mod (get-hp obj) 12) 6))
							   (let ((坐标 (get-坐标 obj)))
							     (s纹理-按纹理规格blit render 奖励点s纹理 (坐标x 坐标) (坐标y 坐标)))))
						       奖励点ls)
						 
						  (爆炸特效ls渲染 render 爆炸特效ls 爆炸特效s纹理)
						  (碎片lol渲染 render 碎片lol 敌机s纹理 取用框ls)
						  (map (lambda (str x坐标)
							 (矩形区域渲染字符串 字体 render str x坐标 0 255 255 255 0 '左对齐 460 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE))
						       (list (format "当前分:~3,'0d" 当前分) (format "最高分:~3,'0d" 最高分)) ;最高分的字符串宽度莫名其妙在1160... 2024年6月25日22:17:43
						       (list 20 1000)
						       )
						  ))
		    
		    
		     )))
	    (cons
	     (lambda (状态) (and (equal? 状态 '结束状态) (>= 当前分 (得分榜记录分数-get (last 得分榜))) (not (member (list 当前记录获得者 当前分) 得分榜)))) ;当当前分>=最低分时候录入记录 2024年6月20日19:42:22
	     (list (lambda (当前状态)
		     (启动 重置计时器)	;重置重置计时器

		     (mix-pause-music)
		     (cond ((sdl-event-text-input?)
			    (set! 当前记录获得者 (string-append 当前记录获得者 (charls->u8string (sdl-event-text-input-text))))
			    (字体扩展字符! 字体 (string->list 当前记录获得者) render)
			    )
			   ((sdl-event-key-down? ;; 退格键的处理,chez-sdl的代码这里有问题,值不对,#\b应该是#\backspace 2024年6月20日19:48:51 
			     SDLK-BACKSPACE
			     )
			    (let ((串长 (string-length 当前记录获得者)))
			      (set! 当前记录获得者 (if (= 串长 0) "" (string-drop-right 当前记录获得者 1))))
			    )
			   ((sdl-event-key-down? SDLK-RETURN)
			    (set! 得分榜 (得分榜更新 得分榜 (list 当前记录获得者 当前分)))
			    (启动 重置计时器) ;重启重置计时器
			    )
			   (else
			    '()))
		     (set! 光标闪烁计数器 (+ 1 光标闪烁计数器))
		     (set! 当前渲染器 (渲染器构造 render 0 0 0 255
						  (矩形区域渲染字符串 字体 render 录入提示str 窗口中轴线x 0 255 255 255 0 '居中 600 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)
						 
						  (let ((xyls (矩形区域渲染字符串 字体 render 当前记录获得者 窗口中轴线x 100 255 255 255 0 '居中 600 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)))
						    ;; 光标的加入
						    (cond ((< 光标闪烁计数器 (/ 帧率 2))
							   (begin
							     (sdl-set-render-draw-color! render 0 255 0 122) ;; green
							     (sdl-render-fill-rect render (apply make-sdl-rect (append xyls whls)))))
							  ((> 光标闪烁计数器 帧率) (set! 光标闪烁计数器 0))))

						  (矩形区域渲染字符串 字体 render 录入确认str 窗口中轴线x (- 窗口高 200) 255 255 255 0 '居中 600 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)
						 
						 
						  ))
		    
		     )))
	    (cons
	     (lambda (状态) (and (equal? 状态 '结束状态) ;; (null? 我方飞机) (<= (获取时间戳 重置计时器) 重置时间间隔)
				 (or (< 当前分 (得分榜记录分数-get (last 得分榜))) (member (list 当前记录获得者 当前分) 得分榜)))) ;结束,为空,超时后回到主循环
	     (list (lambda (当前状态)	;暂停后重启
		     ;; (format #t "3~s~%" (car 游戏状态)) ;进入不了这里
		     ;; (mix-pause-music)	;暂停音乐
		     (cond ((<= 状态切换计数器 0)
			    (set! 状态切换计数器 (* 帧率 5))
			    (set! 游戏状态 '(标题状态))) ;计数超过后切换到标题界面
			   ((sdl-event-key-down? ;; 退格键的处理,chez-sdl的代码这里有问题,值不对,#\b应该是#\backspace 2024年6月20日19:48:51 
			     SDLK-RETURN)
			    (set! 游戏状态 '(过渡状态))
			    (set! 状态切换计数器 (* 帧率 5))
			    )
			   (else
			    (set! 状态切换计数器 (- 状态切换计数器 1)))
			   )
		    
		     (set! 当前渲染器 (渲染器构造 render 0 0 0 255
						  (矩形区域渲染字符串 字体 render 得分榜str 窗口中轴线x 40  #xFB #xDA #x41 0
								      '居中 400 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)
						  (let loop ((个数 (length 得分榜))
							     (剩余记录 得分榜)
							     (y坐标 得分榜记录坐标y初始值))
						    (cond ((= 个数 0) '())
							  (else
							   (let* ((当前记录 (car 剩余记录))
								  (记录的分数 (cadr 当前记录)))
							     (矩形区域渲染字符串 字体 render (format "#~s .................. ~3,'0d" (car 当前记录) 记录的分数)
										 窗口中轴线x y坐标 255 255 (if (equal? 当前记录 (list 当前记录获得者 当前分))
													       0
													       255) 0 '居中 900 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)
							     )
							   (loop (- 个数 1)
								 (cdr 剩余记录)
								 (- y坐标 得分记录y坐标步进值)))))
						 
						  (矩形区域渲染字符串 字体 render  得分榜提示str 窗口中轴线x 820  #xFB #xDA #x41 0
								      '居中 400 +inf.0 0 0 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE) ;渲染标题 2024年6月3日11:17:34
						 
						 
						  ))
		     )
		   ))
	    (cons
	     (lambda (状态) (and (equal? 状态 '过渡状态) ;; (null? 我方飞机)
				 ;; (or (sdl-event-key-down? SDLK-RCTRL) (> (获取时间戳 重置计时器) 重置时间间隔))
				 ))	;结束,为空,超时后回到主循环
	     (list (lambda (当前状态)	;暂停后重启
		     ;; (format #t "3~s~%" (car 游戏状态)) ;进入不了这里
		     (set! 游戏状态 '(主要状态))
		     (mix-resume-music)	;重启bgm
		     (set! 我方飞机 (list (make-可移动可破坏obj '坐标 0+i 'hp 我方飞机初始hp)))
		     (set! 我方子弹 '())
		     (set! 敌方飞机 '())
		     (set! 敌方子弹 '())
		     (set! 当前分 0)
		     (set! 当前记录获得者 "匿名")
		     )
		   )
	     )
	   
	    )
	   ))
    
     (启动 敌机更新计时器) ;不需要每次循环都重启的计时器  2024年3月21日20:02:55
     (mix-volume-music 33)
     (mix-play-music bgm -1)
     (测试输出 (list 我方飞机h 我方飞机w))

     (字体扩展字符! 字体 (list->set (string->list (string-append 按下开始str 飞机大战str chezstr 得分榜str 得分榜提示str 录入提示str 录入确认str "0123456789"
								 "当前分:" "最高#\". " 当前记录获得者)))
		    render)
     ;; (sttf-字体字符图集初始化 sttf 字体path (list ASCII码区间 中文区间) render 32)
     (游戏循环 game 
	       (lambda (something)
		 ;; 固定时间间隔,追逐实际时间的游戏循环模式,目前这种形式下,如果每次更新时间都太长,会导致永远追不上实际时间 2024年8月9日22:50:04
		 (set! 脉冲间隔 (获取时间戳 计时器0)) ;大的离谱..... 2024年7月29日01:18:26
		 ;; (set! 时间间隔 (获取时间戳 计时器0))
		 (启动 计时器0)	;重置计时器,下次获取时间坐标时就是时间间隔
		 (set! 累积时间间隔 (+ 脉冲间隔 累积时间间隔))
		 (let loop ((lag 累积时间间隔))
		   (if (>= lag 时间间隔)
		       (begin
			 ;; (测试输出 (list '当前累积时间 (floor lag)))
			 (obj更新 游戏状态 '() '() 谓词-act并联映射 游戏状态谓词-actls)
			 (loop (- lag 时间间隔)))
		       (set! 累积时间间隔 lag)))
		
		 ;; (obj更新 游戏状态 '() '() 谓词-act并联映射 游戏状态谓词-actls)
		 ;; (测试输出 (list '累积时间 累积时间间隔))
		 ;; (测试输出 (list '脉冲间隔 脉冲间隔))
		 (当前渲染器)
		 ))
     ))

 ;; (define (core1 game)
 ;;   (lambda (累积时间间隔)
 ;;     ;; (let loop ((lag 累积时间间隔))
 ;;     ;;   (when (>= lag 时间间隔)
 ;;     ;; 	(obj更新 游戏状态 '() '() 谓词-act并联映射 游戏状态谓词-actls)
 ;;     ;; 	(set! 累积时间间隔 (- lag 时间间隔))))
 ;;     (测试输出 (game-render-get game))
 ;;     (测试输出 1)
 ;;     (sdl-render-clear (game-render-get game))
 ;;     (sdl-set-render-draw-color! (game-render-get game)0 255 0 255) ;; green
 ;;     (sdl-render-fill-rect (game-render-get game) (make-sdl-rect 0 0 100 100))
 ;;     (sdl-render-present (game-render-get game))
 ;;     ))

  ;; 在发射子弹的部分遇到问题:无法按住移动的同时射击...



(define (事件->角度)
  ;; 原来这部分重复度太高,其实只需要传入角度,根据事件返回新的角度就OK,速度表示使用极坐标,模值与角度得到dx与dy  2023年3月30日22:31:54
  ;; 其实可以用复数 x i来旋转90度,x -1 旋转 180度
  ;; 1.传入参数进来,传参数出去,函数式
  ;; 2.整包命令式
  ;; 3.按部件分开的函数式
  (cond
   ((and (sdl-event-key-down? SDLK-DOWN)
	 ;; (not (= 原角度 -π/2))
	 ) ;因为必须注意y从0到有值的初始化,所以不能直接用or归并了向上或向下的情况
    0+i
    )
   ((and (sdl-event-key-down? SDLK-UP)
	 ;; (not (= 原角度 π/2))
	 )			      ;之前不是向下移动的,不能直接回头
    0-i
    )
   ((and (sdl-event-key-down? SDLK-RIGHT)
	 ;; (not (= 原角度 π))
	 )			      ;之前不是向下移动的,不能直接回头
    1
    )
   ((and (sdl-event-key-down? SDLK-LEFT)
	 ;; (not (= 原角度 0))
	 )			      ;之前不是向下移动的,不能直接回头
    -1
    )
   (else
    0.0)))

(define (事件->开火状态)
  (cond
   [(sdl-event-key-down? SDLK-LCTRL) #t]
   [(sdl-event-key-up? SDLK-LCTRL) #f]
   [else
    #f]))

(define (速度幅角更新)
  (* 我方飞机移动速度 (事件->角度))
  )

;; (define-sdf-property gobj:hp
;;   hp
;;   'predicate real?
;;   'default 1)

;; (define-type 可移动可破坏obj (坐标对象?) (gobj:hp))

;; (定义匹配度优先广义过程 print 1 display)
;; (广义过程扩展 print ((可移动可破坏obj? obj))
;; 	      (let ((坐标 (get-坐标 obj)))
;; 		(printf "坐标:~d ~d HP:~d ~%" (坐标x 坐标) (坐标y 坐标) (get-hp obj))))

(define (hp归零? 可破坏obj)
(= (get-hp 可破坏obj) 0))

(define (存活? obj)
  (> (get-hp obj) 0))

(define (子弹飞走? 移动obj)		;只能判断是否飞过右侧 2024年3月13日20:02:29
  (> (坐标x (get-坐标 移动obj)) 窗口宽))

(define (死亡? obj)
  (not (存活? obj)))

;; (register-predicate! hp归零? 'hp归零)
;; (register-predicate! 子弹飞走? '子弹飞走)
;; (define 子弹清除? (disjoin hp归零? 子弹飞走?))

;; (define hp减少!
;;   (most-specific-generic-procedure 'hp减少! 2
;; 				   (lambda (hp0 △hp)
;; 				     (- hp0 △hp))))

;; (define-generic-procedure-handler hp减少!
;;   (match-args 可移动可破坏obj? real?)
;;   (lambda (obj n)
;;     (set-hp! obj (- (get-hp obj) n))))

(define (hp-1! obj)
  (hp减少! obj 1))

(define (hp归零! obj)
  (set-hp! obj 0))

(define (右侧中点 坐标obj w h1 h2)
  (+ (get-坐标 坐标obj) (+ w (* 0+i (/ (- h1 h2) 2)))))



;;; 考虑了抽象接口的版本
(define (触达谓词构造器 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj)
    (判定谓词 (坐标提取过程 (get-坐标 坐标obj)) 边界值)))

(define (考虑偏移的触达谓词构造器 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj 对象长)
    (判定谓词 (+ (坐标提取过程 (get-坐标 坐标obj)) 对象长) 边界值)))

;;; 脑袋碰到
(define 抵达屏幕左端? (触达谓词构造器 <= 0 坐标x))
(define 抵达屏幕右端? (考虑偏移的触达谓词构造器 >= 窗口宽 坐标x))
(define 抵达屏幕上端? (触达谓词构造器 <= 0 坐标y))
(define 抵达屏幕下端? (考虑偏移的触达谓词构造器 >= 窗口高 坐标y))
;;; 屁股不见
(define 移出屏幕左端? (考虑偏移的触达谓词构造器 <= 0 坐标x))
(define 移出屏幕右端? (触达谓词构造器 >= 窗口宽 坐标x))
(define 移出屏幕上端? (考虑偏移的触达谓词构造器 <= 0 坐标y))
(define 移出屏幕下端? (触达谓词构造器 >= 窗口高 坐标y))

;; (register-predicate! 抵达屏幕左端? '抵达屏幕左端)
;; (register-predicate! 抵达屏幕右端? '抵达屏幕右端)
;; (register-predicate! 抵达屏幕上端? '抵达屏幕上端)
;; (register-predicate! 抵达屏幕下端? '抵达屏幕下端)

;; (register-predicate! 移出屏幕左端? '移出屏幕左端)
;; (register-predicate! 移出屏幕右端? '移出屏幕右端)
;; (register-predicate! 移出屏幕上端? '移出屏幕上端)
;; (register-predicate! 移出屏幕下端? '移出屏幕下端)

;; (define 敌机清除? (disjoin hp归零? 抵达屏幕左端?))
;; (define 敌弹清除? 敌机清除?)

(define (敌机新增 敌机ls 敌机更新计时器)
  (启动 敌机更新计时器)
  (cons (创建敌机 (+ 窗口宽 (* (random 窗口高) 单位虚数)) 敌机初始hp (+ 敌机普攻冷却时间基数 (random 敌机普攻冷却时间区间))) 敌机ls)) ;初始冷却时间设为1~3秒,避免玩家暴毙 2024年3月19日21:51:05

(define (对象ls-移动! 对象ls 一致的速度 时间间隔)
  (map (lambda (obj)
	 (移动! obj 一致的速度 时间间隔)) 对象ls))

(define (可移动对象碰撞? obj1 obj2 w1 h1 w2 h2)
  (let ((坐标1 (get-坐标 obj1))
	(坐标2 (get-坐标 obj2)))
    (碰撞? (坐标x 坐标1) (坐标y 坐标1) w1 h1
	   (坐标x 坐标2) (坐标y 坐标2) w2 h2)))

(define (一多碰撞? obj1 objls 更新foo! 更新foo2! 碰撞foo? 碰撞判定终止?)
  ;; 判断某个子弹是否命中敌机,如果命中,就结束
  ;; 如果反过来判断敌机是否被攻击到生命值归零依然无法满足要求的话,可以考虑再加一个终止判定谓词的参数. 2024年3月18日21:45:57
  (cond ((null? objls) #f)
	((碰撞判定终止? obj1) #t)
	(else
	 (if (碰撞foo? obj1 (car objls))
	     (begin
	       ;; (print obj1)
	       (更新foo! obj1)
	       (更新foo2! (car objls)))
	     '())
	 (一多碰撞? obj1 (cdr objls) 更新foo! 更新foo2! 碰撞foo? 碰撞判定终止?))))

(define (两组碰撞? ls1 ls2 更新foo1! 更新foo2! 碰撞foo? 碰撞判定终止?)
  (map (lambda (obj)
	 (一多碰撞? obj ls2 更新foo1! 更新foo2! 碰撞foo? 碰撞判定终止?))
       ls1))
;;;
(define (同纹理多实体渲染 坐标ls render s纹理)
  ;; 这个需要增加访问坐标的过程,而且还得对实体类型进行分派....
  (map (lambda (坐标)
	 (s纹理-按纹理规格blit render s纹理 (坐标x 坐标) (坐标y 坐标)))
       坐标ls))
;;;
(define (单位复数 复数)
  (let ((幅值 (magnitude 复数)))
    (+ (/ (real-part 复数) 幅值) (* 单位虚数 (/ (imag-part 复数) 幅值)))))

(define (坐标对象间方向 原点对象 目标对象)
  (单位复数  (apply - (map (lambda (obj)
			     (get-坐标 obj)) (list 目标对象 原点对象))))
  
  )

;; (define-sdf-property gobj:冷却时间计时器
;;   冷却时间
;;   'predicate real?
;;   'default 1)

;; (define-type 可普攻对象 (可移动可破坏obj?) (gobj:冷却时间计时器))

;; (广义过程扩展 print ((可普攻对象? obj))
;; 	      (let ((坐标 (get-坐标 obj)))
;; 		(printf "坐标:~d ~d HP:~d 普攻冷却时间:~d ~%" (坐标x 坐标) (坐标y 坐标) (get-hp obj) (get-普攻冷却时间 obj))))

;; (define get-普攻冷却时间 get-冷却时间)

;; (define set-普攻冷却时间! set-冷却时间!)

(define (创建敌机 坐标c hp 普攻冷却时间)
  (make-可普攻对象 '坐标 坐标c 'hp hp '冷却时间 普攻冷却时间))

(define (开火? obj)
  (<= (get-普攻冷却时间 obj) 0))

(define (敌机计时器状态更新! 敌机 时间间隔)
  (set-普攻冷却时间! 敌机 (- (get-普攻冷却时间 敌机) 时间间隔)))

;; (define-sdf-property gobj:速度c
;;   速度
;;   'predicate complex?
;;   'default 1)

;; (define-type 速度独立对象 (坐标对象?) (gobj:速度c))

;; (define-type 可破坏速度独立对象 (速度独立对象? 可移动可破坏obj?) (gobj:hp))

(define (创建敌方子弹 坐标c hp 速度c)
  (make-可破坏速度独立对象 '坐标 坐标c 'hp hp '速度 速度c))

(define (敌方子弹速度构造 我方飞机 敌方飞机 敌方子弹速度模值)
  ;; 稍稍修改就变成跟踪子弹了
  (* 敌方子弹速度模值 (坐标对象间方向 敌方飞机 我方飞机))
  )

(define (左侧中点 坐标obj h1 w2 h2)
  (+ (get-坐标 坐标obj) (- (* 单位虚数 (/ (- h1 h2) 2)) w2))	;注意不要出现子弹覆盖在敌机脑袋上的情况 2024年3月19日22:30:23
  )

;;;
(define (背景滚动渲染 render 背景s纹理 背景)
  ;; 有点魔幻,效果和期望不太一样 2024年3月25日20:03:13
  (let* ((x (坐标x (get-坐标 背景)))
	 (背景截取的x (+ x 窗口宽)))
    (map (lambda (x w)
	   (sdl-render-copy render (get-纹理 背景s纹理)
			    (make-sdl-rect x 0 w 窗口高)
			    (make-sdl-rect 0 0 窗口宽 窗口高)))

	 (list 0 背景截取的x)
	 (list 背景截取的x (abs x))
	 )))

(define (背景滚动渲染3 render 背景s纹理 背景)
  ;; 有点魔幻,效果和期望不太一样 2024年3月25日20:03:13
  (let* ((x (坐标x (get-坐标 背景)))
	 (右侧部分w (+ x 窗口宽))
	 (背景分割的x (- x)))
    
    (sdl-render-copy render (get-纹理 背景s纹理)
		     (make-sdl-rect 背景分割的x 0 窗口宽 窗口高) ;纹理的右侧
		     (make-sdl-rect 0 0 右侧部分w 窗口高) ;渲染到窗口左侧
		     )

    (sdl-render-copy render (get-纹理 背景s纹理)
		     (make-sdl-rect 0 0 背景分割的x 窗口高) ;纹理的左侧
		     (make-sdl-rect 右侧部分w 0 窗口宽 窗口高) ;渲染到窗口右侧
		     )
    
    ))



;;; 




(define (构造总和固定的随机数集合 总和 个数)
  (letrec ((foo (lambda (总和 个数 acc)
		  (cond ((<= 个数 1) (cons 总和 acc))
			(else
			 (let ((随机数 (random 总和)))
			   (foo (- 总和 随机数) (- 个数 1) (cons 随机数 acc))))))))
    (foo 总和 个数 '())))

(define (新星加入星组 星组ls 数量 创建星星foo)
  (let loop ((次数 数量)
	     (星组 星组ls))
    (cond ((= 次数 0) 星组)
	  (else
	   (loop (- 次数 1)
		 (cons (创建星星foo) 星组))))
    )
  )


;; (define (新星加入星组lol! 星组lol 星星总数 创建星星foo)
;;   (let ((当前星星数 (apply + (map length 星组lol))))
;;     (when (< 当前星星数 星星总数)
;;       (set! 星组lol (map (lambda (ls 数量)
;; 			   (新星加入星组 ls 数量 创建星星foo))
;; 			 星组lol (构造总和固定的随机数集合 (- 星星总数 当前星星数)
;; 							   星等数)
;; 			 )))))



;;; 爆炸特效
;; (define-sdf-property gobj:颜色枚举索引
;;   颜色枚举索引
;;   'predicate integer?
;;   'default-value 0
;;   )

;; (define-type 带颜色可破坏速度独立对象 (可破坏速度独立对象?) (gobj:颜色枚举索引))

(define (创建爆炸特效 参照坐标c)
  (make-带颜色可破坏速度独立对象 '坐标 (+ 参照坐标c
					  (random-in 爆炸特效起始坐标偏移量最小值 爆炸特效起始坐标偏移量最大值 random-complex))
				 'hp (random 爆炸特效最大生命值) '速度 (random-complex 爆炸特效速度最大值)
				 '颜色枚举索引 (random 爆炸特效色彩数)))

(define (新增爆炸特效 参照坐标c 数量)
  (let loop ((n 数量)
	     (acc '()))
    (cond ((= n 0) acc)
	  (else
	   (loop (- n 1)
		 (cons (创建爆炸特效 参照坐标c) acc))))))

;; (定义匹配度优先广义过程 移动消散! 4 (constant-generic-procedure-handler #f))
;; (广义过程扩展 移动消散! ((可破坏速度独立对象? obj) (complex? 速度) (real? △h) (real? t))
;; 	      (移动! obj (get-速度 obj) t)
;; 	      (hp减少! obj (* △h t)))

(define (objls移动消散! objls 消散率 时间间隔)
  (map (lambda (obj)
	 (移动消散! obj (get-速度 obj) 消散率 时间间隔))
       objls))

(define (objlol移动消散! objlol 消散率 时间间隔)
  (map (lambda (objls)
	 (objls移动消散! objls 消散率 时间间隔))
       objlol))

(define (爆炸特效渲染 render 爆炸特效 爆炸特效纹理)
  (let ((color (list-ref 爆炸特效色彩 (get-颜色枚举索引 爆炸特效))))
    (sdl-set-texture-color-mod! (get-纹理 爆炸特效纹理) (sdl-color-r color) (sdl-color-g color) (sdl-color-b color))
    (sdl-set-texture-alpha-mod! (get-纹理 爆炸特效纹理) (exact (round (* 爆炸特效透明度 (get-hp 爆炸特效)))))
    (s纹理-按纹理规格blit render 爆炸特效纹理 (坐标x (get-坐标 爆炸特效)) (坐标y (get-坐标 爆炸特效)))))

(define (爆炸特效ls渲染 render 爆炸特效ls 爆炸特效纹理)
  (begin
    ;; 颜色好淡啊 ...
    (sdl-set-render-draw-blend-mode! render SDL-BLENDMODE-ADD)
    (sdl-set-texture-blend-mode! (get-纹理 爆炸特效纹理) SDL-BLENDMODE-ADD)
    (map (lambda (爆炸特效)
	   (爆炸特效渲染 render 爆炸特效 爆炸特效纹理))
	 爆炸特效ls)))

;;;  飞机碎片
(define 创建碎片 创建敌方子弹)

(define (创建碎片ls 参照物 参照物w 参照物h)
  (let ((参照坐标 (get-坐标 参照物))
	(w中点偏移量 (/ 参照物w 2))
	(h中点偏移量 (* 单位虚数 (/ 参照物h 2))))
    (map (lambda (坐标 速度)
	   (创建碎片 坐标 碎片生命值 速度))
	 (list 参照坐标 (+ 参照坐标 w中点偏移量) (+ 参照坐标 h中点偏移量) (+ 参照坐标 w中点偏移量 h中点偏移量))
	 ((rec 随机数列表构造
	    (lambda (i)
	      (if (= i 0)
		  '()
		  (cons (random-in 碎片速度最小值 碎片速度最大值 random-complex) (随机数列表构造 (- i 1))))))
	  碎片数量))))

(define (碎片ls渲染 render 碎片ls s纹理 取用框ls)
  (map (lambda (碎片 取用框)
	 (框选纹理blit render 碎片 s纹理 取用框))
       碎片ls 取用框ls))

(define (碎片lol渲染 render 碎片lol s纹理 取用框ls)
  (map (lambda (碎片ls)
	 (碎片ls渲染 render 碎片ls s纹理 取用框ls))
       碎片lol))

(define (矩形阵列 规格ls 分割数量ls)
  ;; 10 10 2 2 -> ((0 0 5 5) (5 0 5 5) (0 5 5 5) (5 5 5 5))
  ;; 用递归可以直接抽象到高维情形 2024年4月24日20:15:50
  (letrec ((内 (lambda (数量ls 规格ls)
		 (cond ((null? 数量ls) '(()))
		       (else
			(let ((当前分割数 (car 数量ls))
			      (当前规格 (car 规格ls)))
			  (let loop ((i 0)
				     (坐标 0)
				     (acc '()))
			    (cond ((= i 当前分割数) acc)
				  (else
				   (loop (+ i 1)
					 (+ 坐标 当前规格)
					 (append
					  (map (lambda (其它维度)
						 (cons 坐标 (append 其它维度 (list 当前规格))))
					       (内 (cdr 数量ls) (cdr 规格ls)))
					  acc)))))))))))
    (let ((每部分规格ls (map / 规格ls 分割数量ls)))
      (内 分割数量ls 每部分规格ls)
      )))

(define (list->sdl-rect ls)
  (apply make-sdl-rect ls))

(define (objls死亡? objls)
  (if (null? objls)
      #t
      (死亡? (car objls))))

(define (any? obj)
  #t)

(define (nothing obj) '())

(define (创建奖励点 坐标对象 坐标对象w 坐标对象h 奖励点w 奖励点h 速度)
  (make-可破坏速度独立对象 '坐标 (+ (get-坐标 坐标对象) (+ (- (/ 坐标对象w 2) (/ 奖励点w 2))
							(* 单位虚数 (- (/ 坐标对象h 2) (/ 奖励点h 2)))))
			'hp 奖励点生命初始值
			'速度 速度))


(define 得分榜记录分数-get cadr)
(define 得分榜首条记录-get car)

(define (得分榜更新 得分榜 当前分记录)
  (if (<= (得分榜记录分数-get 当前分记录) (得分榜记录分数-get (得分榜首条记录-get 得分榜)))
      得分榜
      (sort (lambda (记录a 记录b)
	      (apply <= (map 得分榜记录分数-get (list 记录a 记录b)))) (cons 当前分记录 (cdr 得分榜)))))

;; (define (得分榜渲染 得分榜 render)
;;   (sttf字符渲染进矩形区域 sttf render 字体path str  x坐标 0 255 255 255 '左对齐 122 0.0 NULL SDL_FLIP_NONE))
(游戏循环 game (标题状态) 渲染!)
