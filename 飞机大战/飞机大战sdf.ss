(load "D://code//aim4//game-uk//l-engine-2.ss")

#|
么得,sdf-udp各个部分互相引用,难以lib化,太恶心.... 2024年1月29日21:49:21
|#
;;; 配置 config
(define 测试量)
(define 窗口宽 1280)
(define 窗口高 960)
(define 帧率 160)
(define wintitle "飞机大战")
(define NULL)

(define 我方素材path "./res/image/hero1-r.png")
(define 我方飞机移动速度 5)
(define 我方飞机初始hp 3)

(define 我方子弹素材path "./res/image/bullet1.png")
(define 子弹速度 1)
(define 敌弹速度 0.405)

(define 敌机素材path "./res/image/enemy1-l.png")
(define 敌机速度 -0.5)
(define 敌机生成时间间隔 600)

(define 敌方子弹素材path "./res/image/enemy_bullet.png")
(define 敌机普攻冷却时间基数 1200.0)
(define 敌机普攻冷却时间区间 1200.0)
(define 单位虚数 0+i)
(define 敌机初始hp 1)
(define 敌弹初始hp 1)

(define 重置时间间隔 6000)

(define 背景path "./res/image/R-C.jpg")
(define 背景移动速度 -0.3)

(define 星星总数 500)
(define 星等数 3)
(define 星星速度 (+ 背景移动速度 -0.2))

(define 爆炸特效起始坐标偏移量最大值 32+32i)
(define 爆炸特效起始坐标偏移量最小值 (- 爆炸特效起始坐标偏移量最大值))
(define 爆炸特效速度最大值 0.01+0.01i)
(define 爆炸特效最大生命值 3)
(define 爆炸特效透明度 (exact (floor (/ 255 爆炸特效最大生命值))))
(define 爆炸特效色彩 (list (make-sdl-color 255 0 0 255)	;红色
			   (make-sdl-color 255 128 0 255) ;橙色
			   (make-sdl-color 255 255 0 255) ;黄色
			   (make-sdl-color 255 255 255 255)  ;白色
			   ))

(define 爆炸特效色彩数 (length 爆炸特效色彩))
(define 单次爆炸特效数下限 2)
(define 单次爆炸特效数随机区间 2)
(define 爆炸特效消散率 0.001)

(define 爆炸特效素材path "./res/image/enemy1_down3.png")

(define 碎片生命值 2)
(define 碎片速度最大值 0.1+0.1i)
(define 碎片速度最小值 (- 碎片速度最大值))
(define 碎片数量 4)
(define 碎片纹理分割数 '(2 2))
(define 碎片消散率 0.001)

(define 频率 44100)
(define 编码flag AUDIO_S16LSB)
(define 声道数量 2)
(define chunksize 2048)

(define 我方爆炸音效path "./res/sound/player-explosion.wav")
(define 敌方爆炸音效path "./res/sound/enemy-explosion.wav")
(define 我方发弹音效path "./res/sound/player-fire.wav")
(define 敌方发弹音效path "./res/sound/enemy-fire.wav")
(define bgmpath "./res/music/征战蓝天1.MP3")

(define 字体path "C:/Windows/Fonts/simfang.ttf")
(define 颜色位深 32)

(define 奖励点path "./res/image/point.png")
(define 奖励点速度最大值 2)
(define 奖励点速度y最小值 -2)
(define 奖励点生命初始值 (* 帧率 5))
(define 奖励点闪烁临界值 (* 帧率 2))

(define 得分榜条目数 8)
(define 得分榜标题坐标x (- (/ 窗口宽 2) 50))
(define 得分榜记录坐标y初始值 750)
(define 得分榜记录坐标x (- (/ 窗口宽 2) 87))
(define 得分记录y坐标步进值 75)
(define 得分榜提示坐标x初始值 (- (/ 窗口宽 2) 45))
(define 得分榜提示坐标y初始值 800)

;;; 脚本 script
(define (run)
  (let ((game (创建game SDL-INIT-EVERYTHING IMG_INIT_EVERYTHING (bitwise-ior MIX_INIT_FLAC MIX_INIT_MP3 MIX_INIT_OGG)
			(/ 1000 帧率) wintitle SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高)))
    (游戏循环 game (core game))))

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

(define (core game)			;core的返回值是一个渲染器过程
  ;; 在发射子弹的部分遇到问题:无法按住移动的同时射击...
  (mix-open-audio 频率 编码flag 声道数量 chunksize)
  (let* ((时间间隔 (get-每帧毫秒 game))
	 (脉冲间隔 0)
	 (render (game-render-get game))
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
	 
	 (背景s纹理 (s纹理-mk render 背景path))
	 (背景 (创建坐标对象 0))
	 
	 (星组lol (map (lambda (ls 数量)
			 (新星加入星组 ls 数量 初次创建星星))
		       (make-list 星等数 '())
		       (构造总和固定的随机数集合 星星总数 星等数)
		       );; (make-list 星等数 '())
		  )
	 (星等ls (cdr (iota (+ 1 星等数))))

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
	 (sttf (sttf-管理 颜色位深))
	 (whls (ttf-size-text-m (ttf-open-font 字体path 颜色位深) "0" 'utf8)) ;为了获取光标的宽和高 2024年6月20日20:51:28

	 (得分榜 (make-list 得分榜条目数 (list "匿名" 0)))
	 (当前记录获得者 "匿名")
	 (光标闪烁计数器 0)

	 (游戏状态 '(标题状态))
	 (当前渲染器 '())
	 (状态切换计数器 (* 帧率 5))

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
					     (敌机计时器状态更新! 敌机 时间间隔)	;又开始和时间缠斗了....2024年5月17日17:46:50
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

	 (我方子弹谓词-actls (list (cons any?
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
					     objls))	;需要返回值 2024年5月17日18:42:44)
				     )))

	 (游戏状态谓词-actls
	  (list
	   (cons
	    (lambda (状态) (equal? 状态 '标题状态))
	    (list (lambda (当前状态)
		    (mix-pause-music)
		    (cond ((<= 状态切换计数器 0)
			   (set! 状态切换计数器 (* 帧率 5))
			   (set! 游戏状态 '(结束状态))) ;计数超过后切换到高分榜界面
			  ((sdl-event-key-down? ;; 退格键的处理,chez-sdl的代码这里有问题,值不对,#\b应该是#\backspace 2024年6月20日19:48:51 
			    SDLK-RETURN)
			   (set! 游戏状态 '(过渡状态))
			   (set! 状态切换计数器 (* 帧率 5))
			   )
			  (else
			   (set! 状态切换计数器 (- 状态切换计数器 1)))
			)
		    ;; 下面这部分代码有点重复 2024年7月2日19:20:51
		    (set! 星组lol (obj更新 星组lol 星组移除谓词-actls '() 谓词-act串联映射 星组谓词-actls))
		    (if (< (+ (坐标x (get-坐标 背景)) 窗口宽) 0)
			(set-坐标! 背景 0)
			(移动! 背景 背景移动速度 时间间隔)
			)
		    (let ((当前星星数 (apply + (map length 星组lol))))
		      (when (< 当前星星数 星星总数)
			(set! 星组lol (map (lambda (ls 数量)
					     (新星加入星组 ls 数量 循环中创建星星))
					   星组lol (构造总和固定的随机数集合 (- 星星总数 当前星星数)
									     星等数)
					   ))))

		    (set! 当前渲染器
			  (渲染器 (lambda ()
				    (背景滚动渲染2 render 背景s纹理 背景)
				    (when (< (mod 状态切换计数器 40) 20)
				      (sttf字符渲染进矩形区域 sttf render 字体path "按下enter玩游戏" 640 820  #xFB #xDA #x41 '居中 1160 0.0 NULL SDL_FLIP_NONE))
				    (渲染星组ls render 星组lol 星等ls)
				    (map (lambda (str x坐标 y坐标)
					   (sttf字符渲染进矩形区域 sttf render 字体path str x坐标 y坐标 #xFB #xDA #x41 '居中 1160 0.0 NULL SDL_FLIP_NONE))
					 (list (format "飞机大战") (format "Chezscheme"))
					 (list 640 640)
					 (list 200 300)
					 ))
				  render 96 128 255 255))
		
		    )))
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

		          
		    (set! 当前渲染器 (渲染器 (lambda ()
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
						      (sttf字符渲染进矩形区域 sttf render 字体path str x坐标 0 255 255 255 '左对齐 1160 0.0 NULL SDL_FLIP_NONE))
						    (list (format "当前分:~3,'0d" 当前分) (format "最高分:~3,'0d" 最高分)) ;最高分的字符串宽度莫名其妙在1160... 2024年6月25日22:17:43
						    (list 20 1000)
						    )
					       
					       )
					     
					     render 96 128 255 255))
		    
		    )
		  ))
	   (cons
	    (lambda (状态) (and (equal? 状态 '结束状态) (>= 当前分 (得分榜记录分数-get (last 得分榜))) (not (member (list 当前记录获得者 当前分) 得分榜)))) ;当当前分>=最低分时候录入记录 2024年6月20日19:42:22
	    (list (lambda (当前状态)
		    (启动 重置计时器)	;重置重置计时器

		    (mix-pause-music)
		    (cond ((sdl-event-text-input?)
			   (set! 当前记录获得者 (string-append 当前记录获得者 (charls->u8string (sdl-event-text-input-text))))
			   )
			  ((sdl-event-key-down? ;; 退格键的处理,chez-sdl的代码这里有问题,值不对,#\b应该是#\backspace 2024年6月20日19:48:51 
			    SDLK-BACKSPACE
			    )
			   (set! 当前记录获得者 (substring 当前记录获得者 0 (- (string-length 当前记录获得者) 1)))
			   )
			  ((sdl-event-key-down? SDLK-RETURN)
			   (set! 得分榜 (得分榜更新 得分榜 (list 当前记录获得者 当前分)))
			   (启动 重置计时器) ;重启重置计时器
			   )
			  (else
			   '()))
		    (set! 光标闪烁计数器 (+ 1 光标闪烁计数器))
		    (set! 当前渲染器 (渲染器
				      (lambda ()
					
					(sttf字符渲染进矩形区域 sttf render 字体path "恭喜获得最高分,请输入你的昵称:" 得分榜标题坐标x 0 255 255 255 '居中 900 0.0 NULL SDL_FLIP_NONE)
					
					(let ((xyls (sttf字符渲染进矩形区域 sttf render 字体path 当前记录获得者 得分榜标题坐标x 100 255 255 255 '居中 900 0.0 NULL SDL_FLIP_NONE)))
					  ;; 光标的加入
					  (cond ((< 光标闪烁计数器 (/ 帧率 2))
						 (begin
						   (sdl-set-render-draw-color! render 0 255 0 122) ;; green
						   (sdl-render-fill-rect render (apply make-sdl-rect (append xyls whls)))))
						((> 光标闪烁计数器 帧率) (set! 光标闪烁计数器 0))))

					(sttf字符渲染进矩形区域 sttf render 字体path "请敲回车键确认" 得分榜标题坐标x (- 窗口高 200) 255 255 255 '居中 900 0.0 NULL SDL_FLIP_NONE)
					
					)
				      render 0 0 0 255))
		    
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
		    (set! 当前渲染器 (渲染器
				      (lambda ()
					(sttf字符渲染进矩形区域 sttf render 字体path "得分榜" 得分榜标题坐标x 0 255 255 255 '居中 900 0.0 NULL SDL_FLIP_NONE) ;渲染标题 2024年6月3日11:17:34
					(let loop ((个数 (length 得分榜))
						   (剩余记录 得分榜)
						   (y坐标 得分榜记录坐标y初始值))
					  (cond ((= 个数 0) '())
						(else
						 (let* ((当前记录 (car 剩余记录))
							(记录的分数 (cadr 当前记录)))
						   (sttf字符渲染进矩形区域 sttf render 字体path (format "#~s .................. ~3,'0d" (car 当前记录) 记录的分数)
									   得分榜记录坐标x y坐标 255 255 (if (equal? 当前记录 (list 当前记录获得者 当前分))
													     0
													     255) '居中 1000 0.0 NULL SDL_FLIP_NONE)
						   )
						 (loop (- 个数 1)
						       (cdr 剩余记录)
						       (- y坐标 得分记录y坐标步进值)))))
					
					(sttf字符渲染进矩形区域 sttf render 字体path "按下右侧Ctrl键继续" 得分榜提示坐标x初始值 得分榜提示坐标y初始值
								255 255 255 '居中 900 0.0 NULL SDL_FLIP_NONE) ;渲染标题 2024年6月3日11:17:34
					
					)
				      render 0 0 0 255))
		    )
		  ))
	   (cons
	    (lambda (状态) (and (equal? 状态 '过渡状态) ;; (null? 我方飞机)
				;; (or (sdl-event-key-down? SDLK-RCTRL) (> (获取时间戳 重置计时器) 重置时间间隔))
				)) ;结束,为空,超时后回到主循环
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
	  )
	 )
    
    (启动 敌机更新计时器)		;不需要每次循环都重启的计时器  2024年3月21日20:02:55
    (mix-volume-music 33)
    (mix-play-music bgm -1)
    
    (sttf-字体字符图集初始化 sttf 字体path (list ASCII码区间 中文区间) render 32)
        
    (lambda (something)
      
      (set! 脉冲间隔 (获取时间戳 计时器0)) ;大的离谱..... 2024年7月29日01:18:26
      (启动 计时器0)	;重置计时器,下次获取时间坐标时就是时间间隔
      (set! 累积时间间隔 (+ 脉冲间隔 累积时间间隔))
      (let loop ((lag 累积时间间隔))
	(when (>= lag 时间间隔)
	  (测试输出 (list '当前累积时间 (floor lag)))
	  (obj更新 游戏状态 '() '() 谓词-act并联映射 游戏状态谓词-actls)
	  (loop (- lag 时间间隔)))
	(set! 累积时间间隔 lag))
      (测试输出 (list '累积时间 累积时间间隔))
      (测试输出 (list '脉冲间隔 脉冲间隔))
      当前渲染器			;这个过程需要返回一个渲染器
      )))

(define 坐标x real-part)

(define 坐标y imag-part)

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
	 ) ;之前不是向下移动的,不能直接回头
    0-i
    )
   ((and (sdl-event-key-down? SDLK-RIGHT)
	 ;; (not (= 原角度 π))
	 ) ;之前不是向下移动的,不能直接回头
    1
    )
   ((and (sdl-event-key-down? SDLK-LEFT)
	 ;; (not (= 原角度 0))
	 ) ;之前不是向下移动的,不能直接回头
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

(define gobj:hp
  (make-property 'hp
		 'predicate real?
		 'default 1))

(define-type 可移动可破坏obj (坐标对象?) (gobj:hp))

(定义匹配度优先广义过程 print 1 display)
(广义过程扩展 print ((可移动可破坏obj? obj))
	      (let ((坐标 (get-坐标 obj)))
		(printf "坐标:~d ~d HP:~d ~%" (坐标x 坐标) (坐标y 坐标) (get-hp obj))))

(define 移动!
  (most-specific-generic-procedure '移动! 3
				   (constant-generic-procedure-handler #f)))

(广义过程扩展 移动! ((坐标对象? obj) (complex? v) (real? t))
	      (set-坐标! obj (+ (get-坐标 obj) (* v t))))

(define (hp归零? 可破坏obj)
  (= (get-hp 可破坏obj) 0))

(define (存活? obj)
  (> (get-hp obj) 0))

(define (子弹飞走? 移动obj)		;只能判断是否飞过右侧 2024年3月13日20:02:29
  (> (坐标x (get-坐标 移动obj)) 窗口宽))

(define (死亡? obj)
  (not (存活? obj)))

(register-predicate! hp归零? 'hp归零)
(register-predicate! 子弹飞走? '子弹飞走)
(define 子弹清除? (disjoin hp归零? 子弹飞走?))

(define hp减少!
  (most-specific-generic-procedure 'hp减少! 2
				   (lambda (hp0 △hp)
				     (- hp0 △hp))))

(define-generic-procedure-handler hp减少!
  (match-args 可移动可破坏obj? real?)
  (lambda (obj n)
    (set-hp! obj (- (get-hp obj) n))))

(define (hp-1! obj)
  (hp减少! obj 1))

(define (hp归零! obj)
  (set-hp! obj 0))

(define (右侧中点 坐标obj w h1 h2)
  (+ (get-坐标 坐标obj) (+ w (* 0+i (/ (- h1 h2) 2)))))

;; (define (普通攻击构造 冷却时间 计时器))
(define (考虑偏移的边界触达谓词构造器 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj 对象长)
    (判定谓词 (+ (坐标提取过程 (get-坐标 坐标obj)) 对象长) 边界值)))

(define (触达谓词构造器 判定谓词 边界值 坐标提取过程)
  (lambda (坐标obj)
    (判定谓词 (坐标提取过程 (get-坐标 坐标obj)) 边界值)))

;;; 脑袋碰到
(define 抵达屏幕左端? (触达谓词构造器 <= 0 坐标x))
(define 抵达屏幕右端? (考虑偏移的边界触达谓词构造器 >= 窗口宽 坐标x))
(define 抵达屏幕上端? (触达谓词构造器 <= 0 坐标y))
(define 抵达屏幕下端? (考虑偏移的边界触达谓词构造器 >= 窗口高 坐标y))
;;; 屁股不见
(define 移出屏幕左端? (考虑偏移的边界触达谓词构造器 <= 0 坐标x))
(define 移出屏幕右端? (触达谓词构造器 >= 窗口宽 坐标x))
(define 移出屏幕上端? (考虑偏移的边界触达谓词构造器 <= 0 坐标y))
(define 移出屏幕下端? (触达谓词构造器 >= 窗口高 坐标y))

(register-predicate! 抵达屏幕左端? '抵达屏幕左端)
(register-predicate! 抵达屏幕右端? '抵达屏幕右端)
(register-predicate! 抵达屏幕上端? '抵达屏幕上端)
(register-predicate! 抵达屏幕下端? '抵达屏幕下端)

(register-predicate! 移出屏幕左端? '移出屏幕左端)
(register-predicate! 移出屏幕右端? '移出屏幕右端)
(register-predicate! 移出屏幕上端? '移出屏幕上端)
(register-predicate! 移出屏幕下端? '移出屏幕下端)

(define 敌机清除? (disjoin hp归零? 抵达屏幕左端?))
(define 敌弹清除? 敌机清除?)

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

(define gobj:冷却时间计时器
  (make-property '冷却时间
		 'predicate real?
		 'default 1))

(define-type 可普攻对象 (可移动可破坏obj?) (gobj:冷却时间计时器))

(广义过程扩展 print ((可普攻对象? obj))
	      (let ((坐标 (get-坐标 obj)))
		(printf "坐标:~d ~d HP:~d 普攻冷却时间:~d ~%" (坐标x 坐标) (坐标y 坐标) (get-hp obj) (get-普攻冷却时间 obj))))

(define get-普攻冷却时间 
  (property-getter  gobj:冷却时间计时器 可普攻对象?))

(define set-普攻冷却时间!
  (property-setter gobj:冷却时间计时器 可普攻对象? real?))

(define (创建敌机 坐标c hp 普攻冷却时间)
  (make-可普攻对象 '坐标 坐标c 'hp hp '冷却时间 普攻冷却时间))

(define (开火? obj)
  (<= (get-普攻冷却时间 obj) 0))

(define (敌机计时器状态更新! 敌机 时间间隔)
  (set-普攻冷却时间! 敌机 (- (get-普攻冷却时间 敌机) 时间间隔)))

(define gobj:速度c
  (make-property '速度
		 'predicate complex?
		 'default 1))

(define-type 速度独立对象 (坐标对象?) (gobj:速度c))

(define-type 可破坏速度独立对象 (速度独立对象? 可移动可破坏obj?) (gobj:hp))

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

(define (背景滚动渲染2 render 背景s纹理 背景)
  (let* ((x (坐标x (get-坐标 背景)))
	 )
    (map (lambda (x)
	   (sdl-render-copy render (get-纹理 背景s纹理)
			    NULL
			    (make-sdl-rect x 0 窗口宽 窗口高)))

	 (list x (+ 窗口宽 x))
	 )))

;;; 
(define (初次创建星星)
  ;; 所有新构造的星星全都出现在屏幕最右侧 2024年3月31日11:33:23
  (创建坐标对象 (+ (random 窗口宽) (* 单位虚数 (random 窗口高)))))

(define (循环中创建星星)
  ;; 所有新构造的星星全都出现在屏幕最右侧 2024年3月31日11:33:23
  (创建坐标对象 (+ 窗口宽 (* 单位虚数 (random 窗口高)))))

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


(define (新星加入星组lol! 星组lol 星星总数 创建星星foo)
  (let ((当前星星数 (apply + (map length 星组lol))))
    (when (< 当前星星数 星星总数)
      (set! 星组lol (map (lambda (ls 数量)
			   (新星加入星组 ls 数量 创建星星foo))
			 星组lol (构造总和固定的随机数集合 (- 星星总数 当前星星数)
							   星等数)
			 )))))

(define (渲染星星 render 星星 星等)
  (let* ((星星亮度 (* 星等 32))
	 ;; (星星坐标 (get-坐标 星星))
	 (x坐标 (exact (round (get-x坐标 星星))))  ;吃类型的C语言接口封装溢出了....2024年3月29日21:02:55
	 (y坐标 (exact (round (get-y坐标 星星))))
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

;;; 爆炸特效
(define (random-in negative positive randomfoo)
  (- (randomfoo positive) (randomfoo(- negative))))

(define (random-complex c)	      	;实部和虚部都必须是正数...2024年4月16日12:40:13
  (+ (random (real-part c)) (* 单位虚数 (random (imag-part c)))))

(define gobj:颜色枚举索引
  (make-property '颜色枚举索引
                 'predicate integer?
                 'default-value 0)
  )

(define-type 带颜色可破坏速度独立对象 (可破坏速度独立对象?) (gobj:颜色枚举索引))

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
;;;
(define (obj更新 objls 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls)		
  (cond ((null? objls) '())
	;; hp归零的状态变化和飞机飞出屏幕的变化不同,关键在于副作用 2024年5月13日21:43:32
	((新增/移除谓词-act映射 (car objls) 移除谓词-actls #t) (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls))
	;; ((新增/移除谓词-act映射 (car objls) 新增谓词-actls #t) (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls))
	(else
	 (cons (谓词-act映射foo (car objls) 谓词-actls) ;先并后串或者相反的顺序问题 2024年5月13日10:11:28
	       (obj更新 (cdr objls) 移除谓词-actls 新增谓词-actls 谓词-act映射foo 谓词-actls)))))

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

(定义匹配度优先广义过程 移动消散! 4 (constant-generic-procedure-handler #f))
(广义过程扩展 移动消散! ((可破坏速度独立对象? obj) (complex? 速度) (real? △h) (real? t))
	      (begin (移动! obj (get-速度 obj) t)
		     (hp减少! obj (* △h t))))

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

(define (x镜像 c)
  (- (real-part c) (* 单位虚数 (imag-part c))))

(define (y镜像 c)
  (- (* 单位虚数 (imag-part c)) (real-part c)))

(define 得分榜记录分数-get cadr)
(define 得分榜首条记录-get car)

(define (得分榜更新 得分榜 当前分记录)
  (if (<= (得分榜记录分数-get 当前分记录) (得分榜记录分数-get (得分榜首条记录-get 得分榜)))
      得分榜
      (sort (lambda (记录a 记录b)
	      (apply <= (map 得分榜记录分数-get (list 记录a 记录b)))) (cons 当前分记录 (cdr 得分榜)))))

;; (define (得分榜渲染 得分榜 render)
;;   (sttf字符渲染进矩形区域 sttf render 字体path str  x坐标 0 255 255 255 '左对齐 122 0.0 NULL SDL_FLIP_NONE))
