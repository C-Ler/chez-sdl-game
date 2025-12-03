(define NULL)

;;; SDL底层交互用
;;; 初始化
(define SDL已初始化标志 #b00001)
(define IMG已初始化标志 #b00010)
(define TTF已初始化标志 #b00100)
(define MIX已初始化标志 #b01000)
(define NET已初始化标志 #b10000)

(define-record-type  二进制flag
  (fields (mutable 值)))

(define (mg-init manager flag)
  (sdl-library-init ;; "SDL2.dll"
   )
  (sdl-set-main-ready!)
  (let ((res (sdl-init flag)))
    (when (= -1 res)
      (assertion-violation 'mg-init "SDL初始化失败!" res)))
  ;; (printf "sdl初始化结果:~d~n" ) 	;-1 是有问题
  (二进制flag-值-set! manager SDL已初始化标志))

(define (mg-quit-sdl manager)
  (when (not (= 0 (bitwise-and (二进制flag-值 manager) IMG已初始化标志)))
    (img-quit) ;只有image被加载后才能使用,不然no entry
    )
  (when (not (= 0 (bitwise-and (二进制flag-值 manager) TTF已初始化标志)))
    (ttf-quit) 
    )
  (when (not (= 0 (bitwise-and (二进制flag-值 manager) MIX已初始化标志)))
    (mix-quit) 
    )
  (when (not (= 0 (bitwise-and (二进制flag-值 manager) NET已初始化标志)))
    (sdl-net-quit) 
    )
  (sdl-quit)
  )

(define (sdl依赖跳转初始化 manager sdl-init-flag)
  (case (bitwise-and (二进制flag-值 manager) SDL已初始化标志)
    [(#b00000)  (mg-init manager sdl-init-flag)]) ;这个flag应该是初始化sdl用的)
  ) 

(define (mg-img-init manager flag)	;虽然重复代码挺多,可以搞个构造器构造这些过程,不过还是算了
  (sdl依赖跳转初始化 manager SDL-INIT-EVERYTHING)
  (sdl-image-library-init)
  (let ((res (img-init flag)))
    (when (= -1 res)
      (assertion-violation 'mg-img-init "SDL_img初始化失败!" res))
    ;; (printf "image初始化结果:~d~n" res)
    (二进制flag-值-set! manager (bitwise-or IMG已初始化标志 (二进制flag-值 manager)))))

(define (mg-ttf-init manager)	
  (sdl依赖跳转初始化 manager SDL-INIT-EVERYTHING)
  (sdl-ttf-library-init)
  (let ((res (ttf-init)))
    (when (= -1 res)
      (assertion-violation 'mg-ttf-init "SDL_ttf初始化失败!" res))
    ;; (printf "image初始化结果:~d~n" res)
    (二进制flag-值-set! manager (bitwise-or TTF已初始化标志 (二进制flag-值 manager)))))

(define (mg-mix-init manager flag)	
  (sdl依赖跳转初始化 manager SDL-INIT-EVERYTHING)
  (sdl-mix-library-init)
  (let ((res (mix-init flag)))
    (when (= -1 res)
      (assertion-violation 'mg-mix-init "SDL_mix初始化失败!" res))
    ;; (printf "image初始化结果:~d~n" res)
    (二进制flag-值-set! manager (bitwise-or MIX已初始化标志 (二进制flag-值 manager)))))

(define (mg-net-init manager)	
  (sdl依赖跳转初始化 manager SDL-INIT-EVERYTHING)
  (sdl-net-library-init)
  (let ((res (sdl-net-init)))
    (when (= -1 res)
      (assertion-violation 'mg-net-init "SDL_net初始化失败!" res))
    ;; (printf "image初始化结果:~d~n" res)
    (二进制flag-值-set! manager (bitwise-or NET已初始化标志 (二进制flag-值 manager)))))

;;; 窗口(游戏中可能有多个窗口,直接用game-record绑起来
(define current-renderer (make-parameter '未定义的渲染器))
(define current-surface (make-parameter '未定义表面))

(define (创建窗口 titlestr x y w h)
  (let* ((win (sdl-create-window titlestr x y w h))
	 (winrender (sdl-create-renderer win -1
					 SDL-RENDERER-ACCELERATED	;硬件加速,使用GPU
					 SDL-RENDERER-PRESENT-VSYNC)))
    (current-renderer winrender)
    (current-surface (sdl-get-window-surface win))
    win))

(define-record-type %game		;如果protocol的new过程接受的参数太多,debug会很难......
  (fields (mutable 窗口) (mutable 每帧毫秒) (mutable 退出f)))

(define (创建game sdl-flag img-flag mix-flag 每帧时长 标题str 窗口x 窗口y 窗口宽 窗口高)
  ;; 这里还是用了闭包 2024年7月28日21:42:18
  (let ((manager (make-二进制flag 0)))
    ;; 这一堆init应该单独分开,
    (mg-init manager sdl-flag)
    (mg-img-init manager img-flag)
    (mg-ttf-init manager)
    (mg-mix-init manager mix-flag)
    (mg-net-init manager)
    (sdl-set-hint! "SDL_LOGS" "1")
    (sdl-set-hint! "SDL_VIDEO_DEBUG" "1")  ;; 启用视频子系统详细日志
    (make-%game (创建窗口 标题str 窗口x 窗口y 窗口宽 窗口高)
		每帧时长
		(lambda () (collect)
			(sdl-free-garbage)
			(mg-quit-sdl manager)))
    )
  )

(define get-每帧毫秒 %game-每帧毫秒)
(define get-window %game-窗口)
(define get-quit-foo %game-退出f)
