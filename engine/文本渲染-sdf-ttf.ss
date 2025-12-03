;;;
(define-record-type (%游戏字体 make-游戏字体 游戏字体?)
  (fields  (mutable font get-font set-font!)                ; 字体对象
	   (mutable 图集表面 get-图集表面 set-图集表面!)      ; 图集表面
	   (mutable 图集纹理 get-图集纹理 set-图集纹理!)      ; 图集纹理  
	   (mutable 图集规格 get-图集规格 set-图集规格!)      ; 图集规格
	   (mutable 扩展次数 get-扩展次数 set-扩展次数!)      ; 扩展次数
	   (mutable 字符-取用框ht get-字符-取用框ht set-字符-取用框ht!) ; 字符哈希表
	   (mutable 空白格坐标 get-第一个空白格坐标ls set-第一个空白格坐标ls!) ; 空白坐标
	   (mutable ASCII字符宽度 get-ASCII字符宽度 set-ASCII字符宽度!) ; ASCII宽度
	   (mutable ASCII字符高度 get-ASCII字符高度 set-ASCII字符高度!) ; ASCII高度
	   (mutable 纹理更新tag get-纹理脏? set-纹理脏?!)
	   )
  (nongenerative)
  (protocol
   (lambda (new)
     (lambda (font 图集表面 图集规格)
       (let ((whls (ttf-size-text-m font "0" 'utf8)))
	 (字体图集表面背景透明化! 图集表面)
	 (new font 图集表面 '没有纹理 图集规格 1 (make-hashtable equal-hash equal?) '(0 0) (car whls) (cadr whls) #f))
       )))
  )

(define-record-type (%游戏字符串 make-游戏字符串 游戏字符串?)
  (fields  (mutable 游戏字体 get-game-字体 set-game-字体!)  ; 游戏字体
	   (mutable 字符串 get-字符串 set-字符串!)          ; 字符串内容
	   )
  (nongenerative))

(define (创建字体 fontpath size 图集规格 color-depth)
  (make-游戏字体 ;; 'font
		 (ttf-open-font fontpath size)
		 ;; '图集表面
		 (sdl-create-rgb-surface 0 图集规格 图集规格 color-depth 0 0 0 #xff)
		 ;; '图集规格
		 图集规格
		 ))

(define (字体图集表面背景透明化! 图集表面)
  ;; (测试输出 2)
  (when (< 0 (sdl-set-color-key! 图集表面 SDL-TRUE
				 (sdl-map-rgba (ftype-ref SDL_Surface (format) 图集表面)
					       0 0 0 0)))
    (error 'sttf-扩展 "图集颜色key设置失败")) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
  )

(define (get-已扩展字符 game-字体)
  (hashtable-keys (get-字符-取用框ht game-字体)))

(define current-font (make-parameter '未定义字体))

(define (创建字符串 字符串 game-字体 renderer)
  (let* ((res
	  (make-游戏字符串 game-字体 字符串))
	 )
    (字体扩展字符! game-字体 (string->list 字符串) renderer)
    res))

(define (字体扩展字符! game-字体 charset renderer)
  ;; 与其返回全部扩展的字符,判断新的字符是否已经扩展,不如逐个遍历,判断是否已经是哈希表的键. 2024年8月24日11:38:48
  ;; (测试输出 charset)
  (let loop ((剩余字符ls charset))
    (if (null? 剩余字符ls)
	(when (get-纹理脏? game-字体)
	  (set-图集纹理! game-字体 (sdl-create-texture-from-surface renderer (get-图集表面 game-字体)))
	  (set-纹理脏?! game-字体 #f)
	  ) ;当图集扩展完之后,整个surface创建纹理,ds的指导--2025年11月26日11:52:13
	(let ((当前字符 (car 剩余字符ls)))
	  (unless (get-字符-取用框 game-字体 当前字符) ;没扩展过的字符,直接扩展
	    (字符图集扩展 game-字体 (car 剩余字符ls) renderer))
	  (loop (cdr 剩余字符ls))) 
	)))

(define %字符默认颜色 (make-sdl-color 255 255 255 #xff))

(define (字符图集扩展 game-字体 char renderer)
  (let* ((字符表面 (ttf-render ttf-render-utf8-blended (get-font game-字体) (string char) %字符默认颜色))
	 (whls (ttf-size-text-m (get-font game-字体) (string char) 'utf8))
	 (字符w (car whls))
	 (字符h (cadr whls))
	 (空白格坐标 (get-第一个空白格坐标ls game-字体))
	 (索引框x (car 空白格坐标))
	 (索引框y (cadr 空白格坐标))
	 (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
	 (图集规格 (get-图集规格 game-字体))
	 (当前扩展次数 (get-扩展次数 game-字体))
	 )
    ;; (测试输出 (list whls 当前扩展次数 空白格坐标))
    
    (when (> (+ 索引框x 字符w) 图集规格)		;超过行宽
      (sdl-rect-x-set! 索引框 0)
      (sdl-rect-y-set! 索引框 (+ 索引框y 字符h))
      )

    (when (> (+ (sdl-rect-y 索引框) 字符h) (* 当前扩展次数 图集规格)) ;超过下限的情况,如果一开始指定的图集就太扁,不换行就会超过下限,这里必须要获取局部状态,因为可能换行,用索引框y会导致依然在判断上一行的y+h
      (扩展字体图集! game-字体 图集规格 当前扩展次数))
    
    (sdl-blit-surface 字符表面 (make-sdl-rect 0 0 字符w 字符h)
		      (get-图集表面 game-字体) 索引框) ;把新扩展的字符表面blit到字符图集上
    (hashtable-set! (get-字符-取用框ht game-字体) char 索引框)
    (set-第一个空白格坐标ls! game-字体 (list (+ 字符w (sdl-rect-x 索引框)) (sdl-rect-y 索引框))) ;x肯定要移动一位,
    (set-纹理脏?! game-字体 #t)
    ))

(define (扩展字体图集! game-字体 图集规格 当前扩展次数)
  (let* ((当前图集 (get-图集表面 game-字体))
	 (当前图集高度 (* 图集规格 当前扩展次数))
	 (当前图集矩形 (make-sdl-rect 0 0 图集规格 当前图集高度)))
    (set-图集表面! game-字体 (sdl-create-rgb-surface 0 图集规格 (+ 当前图集高度 图集规格) 32 0 0 0 #xff)) ;这里还是不够完善,写死了颜色位深....
    (set-扩展次数! game-字体 (+ 当前扩展次数 1))
    (字体图集表面背景透明化! (get-图集表面 game-字体)) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
    (sdl-blit-surface 当前图集 当前图集矩形 
		      (get-图集表面 game-字体) 当前图集矩形) ;把之前的图集blit到扩展后的上面去
    ))

(define (set-字体扩展字符! 游戏字符串 game-font renderer)
  ;; 每次给游戏用字符串赋值后,与其取新字符串同旧字符串不同的部分之后再判断是否扩展,比如直接逐个遍历 2024年8月24日11:40:01
  (字体扩展字符! game-font (string->list (get-字符串 游戏字符串)) renderer)	;仅对修改后的字体扩展当前的字符,之前字体的字符不见.
  (set-game-字体! 游戏字符串 game-font))

(define (get-字符-取用框 game-字体 char)
  (hashtable-ref (get-字符-取用框ht game-字体) char #f
		 ;; (string-append "没扩展的:" (make-string 1 char))  不返回#f会导致判断字符是否扩展出错
		 ))

;;; 构造布局信息,避免渲染时候重复计算 2025年12月3日11:51:03
(define-record-type 布局文本		;如果每个字符的位置,规格,颜色,都不一样就得考虑原本共享的信息分别化保存了
  (fields 
   字体              ; 游戏字体对象
   布局字符v            ; 要渲染的字符串,其实好像不如直接blit一个大纹理
   ;; 对齐方式          ; 对齐设置,如果需要重新布局的话,最好能获取原本的布局方式,不然要传递参数穿透作用域,可能很麻烦....
   字符旋转角度
   字符旋转中心点
   字符镜像)         ; 
  (nongenerative)
  )

(define-record-type 布局字符信息
  (fields
   字符
   目标框)
  (nongenerative)
  )

(define (按布局渲染文本 布局 renderer)
  ;; 所以为啥不直接渲染一个装了一堆字的纹理?
  (let* ((end (vector-length (布局文本-布局字符v 布局)))
	 (game-字体 (布局文本-字体 布局))
	 (图集纹理 (get-图集纹理 game-字体))
	 (旋转角度 (布局文本-字符旋转角度 布局))
	 (中心点 (布局文本-字符旋转中心点 布局))
	 (镜像flag (布局文本-字符镜像 布局)))
    (let loop ((i 0))
      (cond ((= i end) '())
	    (else
	     (let ((布局字符 (vector-ref (布局文本-布局字符v 布局) i)))
	       (sdl-render-copy-ex renderer 图集纹理 (get-字符-取用框 game-字体 (布局字符信息-字符 布局字符)) (布局字符信息-目标框 布局字符) 旋转角度 中心点 镜像flag)
	       (loop (+ i 1)))
	     ))
      )))

(define (创建文本布局信息 game-字体 renderer 目标字符串 init-x init-y
			  r g b a
			  对齐方式 区域宽度 区域高度 字符x间距 字符y间距 ;; 显示方式 排列方向单位向量
			  w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag)
  (when (or (<=  区域高度 0) (<=  区域宽度 0))
    (error '创建文本布局信息 "当前区域无法渲染" (list 区域宽度 区域高度)))
  (let* ((字串长 (string-length 目标字符串))
	 (布局字符v (make-vector 字串长))
	 (估计的字符宽度 (* 2 (get-ASCII字符宽度 game-字体)))
	 (字符高度 (get-ASCII字符高度 game-字体)) ;这样估计高度不行,需要用取用框的高度
	 (全部放进区域需要的缩放系数 (缩放系数计算 字串长
						   (* w缩放系数 估计的字符宽度) (* h缩放系数 字符高度)
						   区域宽度 区域高度
						   字符x间距 字符y间距))
	 (缩放后的预计字符宽度 (* 估计的字符宽度 w缩放系数 全部放进区域需要的缩放系数))
	 (估计的每行字符数 (+ 1 (ceiling (/ (- 区域宽度 缩放后的预计字符宽度) (+ 缩放后的预计字符宽度 字符x间距)))))
	 (适应后w缩放系数 (* w缩放系数 全部放进区域需要的缩放系数))
	 (适应后h缩放系数 (* h缩放系数 全部放进区域需要的缩放系数))
	 (行高 (+ 字符y间距 (* h缩放系数 全部放进区域需要的缩放系数 (get-ASCII字符高度 game-字体)))))
    (let loop (
	       (剩余字符串 目标字符串)
	       (当前y值 init-y)
	       (当前x值 init-x)
	       (i 0))
      (cond ((string=? 剩余字符串 "") (list (+ 当前x值 字符x间距) 当前y值))
	    ((string=? (string-take 剩余字符串 1) "\n") (loop (string-drop 剩余字符串 1)
							      (+ 当前y值 行高)
							      init-x
							      i)) 
	    (else
	     (let ((估计的渲染部分字符个数 (exact (min 估计的每行字符数 (or (string-index 剩余字符串 #\newline) +inf.0) (string-length 剩余字符串)))))
	       (let-values ([(被渲染的部分 w-acc 剩余部分)
			     (取用框计算acc game-字体 renderer
					    (string-take 剩余字符串 估计的渲染部分字符个数)
					    (string-drop 剩余字符串 估计的渲染部分字符个数)
					    区域宽度 字符x间距 适应后w缩放系数
					    )])
		 (let-values ([(当前x 当前ref)
			       (创建单行布局信息! 布局字符v game-字体 i  被渲染的部分 (case 对齐方式
											[(居中) (- init-x (/ w-acc 2))]
											[(右对齐) (+ init-x w-acc)]
											[else init-x]) 当前y值
											适应后w缩放系数 适应后h缩放系数  字符x间距)])
		   (loop 剩余部分
			 (+ 当前y值 (* 适应后h缩放系数 字符高度) 字符y间距)
			 当前x
			 当前ref
			 ))
		 )))))
    (make-布局文本 game-字体 布局字符v 字符旋转角度 中心点 镜像flag)
    )
  
  )

(define (创建单行布局信息! 布局字符v game-字体 初始ref 目标字符串 init-x init-y w缩放系数 h缩放系数 字符x间距)
  (let loop ((charls (string->list 目标字符串))
	     (x init-x)
	     (i 初始ref))
    (cond ((null? charls) (values x i))
	  (else
	   (let* ((当前字符 (car charls))
		  (索引框 (get-字符-取用框 game-字体 当前字符))
		  (字符宽度 (* (sdl-rect-w 索引框) w缩放系数)))
	     (vector-set! 布局字符v i (make-布局字符信息 当前字符 (make-sdl-rect x init-y 字符宽度 (* (sdl-rect-h 索引框) h缩放系数))))
	     (loop (cdr charls)
		   (+ x 字符宽度 字符x间距)
		   (+ i 1)))))))

(define (取用框计算acc game-字体 renderer 当前字符串 剩余字符串 区域宽度 字符x间距 w缩放系数)
  ;; 需要选取合适的字符串,返回宽度用来计算居中或者右对齐的初始渲染x坐标,同时返回选取的字符串用于渲染
  ;; 
  (let loop ((当前宽度 (+ (* (- (string-length 当前字符串) 1) 字符x间距)
			  (* (apply + (map (lambda (char)
					     (sdl-rect-w (get-字符-取用框 game-字体 char)   ;; (字符扩展且返回取用框 game-字体 char renderer)
							 )) (string->list 当前字符串))) w缩放系数)))
	     (当前字符串 当前字符串)
	     (剩余字符串 剩余字符串))
    (cond ((> 当前宽度 区域宽度) 	;估计的宽度实际计算下来超过了区域宽度,减少当前字符串的最后一个字符,增加剩余字符串
	   (let* ((减少字符  (string-ref (string-take-right 当前字符串 1) 0))
		  (减少后宽度 (- 当前宽度 字符x间距 (* w缩放系数 (sdl-rect-w (get-字符-取用框 game-字体 减少字符))))))
	     (if (< 减少后宽度 区域宽度)
		 (values (string-drop-right 当前字符串 1) 减少后宽度 (string-append (string-take-right 当前字符串 1) 剩余字符串))
		 (loop 减少后宽度
		       (string-drop-right 当前字符串 1)
		       (string-append (string-take-right 当前字符串 1) 剩余字符串))))
	   )
	  ((string=? "" 剩余字符串)	;剩余部分为空,不需要去掉换行符,直接返回
	   (values 当前字符串 当前宽度 剩余字符串))
	  ((and (string=? "\n" (string-take 剩余字符串 1)) ;剩下部分的第一个字符是换行符且当前宽度没超过区域宽度,不需要尝试增加字符
		(<= 当前宽度 区域宽度))
	      (values 当前字符串 当前宽度 (string-drop 剩余字符串 1))) ;因为返回之后渲染并且换行了,必须去掉剩余部分开头的换行符 2024年8月23日00:59:08
	  (else
	   (let* ((新增字符 (string-ref 剩余字符串 0))
		  (增加后宽度 (+ 当前宽度 字符x间距 (* w缩放系数 (sdl-rect-w (get-字符-取用框 game-字体 新增字符) ;; (字符扩展且返回取用框 game-字体 新增字符 renderer)
																     )))))
	     (if (> 增加后宽度 区域宽度)
		 (values 当前字符串 当前宽度 剩余字符串)
		 (loop 增加后宽度
		       (string-append 当前字符串 (string-take 剩余字符串 1))
		       (string-drop 剩余字符串 1))))))))

(define (缩放系数计算 字符个数 字符宽度 字符高度 区域宽 区域高 字间距 行间距)
  ;; 假设每个字符的长和宽都一样 2024年8月21日19:07:05
  (if (= 字符个数 0)
      1
      (let loop ((每行字符数 (floor (/ 区域宽 字符宽度)))
		 (当前缩小系数 1)
		 )
	(let* ((行数 (ceiling (/ 字符个数 每行字符数)))
	       (减少一行后的每行字符数 (if (<= 行数 1)
					   字符个数
					   (ceiling (/ 字符个数 (- 行数 1)))))
	       (减少行数的缩小系数 (/ (- 区域宽 (* 字间距 (- 每行字符数 1))) 减少一行后的每行字符数 字符宽度))
	       (按当前行数铺满的缩小系数 (/ (- 区域高 (* 行间距 (- 行数 1))) 行数 字符高度))
	       )
	  (cond ((< 当前缩小系数 按当前行数铺满的缩小系数) 当前缩小系数)
		((<= 减少行数的缩小系数 按当前行数铺满的缩小系数) 按当前行数铺满的缩小系数)
		(else
		 (loop 减少一行后的每行字符数
		       减少行数的缩小系数)))
	  )
	)))

(define (charls->u8string charls)
  ;; 依赖windows
  (multibyte->string 'cp-acp (u8-list->bytevector (map char->integer charls))))


;;; 使用了current-font之后
(define (创建字符串-ctcr 字符串)
  (let* ((res
	  (make-游戏字符串 (current-font) 字符串))
	 )
    (字体扩展字符-ctcr! (string->list 字符串))
    res))

(define (字体扩展字符-ctcr! charset)
  ;; 与其返回全部扩展的字符,判断新的字符是否已经扩展,不如逐个遍历,判断是否已经是哈希表的键. 2024年8月24日11:38:48
  (let loop ((剩余字符ls charset))
    (if (null? 剩余字符ls)
	'()
	(let ((当前字符 (car 剩余字符ls)))
	  ;; (测试输出 当前字符)
	  (when (not (get-字符-取用框 (current-font) 当前字符))
	    (字符图集扩展 (current-font) (car 剩余字符ls) (current-renderer)))
	  (loop (cdr 剩余字符ls))))))

(define (set-字符串扩展字符-ctcr! 游戏字符串 字符串)
  ;; 每次给游戏用字符串赋值后,与其取新字符串同旧字符串不同的部分之后再判断是否扩展,比如直接逐个遍历 2024年8月24日11:40:01
  (字体扩展字符-ctcr! (string->list 字符串))
  (set-字符串! 游戏字符串 字符串))

(define (set-字体扩展字符-cr! 游戏字符串 new-font)
  ;; 每次给游戏用字符串赋值后,与其取新字符串同旧字符串不同的部分之后再判断是否扩展,比如直接逐个遍历 2024年8月24日11:40:01
  (字体扩展字符! new-font (string->list (get-字符串 游戏字符串)) (current-renderer))	;仅对修改后的字体扩展当前的字符,之前字体的字符不见.
  (set-game-字体! 游戏字符串 new-font))

