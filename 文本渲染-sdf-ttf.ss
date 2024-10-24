;;;
(define (创建字体 fontpath size 图集规格 color-depth)
  (let* ((res
	  (make-游戏字体 'font (ttf-open-font fontpath size)
			 '图集表面 (sdl-create-rgb-surface 0 图集规格 图集规格 color-depth 0 0 0 #xff)
			 '图集规格 图集规格
			 ))
	 (whls (ttf-size-text-m (get-font res) "0" 'utf8)))
    ;; (测试输出 (get-font res))
    (when (= 0 (ftype-pointer-address (get-font res)))
      (error '创建字体 "无法从fontpath加载字体" fontpath)) 
    (字体图集表面背景透明化! res)
    (set-ASCII字符宽度! res (car whls))
    (set-ASCII字符高度! res (cadr whls))
    res))

(define (字体图集表面背景透明化! game-字体)
  ;; (测试输出 2)
  (when (< 0 (sdl-set-color-key! (get-图集表面 game-字体) SDL-TRUE
				 (sdl-map-rgba (ftype-ref SDL_Surface (format) (get-图集表面 game-字体))
					       0 0 0 0)))
    (error 'sttf-扩展 "图集颜色key设置失败")) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
  ;; (测试输出 3)
  ;; (测试输出 (get-font game-字体))
  ;; (set-图集表面! game-字体 (sdl-convert-surface (get-图集表面 game-字体)
  ;; 						(ftype-ref SDL_Surface (format)
  ;; 							   ;; text
  ;; 							   (ttf-render ttf-render-utf8-blended (get-font game-字体) "0" (make-sdl-color 0 0 0 #xff))
  ;; 							   )
  ;; 						0)) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
  ;; (sdl-convert-surface (get-图集表面 game-字体)
  ;; 		       (ftype-ref SDL_Surface (format)
  ;; 							   ;; text
  ;; 				  (ttf-render ttf-render-utf8-blended (get-font game-字体) "0" (make-sdl-color 0 0 0 #xff))
  ;; 				  )
  ;; 		       0)
  ;; (测试输出 (get-图集表面 game-字体))
  )

(define (get-已扩展字符 game-字体)
  (hashtable-keys (get-字符-取用框ht game-字体)))

(define (list->set ls)
  (cond
   [(null? ls) '()]
   [(member (car ls) (cdr ls)) (list->set (cdr ls))]
   [else (cons (car ls) (list->set (cdr ls)))]))


(define (创建字符串 字符串 game-字体 renderer)
  (let* ((res
	  (make-游戏字符串 '字符串 字符串
			   'game-字体 game-字体
			   ))
	 )
    (字体扩展字符! game-字体 (list->set (string->list 字符串)) renderer)
    res))

(define (字体扩展字符! game-字体 charset renderer)
  ;; 与其返回全部扩展的字符,判断新的字符是否已经扩展,不如逐个遍历,判断是否已经是哈希表的键. 2024年8月24日11:38:48
  (测试输出 charset)
  (let loop ((剩余字符ls charset))
    (if (null? 剩余字符ls)
	'()
	(begin (let ((当前字符 (car 剩余字符ls)))
		 ;; (测试输出 当前字符)
		 (when (not (get-字符-取用框 game-字体 当前字符))
		   (字符图集扩展 game-字体 (car 剩余字符ls) renderer))
		 (loop (cdr 剩余字符ls)))))))

(define (字符图集扩展 game-字体 char renderer)
  (let* ((字符表面 (ttf-render ttf-render-utf8-blended (get-font game-字体) (string char) (make-sdl-color 255 255 255 #xff)))
	 (whls (ttf-size-text-m (get-font game-字体) (string char) 'utf8))
	 (字符w (car whls))
	 (字符h (cadr whls))
	 (空白格坐标 (get-第一个空白格坐标ls game-字体))
	 (索引框x (car 空白格坐标))
	 (索引框y (cadr 空白格坐标))
	 (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
	 (图集规格 (get-图集规格 game-字体))
	 (扩展后x (+ 索引框x 字符w))
	 )
    (when (> (+ 索引框y 字符h) 图集规格) ;超过下限的情况
      (let ((当前图集 (get-图集表面 game-字体))
	    (当前扩展次数 (get-扩展次数 game-字体))
	    (当前图集高度 (* 图集规格 当前扩展次数))
	    (当前图集矩形 (make-sdl-rect 0 0 图集规格 当前图集高度)))
	(set-图集表面! game-字体 (sdl-create-rgb-surface 0 图集规格 (+ 当前图集高度 图集规格) color-depth 0 0 0 #xff))
	(set-扩展次数! game-字体 (+ 当前扩展次数 1))
	(字体图集表面背景透明化! game-字体) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
	(sdl-blit-surface 当前图集 当前图集矩形 
			  (get-图集表面 game-字体) 当前图集矩形) ;把之前的图集blit到扩展后的上面去
	))
   
    (when (> 扩展后x (get-图集规格 game-字体))
      (set! 索引框 (make-sdl-rect 0 (+ 索引框y 字符h 1) 字符w 字符h)))
    (sdl-blit-surface 字符表面 (make-sdl-rect 0 0 字符w 字符h)
		      (get-图集表面 game-字体) 索引框) ;把新扩展的字符表面blit到字符图集上
    (hashtable-set! (get-字符-取用框ht game-字体) char 索引框)
    (set-第一个空白格坐标ls! game-字体 (list (+ 字符w (sdl-rect-x 索引框)) (sdl-rect-y 索引框)))
    (set-图集纹理! game-字体 (sdl-create-texture-from-surface renderer (get-图集表面 game-字体)))))

(define (set-字符串扩展字符! 游戏字符串 字符串 renderer)
  ;; 每次给游戏用字符串赋值后,与其取新字符串同旧字符串不同的部分之后再判断是否扩展,比如直接逐个遍历 2024年8月24日11:40:01
  (字体扩展字符! (get-game-字体 游戏字符串) (list->set (string->list 字符串)) renderer)
  (set-字符串! 游戏字符串 字符串))

(define (set-字体扩展字符! 游戏字符串 game-font renderer)
  ;; 每次给游戏用字符串赋值后,与其取新字符串同旧字符串不同的部分之后再判断是否扩展,比如直接逐个遍历 2024年8月24日11:40:01
  (字体扩展字符! game-font (list->set (string->list (get-字符串 游戏字符串))) renderer)
  (set-game-字体! 游戏字符串 game-font))

(define (get-字符-取用框 game-字体 char)
  (hashtable-ref (get-字符-取用框ht game-字体) char #f
		 ;; (string-append "没扩展的:" (make-string 1 char))  不返回#f会导致判断字符是否扩展出错
		 ))

(define (矩形区域渲染字符串 game-字体 renderer 目标字符串 init-x init-y
			    r g b a
			    对齐方式 区域宽度 区域高度 字符x间距系数 字符y间距系数 显示方式 排列方向单位向量
			    w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag)
  ;; 区域高度传入 +inf.0 就可以得到不限高度的形式
  ;; 因为不知道被渲染的字符串多长,这个init-x还是需要根据对齐方式输入不同的参数,永远是渲染空字符串时候的光标位置 2024年8月25日11:37:59
  (when (or (<=  区域高度 0) (<=  区域宽度 0))
    (error '矩形区域渲染字符串 "当前区域无法渲染" (list 区域宽度 区域高度)))
  (let* ((估计的字符宽度 (* 2 (get-ASCII字符宽度 game-字体)))
	 (字符高度 (get-ASCII字符宽度 game-字体)) ;这样估计高度不行,需要用取用框的高度
	 (全部放进区域需要的缩放系数 (缩放系数计算 (string-length 目标字符串)
						   (* w缩放系数 估计的字符宽度) (* h缩放系数 字符高度)
						   区域宽度 区域高度
						   字符x间距系数 字符y间距系数)
				     )
	 (缩放后的预计字符宽度 (* 估计的字符宽度 w缩放系数 全部放进区域需要的缩放系数))
	 (估计的每行字符数 (+ 1 (ceiling (/ (- 区域宽度 缩放后的预计字符宽度) (+ 缩放后的预计字符宽度 字符x间距系数)))))
	 ;; (初始index (min (- 1 估计的每行字符数) (string-index 目标字符串 #\newline)))
	 )
    (let loop (;; (当前ind 初始index)
	       (剩余字符串 目标字符串)
	       (当前y值 init-y)
	       (当前x值 init-x))
      (cond ((string=? 剩余字符串 "") (list (+ 当前x值 字符x间距系数) 当前y值))
	    ((string=? (string-take 剩余字符串 1) "\n") (loop (string-drop 剩余字符串 1)
							     (+ 当前y值 (* h缩放系数 全部放进区域需要的缩放系数 (get-ASCII字符宽度 game-字体))
								字符y间距系数)
							     init-x)) 
	    (else
	     (let ((估计的渲染部分字符个数 (exact (min 估计的每行字符数 (or (string-index 剩余字符串 #\newline) +inf.0) (string-length 剩余字符串)))))
	       (let-values ([(被渲染的部分 w-acc 剩余部分)
			     (取用框计算acc game-字体 renderer
					    (string-take 剩余字符串 估计的渲染部分字符个数)
					    (string-drop 剩余字符串 估计的渲染部分字符个数)
					    区域宽度 字符x间距系数 w缩放系数 全部放进区域需要的缩放系数
					    )])
		 (loop 剩余部分
		       (+ 当前y值 (* h缩放系数 全部放进区域需要的缩放系数 字符高度)
			  字符y间距系数)
		       (字符串单行渲染 game-字体 renderer  被渲染的部分 (case 对齐方式
									  [(居中) (- init-x (/ w-acc 2))]
									  [(右对齐) (+ init-x w-acc)]
									  [else init-x]) 当前y值
									  r g b a 字符x间距系数
									  (* w缩放系数 全部放进区域需要的缩放系数) (* h缩放系数 全部放进区域需要的缩放系数)
									  字符旋转角度 中心点 镜像flag)))))))))

(define (字符串单行渲染 game-字体 renderer 目标字符串 init-x init-y
			r g b a 字符x间距系数 w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag)
  (let loop ((charls (string->list 目标字符串))
	     (x init-x))
    (cond ((null? charls) x)
	  (else
	   (let* ((索引框 (get-字符-取用框 game-字体 (car charls)))
		  (字符宽度 (* (sdl-rect-w 索引框) w缩放系数)))
	     (sdl-render-copy-ex renderer (get-图集纹理 game-字体) 索引框
				 (make-sdl-rect x init-y 字符宽度 (* (sdl-rect-h 索引框) h缩放系数))
				 字符旋转角度 中心点 镜像flag)
	     (loop (cdr charls)
		   (+ x 字符宽度)))))))

(define (取用框计算acc game-字体 renderer 当前字符串 剩余字符串 区域宽度 字符x间距系数 w缩放系数 区域限定缩放系数)
  ;; 需要选取合适的字符串,返回宽度用来计算居中或者右对齐的初始渲染x坐标,同时返回选取的字符串用于渲染
  ;; 
  (let loop ((当前宽度 (+ (* (- (string-length 当前字符串) 1) 字符x间距系数)
			  (* (apply + (map (lambda (char)
					     (sdl-rect-w (get-字符-取用框 game-字体 char)   ;; (字符扩展且返回取用框 game-字体 char renderer)
							 )) (string->list 当前字符串))) w缩放系数 区域限定缩放系数)))
	     (当前字符串 当前字符串)
	     (剩余字符串 剩余字符串))
    (cond ((> 当前宽度 区域宽度) 	;估计的宽度实际计算下来超过了区域宽度,减少当前字符串的最后一个字符,增加剩余字符串
	   (let* ((减少字符  (string-ref (string-take-right 当前字符串 1) 0))
		  (减少后宽度 (- 当前宽度 字符x间距系数 (* w缩放系数 区域限定缩放系数 (sdl-rect-w (get-字符-取用框 game-字体 减少字符))))))
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
		  (增加后宽度 (+ 当前宽度 字符x间距系数 (* w缩放系数 区域限定缩放系数 (sdl-rect-w (get-字符-取用框 game-字体 新增字符) ;; (字符扩展且返回取用框 game-字体 新增字符 renderer)
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
	))  )


(define (字符扩展且返回取用框 game-字体 char renderer)
  (when (not (get-字符-取用框 game-字体 char))
    (字符图集扩展 game-字体 char renderer))
  (get-字符-取用框 game-字体 char)
  )

  
(define (单字符渲染 char game-字体 renderer x y r g b a w缩放系数 h缩放系数 旋转角度 中心点 镜像flag)
  (let ((索引框 (get-字符-取用框 game-字体 char)))
    (when (not 索引框)
      (字符图集扩展 game-字体 char renderer)
      (set! 索引框 (get-字符-取用框 game-字体 char)))
    (sdl-render-copy-ex renderer (get-图集纹理 game-字体) 索引框
			(make-sdl-rect x y (* (sdl-rect-w 索引框) w缩放系数) (* (sdl-rect-h 索引框) h缩放系数))
			旋转角度 中心点 镜像flag)
    ))


(define (charls->u8string charls)
  ;; 依赖windows
  (multibyte->string 'cp-acp (u8-list->bytevector (map char->integer charls))))
