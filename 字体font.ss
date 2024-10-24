;;;
;; (ttf-open-font ttf-path fontsize)	;得到字体类型
;; TTF_SetFontSize				;动态调整字体大小
;; (ttf-render ttf-render-fun 字体 str sdl-color) ;得到表面
;; (sdl-create-texture-from-surface render 字体表面) ;从表面创建纹理

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

;;; 

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
  ;; 
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
		  ;; (text (ttf-render ttf-render-utf8-blended font "0" (make-sdl-color 0 0 0 #xff)))
		  )
	     (when (< 0 (sdl-set-color-key! surface SDL-TRUE (sdl-map-rgba (ftype-ref SDL_Surface (format) surface) 0 0 0 0)))
	       (error 'sttf-扩展 "图集颜色key设置失败")) ;确保背景透明,必须在convert前这样处理,顺序颠倒的话,字符也会透明,什么都不显示
	     (set! surface (sdl-convert-surface surface
						(ftype-ref SDL_Surface (format)
							   ;; text
							   (ttf-render ttf-render-utf8-blended font "0" (make-sdl-color 0 0 0 #xff))
							   )
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
			       ))))	;最后设置字体路径对应的值为图集纹理,字符和格子的ht构成的list
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
				     (text (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff)))	;这个颜色应该也可以参数化;这个text即使被赋值,也会让导致垃圾回收.... 2024年8月16日18:04:19
				     (whls (ttf-size-text-m font (string 字符) 'utf8))
				     (字符w (car whls))
				     (字符h (cadr whls))
				     (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
				     )
				;; (display whls)
				;; (newline)
				;; (sdl-save-bmp text (string 字符))
				;; (set! text (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff)))
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
	   (let* ((图集规格 240)
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
				       ;; (text (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff)))  ;这个颜色应该也可以参数化
				       (whls (ttf-size-text-m font (string 字符) 'utf8))
				       (字符w (car whls))
				       (字符h (cadr whls))
				       (索引框 (make-sdl-rect 索引框x 索引框y 字符w 字符h))
				       )
				  ;; (display whls)
				  ;; (newline)
				  ;; (sdl-save-bmp text (string 字符))
				  (sdl-blit-surface (ttf-render ttf-render-utf8-blended font (string 字符) (make-sdl-color 255 255 255 #xff))
						    (make-sdl-rect 0 0 字符w 字符h)
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

(define (sttf字符串动态扩展渲染 sttf render fontpath string init-x init-y
				r g b
				对齐方式 字符框宽度 字符x间距系数 字符y间距系数 显示方式
				w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag)
  (sttf-扩展 sttf fontpath str render 字体加载尺寸)
  (sttf字符串渲染 sttf render fontpath string init-x init-y
		  r g b
		  对齐方式 字符框宽度 字符x间距系数 字符y间距系数 显示方式
		  w缩放系数 h缩放系数 字符旋转角度 中心点 镜像flag))

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

