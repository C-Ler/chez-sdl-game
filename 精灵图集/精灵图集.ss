(load "D://code//aim4//game-uk//l-engine-2.ss")


;; (define 窗口宽 1280)
;; (define 窗口高 960)
;; (define 帧率 160)
;; (define NULL)

;; (define wintitle "字符渲染")


;; (define game-字符渲染 (创建game SDL-INIT-EVERYTHING IMG_INIT_EVERYTHING (bitwise-ior MIX_INIT_FLAC MIX_INIT_MP3 MIX_INIT_OGG)
;; 				(/ 1000 帧率) wintitle SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高))

(define (构造二叉树叶子 obj)
  (cons obj (cons '() '())))

(define (二叉树叶子? tree)
  (and (list? tree) (not (null? tree)) (equal? (cdr tree) (cons '() '()))))

(define (get-obj node/leaf)
  (car node/leaf))

(define (get-left node)
  (cadr node))

(define (get-right node)
  (cddr node))

;; (define (分叉! node)
;;   (set-cdr! node )
;;   )
(define (新增叶子! btree obj 更新foo!)
  (call/cc
   (lambda (k)
     (letrec ((res (lambda (btree)
		     (cond ((null? btree) #f)
			   ((二叉树叶子? btree) (cond ((更新foo! btree obj) (k #t))
						      (else (res (get-left btree))
							    (res (get-right btree))
							    )
						      
						      )
			    ;; (if (pred obj (get-obj btree))
			    ;;     (begin (set-car! btree (更新foo obj (get-obj btree)))
			    ;; 	      (set-cdr! btree (分叉foo obj (get-obj btree)))
			    ;; 	      (k))
			    ;;     #f
			    ;;     )
			    )
			   (else 
			    (res (get-left btree))
			    (res (get-right btree))
			    )))))
       (res btree))))
  )

(define-sdf-property gobj:name
  name
  'predicate string?
  'default-value "")

(define-sdf-property gobj:图集节点x
  节点x
  'predicate integer?
  'default-value 0)

(define-sdf-property gobj:图集节点y
  节点y
  'predicate integer?
  'default-value 0)

(define-sdf-property gobj:图集节点w
  节点w					;这个不能命名为w,不然会影响引擎表面的get-w和get-h
  'predicate integer?
  'default-value 0)

(define-sdf-property gobj:图集节点h
  节点h
  'predicate integer?
  'default-value 0)

(define-sdf-property gobj:游戏表面
  游戏表面
  'predicate 游戏表面?)

(define-type 当前图像 (游戏表面?) (gobj:name))

(define (创建名称图像 path)
  (let* ((表面 (sdl-convert-surface-format (img-load path) SDL_PIXELFORMAT_RGBA32 0)) ;由于传入的表面 pixelformat是24位的,修改像素有些麻烦,所以最后还是用了去年的策略,先转换格式  2023年5月7日20:34:24
	 )
    (make-当前图像 'name path '表面 表面 '像素指针 (make-ftype-pointer unsigned-32 (ftype-ref SDL_Surface (pixels) 表面)))))

;; (define-type 图集扩展节点 (当前图像?) (gobj:图集节点x gobj:图集节点y)) 这样估计没法实现共享部分
(define-type 图集扩展节点 () (gobj:游戏表面 gobj:name gobj:图集节点x gobj:图集节点y gobj:图集节点w gobj:图集节点h))

(define (创建图集节点 游戏表面 name x y w h)
  (make-图集扩展节点 '游戏表面 游戏表面 'name name '节点x x '节点y y '节点w w '节点h h))

(define (图集扩展更新二叉树 btree obj)
  ;; 需要一个共享的变量保存间隔值
  ;; obj是自定义的游戏表面类型
  ;; btree不仅要保存坐标和规格,还需要共享图集
  (let* ((当前obj (get-obj btree))
	 (当前x (get-节点x 当前obj))
	 (当前y (get-节点y 当前obj))
	 (当前w (get-节点w 当前obj))
	 (当前h (get-节点h 当前obj))
	 (节点name (get-name obj))
	 (w (get-w obj))
	 (h (get-h obj)))
    (cond [(and (>= 当前w w) (>= 当前h h))
	   (测试输出 1)
	   (sdl-blit-surface (get-表面 obj) (make-sdl-rect 0 0 w h) (get-表面 (get-游戏表面 当前obj)) (make-sdl-rect 当前x 当前y w h))
	   (set-节点w! 当前obj w)
	   (set-节点h! 当前obj h)
	   (set-name! 当前obj 节点name)
	   (set-cdr! btree (cons (构造二叉树叶子 (创建图集节点 (get-游戏表面 当前obj) "空节点"
							       (+ 当前x w 间隔值) 当前y (- 当前w w 间隔值) h)
						 ;; (make-sdl-rect (+ 当前x w 间隔值) 当前y (- 当前w w 间隔值) h)
						 )
				 (构造二叉树叶子 (创建图集节点 (get-游戏表面 当前obj) "空节点"
							       当前x (+ 当前y h 间隔值) 当前w (- 当前h h 间隔值)))))
	   (测试输出 2)
	   
	   ]
	  [(and (>= 当前w h) (>= 当前h w))
	   (精灵图用旋转blit obj (get-游戏表面 当前obj) 当前x 当前y)
	   (set-节点w! 当前obj h)
	   (set-节点h! 当前obj w)
	   (set-name! 当前obj 节点name)
	   (set-cdr! btree (cons (构造二叉树叶子 (创建图集节点 (get-游戏表面 当前obj) "空节点" (+ 当前x h 间隔值) 当前y (- 当前w h 间隔值) w))
				 (构造二叉树叶子 (创建图集节点 (get-游戏表面 当前obj) "空节点" 当前x (+ 当前y w 间隔值) 当前w (- 当前h w 间隔值)))))
	   
	   ]
	  [else #f ;不能在这时扩展图集,因为只尝试了一个叶子,应该回去继续遍历btree剩下的叶子
	   ])
    
    ))

(define (空节点? 图集节点)
  (equal? (get-name 图集节点) "空节点"))

(define (扩展图集! 图集btree 当前表面)
  (let* ((图集节点 (car 图集btree))
	 (图集表面 (get-游戏表面 图集节点))
	 (图集w (get-w 图集表面))
	 (图集h (get-h 图集表面)))
    (测试输出 图集w)
    (测试输出 图集h)
    (let-values ([(扩展后表面 矩形框)
		  (let ((扩展后宽 (+ 图集w (get-w 当前表面)))
			(扩展后高 (+ 图集h (get-h 当前表面)))) ;向右生长的条件
		    (if (>= 图集h (+ 图集w (get-w 当前表面)))
			(begin
			  (向右生长 图集btree (get-w 当前表面))
			  ;; (set-节点w! 图集节点 (+ (get-节点w 图集节点) (get-w 当前表面)))
			  (values
			   (sdl-create-rgb-surface 0 扩展后宽 图集h 32 0 0 0 #xff)
			   (make-sdl-rect 0 0 扩展后宽 图集h)))
			(begin
			  (向下生长 图集btree (get-h 当前表面))
			  (values
			   (sdl-create-rgb-surface 0 图集w 扩展后高 32 0 0 0 #xff)
			   (make-sdl-rect 0 0 图集w 扩展后高)
			   ))))])
      (sdl-blit-surface (get-表面 图集表面) (make-sdl-rect 0 0 图集w 图集h) 扩展后表面 (make-sdl-rect 0 0 (+ 图集w (get-w 当前表面)) 图集h))
      (set-表面! (get-游戏表面 图集节点) 扩展后表面)
      (set-像素指针! (get-游戏表面 图集节点) (make-ftype-pointer unsigned-32 (ftype-ref SDL_Surface (pixels) (get-表面 (get-游戏表面 图集节点))))))
    ))

(define (向右生长 图集btree 新增宽度)
  (cond ((null? 图集btree) #f)
	((二叉树叶子? 图集btree) (set-节点w! (car 图集btree) (+ (get-节点w (car 图集btree)) 新增宽度)))
	(else
	 (向右生长 (get-left 图集btree) 新增宽度)
	 (向右生长 (get-right 图集btree) 新增宽度)
	 )))

(define (向下生长 图集btree 新增高度)
  (cond ((null? 图集btree) #f)
	((二叉树叶子? 图集btree) (set-节点h! (car 图集btree) (+ (get-节点w (car 图集btree)) 新增高度)))
	(else
	 (向下生长 (get-right 图集btree) 新增高度)
	 )))

(define (精灵图用旋转blit 源游戏表面 目标游戏表面 目标x 目标y)
  (let ((源表面w (get-w 源游戏表面))
	(目标表面w (get-w 目标游戏表面))
	(初始y (- (get-h 源游戏表面) 1)))
    (let loop ((目标坐标dx 0)
	       (目标坐标dy 0)
	       (dx 0)
	       (dy 初始y))
      (cond ((and (>= dx 源表面w) (= dy 0)) #t)
	    ((= dy 0)			;换行
	     (set-像素! 目标游戏表面 (+ 目标x 目标坐标dx (* (+ 目标y 目标坐标dy) 目标表面w)) (get-像素 源游戏表面 (+ dx (* dy 源表面w))))
	     (loop 0
		   (+ 目标坐标dy 1)
		   (+ dx 1)
		   初始y)
	     )
	    (else
	     (set-像素! 目标游戏表面 (+ 目标x 目标坐标dx (* (+ 目标y 目标坐标dy) 目标表面w)) (get-像素 源游戏表面 (+ dx (* dy 源表面w))))
	     (loop (+ 目标坐标dx 1)
		   目标坐标dy
		   dx
		   (- dy 1)
		   )
	     )
	    )
      )))

(define (遍历带名字表面列表扩展二叉树 btree objls)
  (cond ((null? objls) btree)
	(else
	 (if (新增叶子! btree (car objls) 图集扩展更新二叉树)
	     (遍历带名字表面列表扩展二叉树 btree (cdr objls))
	     (begin
	       (扩展图集! btree (car objls))
	       ;; (set-节点w 当前obj )
	       (测试输出 "扩展图集")
	       ;; 根据扩展方向,修改所有叶子节点,向右扩展给所有节点增加宽度,向下扩展只给最下面的节点增加高度 2024年9月4日13:12:00
	       (遍历带名字表面列表扩展二叉树 btree objls)) ;仅做了扩展,没将当前表面blit进去,而且没分割节点 2024年8月31日21:51:26
	   )
	 )))

(define 图片文件pt "\\.(jpg|jpeg|png|bmp|pnm|xpm|lbm|pcx|tga)$")

(define (遍历路径分支处理文件 path foo filels处理过程 全路径归并过程)
  (cond ((file-directory? path)
	 (let* ((pathls (map (lambda (pathstr)
			       (string-append path "/" pathstr)) (directory-list path)))
		(directoryls (filter file-directory? pathls))
		(filels (filter file-directory? pathls)))
	   ;; (测试输出 directoryls)
	   (全路径归并过程 (filels处理过程 filels) (apply append (map (lambda (pathstr)
								 (遍历路径分支处理文件 pathstr foo filels处理过程 全路径归并过程))
							       pathls))
			   )))
	((file-regular? path)
	 (foo path))
	(else
	 (error '指定路径下获取文件路径pathls "is not a file-directory" path))))

(define (pathls->具名表面 pathls)
  (map 创建名称图像 pathls))

(define (图集用排序 具名表面ls)
  (sort (lambda (a b)
	  (>= (max (get-w a) (get-h a))
	      (max (get-w b) (get-h b))))
	具名表面ls))

(define 间隔值 1)

(define (btree->节点ls btree)
  (cond ((null? btree) '())
	((二叉树叶子? btree) (if (空节点? (get-obj btree))
				 '()
				 (实例->属性-值als (get-obj btree) 图集扩展节点? '(游戏表面))))
	(else
	 (cons (实例->属性-值als (get-obj btree) 图集扩展节点? '(游戏表面))
	       (append
		(btree->节点ls (get-left btree))
		(btree->节点ls (get-right btree)))))))

(define (实例->属性-值als 实例 type 不转换的属性名ls)
  (let loop ((属性ls (type-properties type))
	     (acc '()))
    (cond ((null? 属性ls) acc)
	  ((memq (property-name (car 属性ls)) 不转换的属性名ls)
	   (loop (cdr 属性ls)
		 acc))
	  (else
	   (let ((当前属性 (car 属性ls)))
	     (loop (cdr 属性ls)
		   (cons (cons (property-name 当前属性) (get-property-value 当前属性 实例)) acc)))))))

(define (图集索引als保存 图集索引als path)
  (pp写入文件 (filter (lambda (pair)
			(not (equal? (car pair) '游戏表面)))
		      图集索引als)
	      path #t))

(define (读取图集索引als path)
  (call-with-input-file path 简单readfoo))

(define (als-ref key alist)
  (assert (cdr (assv key alist))))

(define (图集索引alol->图集索引ht ht alol)
  (cond ((null? alol) ht)
	(else
	 (let ((当前alist (car alol)))
	   (hashtable-set! ht (als-ref 'name 当前alist) (make-sdl-rect (als-ref '节点x 当前alist)
								       (als-ref '节点y 当前alist)
								       (als-ref '节点w 当前alist)
								       (als-ref '节点h 当前alist)
								       ))
	   (图集索引alol->图集索引ht ht (cdr alol))))))

(define 窗口宽 1280)
(define 窗口高 960)
(define 帧率 160)
(define NULL)

(define wintitle "像素变换")
(define 测试用表面path "./sugar.png")
(define 测试用表面path2 "./banana.png")

(define manager (manager-mk))
(mg-init manager SDL-INIT-EVERYTHING)
(mg-img-init manager IMG_INIT_EVERYTHING)

(define 图集宽 203)
(define 图集高 186)
(define 图集 (创建游戏表面 (sdl-create-rgb-surface 0 图集宽 图集高 32 0 0 0 #xff)))
(define 树 (构造二叉树叶子 (创建图集节点 图集 "图集" 0 0 图集宽 图集高)))

(define 图片元素路径 (遍历路径分支处理文件 "./图集元素" list (lambda (x) (filter (lambda (x) (pregexp-match 图片文件pt x)) x)) append ))

(测试输出 图片元素路径)

(遍历带名字表面列表扩展二叉树 树 (图集用排序
				  (pathls->具名表面
				   图片元素路径
				   )))

(图集索引als保存 (btree->节点ls 树) "图集索引.txt")

;; (遍历带名字表面列表扩展二叉树 树 (list (创建名称图像 测试用表面path) (创建名称图像 测试用表面path2)))

;; (测试输出 3)
(img-save-png (get-表面 图集) "图集.png")

(mg-quit-sdl manager)

;; (define game-像素变换 (创建game SDL-INIT-EVERYTHING IMG_INIT_EVERYTHING (bitwise-ior MIX_INIT_FLAC MIX_INIT_MP3 MIX_INIT_OGG)
;; 				(/ 1000 帧率) wintitle SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高))

;; (define (像素变换测试-core game)
;;   (let* ((bmp表面 (创建游戏表面 (sdl-create-rgb-surface 0 100 100 32 0 0 0 #xff)))
;; 	 (sugar表面 (创建游戏表面 (img-load 测试用表面path)))
;; 	 (读取的像素 '())
;; 	 )
;;     (测试输出 (cd))
;;     (测试输出 (get-表面 sugar表面))
;;     (lambda (m)
;;       (cond
;;        ;; ((sdl-event-quit?) '())
;;        ((sdl-event-key-down? SDLK-UP)
;; 	(let loop ((x 0)
;; 		   (acc '()))
;; 	  ;; 可能是像素格式的问题,修改后的效果同期望不同
;; 	  (cond ((>= x 8)
;; 		 (set! 读取的像素 acc))
;; 		(else
;; 		 (set-map-像素! bmp表面
;; 				(+  x 0) #xff #xff #xff #xff)
;; 		 (loop (1+ x)
;; 		       (cons (number->string (get-像素 bmp表面 x) 16) acc)))))
;; 	)
;;        ((sdl-event-key-down? SDLK-DOWN)
;; 	(display 读取的像素)
;; 	(newline)
;; 	(printf "~x~n" (get-format bmp表面))
;; 	)
;;        ((sdl-event-key-down? SDLK-RIGHT)
;; 	(精灵图用旋转blit sugar表面 bmp表面 20 20)
;; 	)
       
;;        (else
;; 	(sdl-blit-surface (get-表面 bmp表面) (make-sdl-rect 0 0 100 100) (game-surface-get game) (make-sdl-rect 0 0 窗口宽 窗口高))
;; 	(sdl-update-window-surface (game-window-get game))
;; 	))
;;       )    
;;     )
;;   )

;; (define core (像素变换测试-core game-像素变换))

;; (游戏循环 game-像素变换 core)
