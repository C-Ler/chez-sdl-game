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

(define (构造更新客体的谓词 客体objls 移除谓词-actls构造过程 新增谓词-actls构造过程 谓词-act映射foo 谓词-actls构造过程)
  ;; 基于两组对象碰撞检测时的抽象,用于既要更新a组对象也要更新b组对象的情形 2024年9月7日11:50:17
  (lambda (其他obj)
    ;; 会导致谓词求值时候调用构造过程,增加循环中的开销... 2024年9月8日10:21:51
    (obj更新 客体objls (移除谓词-actls构造过程 其他obj) (新增谓词-actls构造过程 其他obj) 谓词-act映射foo (谓词-actls构造过程 其他obj))))

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
