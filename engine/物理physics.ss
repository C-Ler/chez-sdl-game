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


