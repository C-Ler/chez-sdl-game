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
