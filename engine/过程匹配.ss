;;; 比如星组,背景,需要匹配到不同的更新过程和渲染过程
;;; 1.过程不能放进record,因为可能不会共享
;;; 2.按参数进行分派过程,SICP.CH2
(define %过程匹配env (make-parameter (make-eq-hashtable)))
;;; 同一个过程tag,对应于不同的数据,会匹配不同的proc

(define (匹配过程put proc . kls)
  (apply 多维ht-set! (%过程匹配env) proc kls))

(define (多维ht-set! 顶层ht putv . 多维k)
  (let loop ((多维kls 多维k)
	     (当前ht 顶层ht))
    (if (null? (cdr 多维kls))
	(hashtable-set! 当前ht (car 多维kls) putv)
	(let ((key (car 多维kls)))
	  (if (hashtable-contains? 当前ht key)
	      (loop (cdr 多维kls)
		    (hashtable-ref 当前ht key (quote (list key '失败))))
	      (make-多维ht putv 多维kls 当前ht)
	      )))))

(define (make-多维ht putv 多维kls 空ht)
  (letrec ((foo
	    (lambda (putv 多维kls acc)
	      (cond ((null? (cdr 多维kls))
		     (hashtable-set! acc (car 多维kls) putv))
		    (else
		     (hashtable-set! acc (car 多维kls) (make-eqv-hashtable))
		     (foo putv (cdr 多维kls)
			  (hashtable-ref acc (car 多维kls) #f)))))))
    (foo putv 多维kls 空ht)
    空ht
    ))

(define (多维ht-ref 多维ht 多维kls default)
  (cond ((null? (cdr 多维kls))
	 (hashtable-ref 多维ht (car 多维kls) default))
	(else
	 (多维ht-ref (hashtable-ref 多维ht (car 多维kls) #f)
		     (cdr 多维kls)
		     default))))

(define (匹配过程get op tags)
  (let ((row (hashtable-ref (%过程匹配env) op (make-eqv-hashtable))))
    (hashtable-ref row tags '匹配失败)))


;;; 这种形式在世界ht用名字和值做kv的时候,匹配过程和获取值都需要k....
;;; 另一种按tag分派,则是通过v存储的tag来分派.
;;; 还是得搞一个谓词->tag的系统,便于匹配过程,rtd可以直接得到标签
;;; 


