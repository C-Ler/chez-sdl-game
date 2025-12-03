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
