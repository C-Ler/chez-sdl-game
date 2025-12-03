(library-directories 
 '("D:\\lib" "D:\\lib\\thunderchez-trunk"
   "D:\\lib" "D:\\lib\\scheme-lib\\packages"
   ))

(import
 (chez-sdl lib sdl)
 (srfi s1 lists)
 ;; (srif s3 list-set)
 (srfi s13 strings)
 (srfi s14 char-sets)			;似乎不支持u8
 (srfi s113 sets)			;需要s128,但是依然有问题 set过程配合comparator结果不行 2023年9月3日15:39:07
 (srfi s128 comparators)
 (srfi s27 random-bits)
 (base-ex simple-ex)			;提供了测试输出
 (pregexp pregexp)

 (srfi s158 generators-and-accumulators) 
 					;Generators and Accumulators ,141得惰性也能实现,这个的本质是轻量级按需求值
 )

;; (include "D://code//aim4//game-uk//engine//sdf-adven-core.ss")
(include "D://code//aim4//game-uk//engine//数学math.ss")
;;; physics
(include "D://code//aim4//game-uk//engine//物理physics.ss") ;由于sdf的属性定义没法include,只能load了.... 2025年11月7日22:33:56

(include "D://code//aim4//game-uk//engine//on-sdl.ss")
(include "D://code//aim4//game-uk//engine//on-sdl-time.ss")
(include "D://code//aim4//game-uk//engine//loop.ss")

(include "D://code//aim4//game-uk//engine//文本渲染-sdf-ttf.ss")

(include "D://code//aim4//game-uk//engine//on-sdl-sound.ss")

(include "D://code//aim4//game-uk//engine//过程匹配.ss")

(include "D://code//aim4//game-uk//engine//game-object.ss")

;;;
;; (define (纹理信息-w 纹理信息)
;;   (cond ((null? 纹理信息)
;; 	 (assertion-violation '纹理信息-w "纹理信息-w获取失败!" 纹理信息))
;; 	(else
;; 	 (caddr 纹理信息))))

;; (define (纹理信息-h 纹理信息)
;;   (cond ((null? 纹理信息)
;; 	 (assertion-violation '纹理信息-h "纹理信息-h获取失败!" 纹理信息))
;; 	(else
;; 	 (cadddr 纹理信息))
;; 	))

;; (define-sdf-property gobj:纹理
;;   纹理
;;   'predicate any-object?		;因为目前没有实现判断C的指针是某个类型的方法,这里需要一个对任何对象都返回#t的谓词
;;   'default (lambda () '无效的情况))

;; (define-type 纹理 () (gobj:纹理))

;; (define (s纹理-mk render path)
;;   (make-纹理 '纹理
;; 	     (let ((纹理 (img-load-texture render path)))
;; 	        (when (ftype-pointer-null? 纹理)
;; 		  (printf "纹理加载失败: ~a~n" (sdl-get-error))
;; 		  (sdl-clear-error!)
;; 		  )
;; 		纹理
;; 		)
;; 	     ))

;; (define s纹理? 纹理?)			;前向兼容 2025年11月5日10:45:59

;; (define (s纹理-加载图片! 纹理 render path)
;;   (set-纹理! 纹理 (img-load-texture render path)))

;; (define (s纹理-纹理信息get 纹理)
;;   (sdl-query-texture (get-纹理 纹理)
;; 		     ))

;; (定义匹配度优先广义过程 s纹理-按纹理规格blit  4 (constant-generic-procedure-handler #f))
;; (广义过程扩展 s纹理-按纹理规格blit ((any-object? render) (纹理? 纹理) (integer? x坐标) (integer? y坐标))
;; 	      (let ((纹理信息 (s纹理-纹理信息get 纹理)))
;; 		(sdl-render-copy render (get-纹理 纹理) NULL (make-sdl-rect x坐标 y坐标 (纹理信息-w 纹理信息) (纹理信息-h 纹理信息)))))	;用下面这个替换这个的内容 2024年4月24日19:58:31

;; (定义匹配度优先广义过程 框选纹理blit 4 (constant-generic-procedure-handler #f))
;; (广义过程扩展 框选纹理blit ((any-object? render) (坐标对象? obj) (纹理? 纹理) (sdl-rect? 取用框))
;; 	      (sdl-render-copy render (get-纹理 纹理) 取用框
;; 			       (make-sdl-rect (get-x obj) (get-y obj) (sdl-rect-w 取用框) (sdl-rect-h 取用框))))

;; ;;; 
;; (define 表面显示
;;   ;; 应该传个窗口,从窗口得到窗口表面,而不是把下一道和前一道都扔进来
;;   (case-lambda
;;     [(图像s表面 窗口s表面 窗口)
;;      (表面显示 图像s表面 0 0 窗口s表面 0 0 窗口)]
;;      [(图像s表面 图像遮罩x 图像遮罩y 窗口s表面 显示x 显示y 窗口)
;;      (let ((矩形a (make-sdl-rect 图像遮罩x 图像遮罩y (s表面-w 图像s表面) (s表面-h 图像s表面)))
;; 	   (矩形b (make-sdl-rect 显示x 显示y (s表面-w 窗口s表面) (s表面-h 窗口s表面))))
;;        ;; 下面这句由于像素修改的需要,窗口表面不使用转码前的初始表面的话,会什么都不显示,原因不明  2023年5月7日23:26:28
;;        (表面显示 (s表面-表面 图像s表面) 矩形a (s表面-init表面 窗口s表面) 矩形b 窗口)
;;        )]
;;     [(图像表面 矩形a 窗口表面 矩形b 窗口)
;;      (sdl-blit-surface 图像表面 矩形a 窗口表面 矩形b)
;;      (sdl-update-window-surface 窗口)]
;;     )
;;   )

;; (define (像素值numls->str ls)
;;   (map (lambda (num)
;; 	 (number->string num 16))
;;        ls))

;;; atlas
(include "D://code//aim4//game-uk//engine//图集atlas.ss")

;;;更新方法
(include "D://code//aim4//game-uk//engine//更新方法.ss")

;;; 渲染扩展
(include "D://code//aim4//game-uk//engine//render-extend.ss")
