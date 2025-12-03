;;; 复数
(define 单位虚数 0+i)

(define 坐标x real-part)
(define 坐标y imag-part)
;;; 位运算扩展
(define (bitwise-or a b)
  (bitwise-xor (bitwise-and a b) (bitwise-xor a b)))

;;; 渲染需要
(define (舍入取整 real)
  (exact (round real)))

(define (中点 值)
  (/ 值 2))

;;; 数学扩展 -- 贪吃蛇部分 
(define π 3.1415926575)
(define π/2 (/ π 2))			;下
(define -π (- π))			;
(define -π/2 (- π/2))			;上

(define (弧度->角度 弧度)
  ;; 渲染器构造直接调用了这个...  2023年5月11日22:48:09
  (* 180.0 (/ 弧度 π)))

(define (容零除 a b)
  (if (= b 0)
      (* a b +inf.0)
      (/ a b)))

(define (两点方向 始点坐标ls 终点坐标ls)
  ;; 这里有点绕,本来应该是计算位移,然后给坐标ls2加上位移,让它移动过去的
  ;; 但是因为移动的模值计算一步会麻烦,所以变成了把点1在计算出的方向上移动模的长度
  ;; 这个位移同点2到点1的位移是反向的
  (let* ((dx (- (坐标x 终点坐标ls) (坐标x 始点坐标ls)))
	 (方向偏移 (if (> dx 0) 0 π)))
    (+ 方向偏移 (atan (容零除
		       (- (坐标y 终点坐标ls) (坐标y 始点坐标ls))
		       dx)))
    ))

(define (极坐标->直角坐标 模 幅角)
  (map (lambda (系数)
	 (* 模 系数)) (list (cos 幅角) (sin 幅角))))

;;;随机数部分
(define (random-in negative positive randomfoo)
  (- (randomfoo positive) (randomfoo (- negative))))

(define (random-offset offset int randomfoo)
  (+ offset (randomfoo int)))

(define (random-complex c)	      	;实部和虚部都必须是正数...2024年4月16日12:40:13
  (+ (random (real-part c)) (* 单位虚数 (random (imag-part c)))))

;;;镜像
(define (x镜像 c)
  (- (real-part c) (* 单位虚数 (imag-part c))))

(define (y镜像 c)
  (- (* 单位虚数 (imag-part c)) (real-part c)))

(define (按轴水平镜像 c 轴)
  (+ (x镜像 (- c 轴)) 轴))

(define (按轴竖直镜像 c 轴)
  (+ (y镜像 (- c 轴)) 轴))

;;;旋转
(define (旋转 c 中心点 旋转c)
  (+ (* (- c 中心点) 旋转c) 中心点))
