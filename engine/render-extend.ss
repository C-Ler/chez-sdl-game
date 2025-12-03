(define (sdl-render-fill-circle renderer cx cy radius)
  (let loop ([x radius]
             [y 0]
             [err 0])
    (when (>= x y)
      ;; 绘制水平扫描线
      (sdl-render-draw-line renderer (- cx x) (+ cy y) (+ cx x) (+ cy y))
      (sdl-render-draw-line renderer (- cx y) (+ cy x) (+ cx y) (+ cy x))
      (sdl-render-draw-line renderer (- cx x) (- cy y) (+ cx x) (- cy y))
      (sdl-render-draw-line renderer (- cx y) (- cy x) (+ cx y) (- cy x))
      (if (<= err 0)
          (loop x 
                (+ y 1) 
                (+ err (* 2 y) 1))
          (loop (- x 1)
                y
                (- err (* 2 x) 1))))))
