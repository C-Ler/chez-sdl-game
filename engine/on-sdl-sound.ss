;;; 44100 AUDIO_S16LSB 2 2048

(define-record-type sdl-music
  (fields (mutable bgm) (mutable volume)))

(define (创建音乐 path)
  (make-sdl-music (mix-load-mus path) 100))

(define (播放/暂停 music)
  (cond ((= (mix-playing-music) 0)
	 (mix-play-music (sdl-music-bgm music) -1)) ;-1是无限循环播放,不然会播放指定次数
	((= (mix-paused-music) 1)
	 (mix-resume-music))
	(else
	 (mix-pause-music))))

;; (mix-volume-music 音乐音量) 直接用这个就好了...

(define (bgm音量设置! music 音量更迭foo 幅度)
  (sdl-music-volume-set! music (音量更迭foo (sdl-music-volume music) 幅度)))

(define (s音频-mk 音乐pathls 音效pathls 频率 编码flag 频道数量 chunksize 初始音量)
  (mix-open-audio 频率 编码flag 频道数量 chunksize)
  (let* (
	 (音乐音量 初始音量)
	 (music (mix-load-mus (car 音乐pathls)))
	 (sound (mix-load-wav (car 音效pathls))) ;单独分开运行库代码就可以加载这个函数,但是放到这里就提示sdl未提供
	 (播放位置 0)
	 (上一播放位置 播放位置)
	 (列表长度 (length 音乐pathls)))
    ;; (sdl-guardian music)
    ;; (sdl-guardian sound)
    (lambda (m)
      (cond
       ((equal? m 'bgm启动/暂停)
	(cond ((= (mix-playing-music) 0)
	       (mix-play-music music -1)) ;-1是无限循环播放,不然会播放指定次数
	      ((= (mix-paused-music) 1)
	       (mix-resume-music))
	      (else
	       (mix-pause-music)))
	)
       ((and (equal? m 'bgm音量增加)
	     (<= 音乐音量 128))
	(set! 音乐音量 (+ 1 音乐音量)))
       ((and (equal? m 'bgm音量降低)
	     (>=  音乐音量 0))
	(set! 音乐音量 (-  音乐音量 1)))
       ((equal? m 'bgm下一首)
	(set! 播放位置 (mod (+ 播放位置 1) 列表长度))
	)
       ((equal? m 'bgm上一首)
	(set! 播放位置 (mod (- 播放位置 1) 列表长度))
	)
       ((equal? m '音效播放)
	(mix-play-channel-timed -1 sound 0 -1))
       ((sdl-event-quit?)
	;; 这个直接通过事件管理不知道有没有效果,确实提供了新的思路.
	;; (mix-free-music music)
	;; (mix-free-chunk sound)
	'()
	)
       (else
	(assertion-violation '音频管理 "接受了不支持的参数!" m)))
      (mix-volume-music 音乐音量)
      (cond ((not (= 上一播放位置 播放位置))
	     (mix-halt-music)
	     ;; (mix-free-music music)
	     (set! music (mix-load-mus (list-ref 音乐pathls 播放位置)))
	     (mix-play-music music -1)
	     (set! 上一播放位置 播放位置)
	     )
	    (else '()))
      )
    )
  )


(define (音频-bgm启停 音频)
  (音频 'bgm启动/暂停))
(define (音频-bgm音量增加 音频)
  (音频 'bgm音量增加))
(define (音频-bgm音量降低 音频)
  (音频 'bgm音量降低))
(define (音频-bgm下一首 音频)
  (音频 'bgm下一首))
(define (音频-音效播放 音频)
  (音频 '音效播放))



