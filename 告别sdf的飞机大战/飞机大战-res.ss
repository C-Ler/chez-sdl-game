
;;; 配置 config
(define 测试量)
(define 窗口宽 1280)
(define 窗口高 960)
(define 帧率 160)
(define wintitle "飞机大战")
(define NULL)

(define 我方素材path "./res/image/hero1-r.png")
(define 我方飞机移动速度 5)
(define 我方飞机初始hp 3)

(define 我方子弹素材path "./res/image/bullet1.png")
(define 子弹速度 1)
(define 敌弹速度 0.105)

(define 敌机素材path "./res/image/enemy1-l.png")
(define 敌机速度 -0.5)
(define 敌机生成时间间隔 600)

(define 敌方子弹素材path "./res/image/enemy_bullet.png")
(define 敌机普攻冷却时间基数 1200.0)
(define 敌机普攻冷却时间区间 1200.0)

(define 敌机初始hp 1)
(define 敌弹初始hp 1)

(define 重置时间间隔 6000)

(define 背景path "./res/image/R-C.jpg")
(define 背景移动速度 -0.3)

(define 星星总数 500)
(define 星等数 3)
(define 星星速度 (+ 背景移动速度 -0.2))

(define 爆炸特效起始坐标偏移量最大值 32+32i)
(define 爆炸特效起始坐标偏移量最小值 (- 爆炸特效起始坐标偏移量最大值))
(define 爆炸特效速度最大值 0.01+0.01i)
(define 爆炸特效最大生命值 3)
(define 爆炸特效透明度 (exact (floor (/ 255 爆炸特效最大生命值))))
(define 爆炸特效色彩 (list (make-sdl-color 255 0 0 255)	;红色
			   (make-sdl-color 255 128 0 255) ;橙色
			   (make-sdl-color 255 255 0 255) ;黄色
			   (make-sdl-color 255 255 255 255)  ;白色
			   ))

(define 爆炸特效色彩数 (length 爆炸特效色彩))
(define 单次爆炸特效数下限 2)
(define 单次爆炸特效数随机区间 2)
(define 爆炸特效消散率 0.001)

(define 爆炸特效素材path "./res/image/enemy1_down3.png")

(define 碎片生命值 2)
(define 碎片速度最大值 0.1+0.1i)
(define 碎片速度最小值 (- 碎片速度最大值))
(define 碎片数量 4)
(define 碎片纹理分割数 '(2 2))
(define 碎片消散率 0.001)

(define 频率 44100)
(define 编码flag AUDIO_S16LSB)
(define 声道数量 2)
(define chunksize 2048)

(define 我方爆炸音效path "./res/sound/player-explosion.wav")
(define 敌方爆炸音效path "./res/sound/enemy-explosion.wav")
(define 我方发弹音效path "./res/sound/player-fire.wav")
(define 敌方发弹音效path "./res/sound/enemy-fire.wav")
(define bgmpath "./res/music/征战蓝天1.MP3")

(define 字体path "C:/Windows/Fonts/simfang.ttf")
(define 颜色位深 32)

(define 奖励点path "./res/image/point.png")
(define 奖励点速度最大值 2)
(define 奖励点速度y最小值 -2)
(define 奖励点生命初始值 (* 帧率 5))
(define 奖励点闪烁临界值 (* 帧率 2))

(define 得分榜条目数 8)
(define 窗口中轴线x (/ 窗口宽 2))
(define 得分榜记录坐标y初始值 750)
(define 得分榜记录坐标x (- (/ 窗口宽 2) 87))
(define 得分记录y坐标步进值 75)
(define 得分榜提示坐标x初始值 (- (/ 窗口宽 2) 45))
(define 得分榜提示坐标y初始值 800)

(define 按下开始str "按下enter玩游戏")
(define 飞机大战str "飞机大战")
(define chezstr "Chezscheme")
(define 得分榜str "得分榜")
(define 得分榜提示str "按下右侧Ctrl键继续")
(define 录入提示str "恭喜获得最高分,请输入你的昵称:")
(define 录入确认str "请敲回车键确认")
