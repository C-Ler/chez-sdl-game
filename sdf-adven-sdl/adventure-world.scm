#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; An adventure game at MIT

#| load 本文件 (start-adventure '小明) (go 'west) (go 'east) 
course-6-frosh picks up course-6-froshbag
Exception in error:wrong-type-argument: wrong argument type, expected #[#{simple-tag lc8mwm3c886i9cm630dumuiew-62} #[#{<tag-shared> lc8mwm3c886i9cm630dumuiew-63} list #<procedure list?> #<procedure constructor at tagging.scm:2680> #<procedure at tagging.scm:2989> #<procedure at collections.scm:1254>]]and caller value: with irritants (#<void> #<void>)

error:wrong-type-argument 被封装成其它error过程时,第二个参数传入了(predicate-description predicate) ,所以出现上面这种情况十分反常 原因不明...  2024年1月29日21:13:11
应该是predicate-description 返回的,但是不知道是传入了哪个参数变成这样的  2024年1月29日21:15:45
应该是传入了gp-list?,没发现哪里调用了不带n:的list?....  2024年1月29日21:19:45
|#

#|
substrate set-up! error-generic-procedure-handler: Inapplicable generic procedure: with irritants (get-tag-shared (#[#{simple-tag ggb5i5qobi1g1l6270n43mnon-63} #[...]]))
|#

;; (include "./sdf-adven-sdl/adventure-substrate.scm")   	;其中的define-record-printer在utlis给出实现,这个过程的实现调用了standard-print-method,是mit的过程
;; (include "./sdf-adven-sdl/adventure-objects.scm")

#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#
(load "D://code//aim4//game-uk//l-engine-2.ss")

;;; 方便替换成其它语言的
(define 说str "说:")
(define 的str "的")
(define 进入str "进入了")
(define 嗨str "嗨,")
(define 痛苦嚎叫str "啊啊!")
(define 无法承受的打击str "下打击我受不了!")
(define 人物挂掉的旁白 '("一声撕心裂肺,透彻心扉的哀嚎回荡在这里..."))
(define 房管的训诫台词 '("怎么还不睡觉?!"
			 "所有人都回自己寝室!"))
(define 房管的自言自语 '("哼哼,等我抓到那些捣蛋鬼..."))
(define 房管的认怂台词 '("这次就先放过你们..."))

(define 回家str "回去了")

(define 巨魔肚子咕噜声 "肚子咕噜咕噜声")

(define 咬了str "咬了")
(define 一口str "一口")

(define 你处在str "你处在")
(define 你的包里有str "你的包里有:")
(define 你在这能看到str "你在这能看到:")
(define 你见到了str "你见到了:")
(define 你可以从这些地方离开str "你可以从这些方向离开:")
(define 无法离开提示 '("没有这个方向的出口..."
                       "你已经见马克思了!"))
(define 从str "从")
(define 离开str "离开")
(define 不可移动提示 "是不可移动的")
(define 正持有提示 "正持有")

(define-syntax 符号->定义为符号的文本
  (lambda (x)
    (syntax-case x ()
      [(_ (后缀) 符号 ...)
       (with-syntax ([(定义的符号 ...) (map (lambda (x)
					      (gen-id #'后缀 x #'后缀))
					    #'(符号 ...))]
		     [(字符串 ...) (map (lambda (x)
					  (datum->syntax #'后缀 (symbol->string (syntax->datum x))))
					#'(符号 ...))])
	 #'(begin (define 定义的符号 字符串)
		  ...))
       ]
      [(_ (前缀 后缀) 符号 ...)
       (with-syntax ([(定义的符号 ...) (map (lambda (x)
					      (gen-id #'后缀 #'前缀 x #'后缀))
					    #'(符号 ...))]
		     [(字符串 ...) (map (lambda (x)
					  (datum->syntax #'后缀 (symbol->string (syntax->datum x))))
					#'(符号 ...))])
	 #'(begin (define 定义的符号 字符串)
		  ...))
       ])))

(符号->定义为符号的文本 (提示- str) 给了 把 从 拿走了 并 我很失望 哇哦 你从哪搞到的? 捡起了 谢了 伙计! 扔掉了
			你干嘛! 已经 在 了 的 到 没有 出口 你 无法 强迫 移动 包 里 有 是 空)

(符号->定义为符号的文本 (str) 这 叫 让我很难受 伤害值) 

;;; 我受够了,可以定义个代码,直接把list中的符号转成string,然后构造一个文件
;;; Misc
;;; 冒险游戏需要用到的其它东西
(define (direction? object)
  (if (memv object known-directions) #t #f))

(register-predicate! direction? 'direction)

(define known-directions
  '(north south east west in out up down skew))

(define (display-to-string object)
  (call-with-output-string
    (lambda (port)
      (display object port))))

(define (random-choice items)
  (guarantee list? items)
  (and (pair? items)
       (list-ref items (random (length items)))))

(define (random-number n)
  (+ (random n) 1))

(define (bias? object)
  (and (real? object)
       (<= 0 object 1)))

(register-predicate! bias? 'bias)

(define (random-bias weight)
  (/ 1 (random-number weight)))

(define (flip-coin bias)
  (>= (random 1.0) bias))

;;; Base object type

(define-sdf-property object:name name)

(define-sdf-property object:description description
  'default-to-property object:name)	;这个十分特殊,用宏展开和别的不一样,不需要加'号

(define-type object () (object:name object:description))

(define (find-object-by-name name objects)
  (find (lambda (object)
          (eqv? name (get-name object)))
        objects))

(define-generic-procedure-handler tagged-data-representation ;这两个可能起到某种作用,比如pp 示例中的各种obj  2024年2月19日20:56:38
  ;; 扩展了tagging中的广义过程,返回一条继承链上的名称 2024年8月10日16:12:15
  (match-args object?)
  (lambda (super object)
    (append (super object)
            (list (get-name object)))))

(define-generic-procedure-handler tagged-data-description
  ;; 扩展了tagging中的广义过程,返回描述 2024年8月10日16:12:15
  (match-args object?)
  (lambda (object)
    (let ((instance-data (tagged-data-data object)))
      (map (lambda (property)
             (list (property-name property)
                   ((instance-data-binding property
                                           instance-data))))
           (instance-data-properties instance-data)))))

;;; Messaging

(define send-message!			;这个gp在 adventure-obj中,只扩展了对thing? place? avatar?的情况  2024年1月22日21:27:12
  (most-specific-generic-procedure 'send-message! 2
				   (lambda (msg obj)
				     (match-args message? object?)
				     (let ((scr (make-screen 'name 'the-screen))) ;原本只是个#f,接着报错了,怀疑是捡起东西后提示某物到哪过于简陋 2024年1月23日21:49:58
				       (lambda (message thing)
					 (display-message (type-properties thing))
					 (display-message message (get-port scr)))))))

(define (narrate! message person-or-place) ;第二个参数提供位置,只有这个位置的人才能听到公告
  (send-message! message
                 (if (person? person-or-place)
                     (get-location person-or-place)
                     person-or-place))
  (if debug-output
      (send-message! message debug-output)))

(define (tell! message person)
  (send-message! message person)
  (if debug-output
      (send-message! message debug-output)))

(define (say! person message)
  (narrate! (append (list person 说str) message)
            person))

(define (announce! message)
  (for-each (lambda (place)
              (send-message! message place))
            (get-all-places))
  (if debug-output
      (send-message! message debug-output)))


(define (display-item item port)
  (display (if (object? item) (get-name item) item) port))

(define (message? object)
  (list? object))

(register-predicate! message? 'message)
(register-predicate! output-port? 'output-port)

(定义匹配度优先广义过程 display-message 2 (constant-generic-procedure-handler #f))

(广义过程扩展 display-message ((message? message) (output-port? port))
	      (guarantee message? message 'display-message)
	      (if (pair? message)
		  (begin
		    (fresh-line port)
		    (display-item (car message) port)
		    (for-each (lambda (item)
				(display " " port)
				(display-item item port))
			      (cdr message)))))

(define (possessive person)
  (string-append (display-to-string (get-name person))
                 的str))

;;; Screen

(define 白色 (make-sdl-color 255 255 255 0))

(define-sdf-property gobj:color
  color
  'predicate sdl-color?
  'default-value 白色 ;这里引用了共享的部分 2024年9月20日16:40:39
  )

(define-type UI类型 ()
  (gobj:color)		;可以想办法让这个render的默认值变成game的render		
  )

(define-sdf-property gobj:rect
  rect
  'predicate sdl-rect?
  'default-value (make-sdl-rect 0 0 0 0)
  )

(define-type 含rectUI (UI类型?)
  (gobj:rect)
  )

(define (构造空游戏字符串)
  (创建字符串-ctcr ""))

(define-sdf-property UI:显示的文本
  显示的文本
  'predicate 游戏字符串?
  'default-supplier 构造空游戏字符串
  )

(define-type 含文本UI (含rectUI?)
  (UI:显示的文本)
  )

(定义匹配度优先广义过程 set-仅字符串! 2 (lambda (a b) (error 'set-仅字符串 "无法设置a的字符串为b" (a b))))

(广义过程扩展 set-仅字符串! ((含文本UI? 含文本UI实例) (string? str))
	      (set-字符串扩展字符-ctcr! (get-显示的文本 含文本UI实例) str))

(广义过程扩展 set-仅字符串! ((含文本UI? 含文本UI实例) (symbol? str))
	      (set-仅字符串! 含文本UI实例 (symbol->string str))
	      )

(define-type 文本-矩形UI (含文本UI?) ()) ;重新求值之后才能将这个变成UI类型的子类,但是get-color确能用....

(define-sdf-property UI:log
  log
  'predicate list?
  'default-value '()
  )

(define-sdf-property UI:消息板最后一行在log的位置
  消息板最后一行在log的位置
  'predicate integer?
  'default-value 0
  )

(define-sdf-property UI:消息板每页字符串数
  消息板每页字符串数
  'predicate integer?
  'default-value 27
  )

(define-type 消息板 (含文本UI?) (UI:log UI:消息板最后一行在log的位置 UI:消息板每页字符串数))

(define output-port-or-消息板? (disjoin output-port? 消息板?))

(define (item->msgstr item)
  ;; string-append 
  (cond	((string? item) item)
	((object? item)
	 (symbol->string (get-name item)))
	((symbol? item)
	 (symbol->string item))
	((number? item) (number->string item))
	((含文本UI? item) (get-显示的文本 item))
	(else (error 'item->msgstr "is not a message" item)))
  ;; ","
  )

(define (消息板字符串更新! 消息板)
  (let ((字符串数 (get-消息板最后一行在log的位置 消息板)))
    (set-字符串扩展字符-ctcr! (get-显示的文本 消息板) (apply string-append
							     (切片 (get-log 消息板) (max 0 (- 字符串数 (get-消息板每页字符串数 消息板)))
								   字符串数)))))

(广义过程扩展 display-message ((message? message) (消息板? 消息板))
	      (set-log! 消息板 (append (get-log 消息板) (list (string-append "
"
							       (item->msgstr (car message))
							       (apply string-append (map (lambda (item)
											   (item->msgstr item))
											 (cdr message))
								      )))))
	      (set-消息板最后一行在log的位置! 消息板 (length (get-log 消息板)))
	      (消息板字符串更新! 消息板))
;;; 原screen
(define-sdf-property screenport port 'predicate output-port-or-消息板? ;; output-port?,原来的只能支持 current-output-port
  'default-supplier current-output-port)	;在需要显示的情况下,设置为消息板

(define-type screen (object?) (screenport))

(define 主要screen (make-screen 'name '主要screen))

(define-generic-procedure-handler send-message!
  (match-args message? screen?)
  (lambda (message screen)
    (display-message message (get-port screen))))

;;; Clock
(define (make-clock)			;封装了clock record的make
  (%make-clock 0 '()))

(define-record-type (<clock> %make-clock clock?)
  (fields (mutable current-time current-time set-current-time!)	;时间和thing两个槽子 2024年7月26日23:27:16
	  (mutable things clock-things set-clock-things!)))

(define (register-with-clock! thing clock) ;下面这两个函数调用了clock-things 传入的clock参数为<void>  2024年1月11日19:51:59 只被objct 的set-up!调用 2024年1月11日20:47:13
  (guard (x [(error? x) (显示thing) (搞个新的clock)])
    (set-clock-things! clock
		       (lset-adjoin eqv?
                                    (clock-things clock)
                                    thing))))

(define (unregister-with-clock! thing clock)
  (set-clock-things! clock
                     (delv thing (clock-things clock))))

(define (tick! clock)
  ;; 获取所有需要更新的对象,遍历更新,上面两个reg和unreg负责将thing加入更新列表或者移出 2024年8月10日10:34:25
  ;; 这一套方式没考虑时间间隔 2024年8月10日10:35:36
  (set-current-time! clock (+ (current-time clock) 1)) ;全局时钟更新一帧 2024年7月26日23:28:56
  (for-each clock-tick! ;同时给每个随时间改变的对象clock-tick! 随时钟更新状态 2024年7月26日23:29:37
            (clock-things clock)))

(定义链式广义过程 clock-tick! 1 (constant-generic-procedure-handler #f))

(define (define-clock-handler type action) ;如果一个type有多个action该怎么办? 2024年5月11日19:58:16
  ;; 这个action是个过程,被封装到handler里面,多个action完全可以合并成一个 2024年7月26日23:39:06
  (define-generic-procedure-handler clock-tick!
    (match-args type)
    (lambda (super object)
      (super object)
      (action object))))

;;; Object types for Adventure game
(define-sdf-property thing:location			;property
  location
  'predicate (lambda (x) (container? x)))

(define-type thing (object?)				;type
  ;; thing的属性是作为container的location,紧跟着定义container类型,唯一的属性是is-list-of thing?,
  (thing:location))

(define-generic-procedure-handler set-up! (match-args thing?)
  (lambda (super thing)
    (super thing)
    (add-thing! (get-location thing) thing)))

(define-generic-procedure-handler tear-down! (match-args thing?)
  (lambda (super thing)
    (remove-thing! (get-location thing) thing)
    (super thing)))

(define-generic-procedure-handler send-message!
  (match-args message? thing?)
  (lambda (message thing)
    (display (type-properties thing))
    (display-message message (get-port 主要screen))))

;;; Containers

(define-sdf-property container:things
  (things
  adder remover)
  'predicate (is-list-of thing?)
  'default-value '())

(define-type container (object?) (container:things))

(define adder-modifier (lambda (value values)
			 (lset-adjoin eqv? values value)))

(define remover-modifier (lambda (value values)
			   (delv value values)))

(property-modifier-extend container:things container? thing? things-adder adder-modifier)
(property-modifier-extend container:things container? thing? things-remover remover-modifier)

(define add-thing! things-adder)

(define remove-thing! things-remover)

;;; Exits

(define-sdf-property exit:from
  from
  'predicate (lambda (x) (place? x)))

(define-sdf-property exit:to
  to
  'predicate (lambda (x) (place? x)))

(define-sdf-property exit:direction
  direction
  'predicate direction?)

(define-type exit (object?) (exit:from exit:to exit:direction))

(define-generic-procedure-handler set-up! (match-args exit?)
  (lambda (super exit)
    (super exit)
    (add-exit! (get-from exit) exit)))

;;; Places

(define-sdf-property place:vistas
  (vistas adder)
  'predicate (lambda (x)
               (and (list? x) (every place? x)))
  'default-value '())

(define-sdf-property place:exits
  (exits adder)
  'predicate (lambda (x)
               (and (list? x) (every place? x)))
  'default-value '())

(define-type place (container?) (place:vistas place:exits))

(property-modifier-extend place:vistas place? place? vistas-adder adder-modifier)
(property-modifier-extend place:exits place? exit? exits-adder adder-modifier)

(define add-vista! vistas-adder)

(define add-exit! exits-adder)

(define (find-exit-in-direction direction place)
  (find (lambda (exit)
          (eqv? (get-direction exit) direction))
        (get-exits place)))

(define (people-in-place place)
  (filter person? (get-things place)))

(define (things-in-place place)
  (remove person? (get-things place)))

(define (all-things-in-place place)
  (append (things-in-place place)
          (append-map get-things (people-in-place place))))

(define (takeable-things place)
  ;; 某个场所的可移动物品以及场所中所有人身上的物品?
  (append (filter mobile-thing? (things-in-place place))
          (append-map get-things (people-in-place place))))

(define-generic-procedure-handler send-message!
  (match-args message? place?)
  (lambda (message place)
    (for-each (lambda (person)
                (send-message! message person))
              (people-in-place place))))

;;; Mobile things

(define-sdf-property mobile-thing:origin
  ;; 这个属性在定义类型时默认值就是location这个属性的值
  origin
  'predicate place?
  'default-to-property thing:location)

(define-type mobile-thing (thing?) (mobile-thing:origin))

(定义链式广义过程 enter-place! 1 (constant-generic-procedure-handler #f))

(define leave-place!
  (most-specific-generic-procedure 'leave-place! 1
    (constant-generic-procedure-handler #f)))

;;; People
(define-sdf-property persohealth
  health
  'predicate exact-integer?
  'default-value 3)

(define-sdf-property persobag
  ;; 默认值是(lambda () (make-bag 'name 'bag)的返回值,这个参数传入时不会被求值... 2024年8月11日17:34:20
  bag
  'predicate bag?
  'default-supplier
  (lambda () (make-bag 'name ;; (get-name obj)
		       'bag		;如果要实现1月的期望,得修改<property>record,增加关键字,同时还要调整其他机制. 2024年10月14日20:30:54
		       )))	;所有人物的bag都特么叫my-bag,得想个办法,改成xx的bag这样  2024年1月28日18:17:47

;;; 扩展后的person,支持了普通攻击的互动,人物速度+人物朝向+冷却时间 2024年11月1日21:02:15
;;; 对应也需要能获取时间间隔
(define-sdf-property 人物速度
  速度
  'predicate complex?
  'default-value 0)

(define-sdf-property 人物朝向
  朝向
  'predicate complex?
  'default-value 0)

(define-sdf-property 人物普攻冷却时间
  普攻冷却时间
  'predicate real?
  'default-value 500)

(define-sdf-property 人物当前普攻冷却时间
  当前普攻冷却时间
  'predicate real?
  'default-value 0)

(define-type person (mobile-thing?) (persohealth persobag 人物速度 人物朝向 人物普攻冷却时间 人物当前普攻冷却时间))

(define-generic-procedure-handler set-up! (match-args person?)
  (lambda (super person)
    (super person)
    (set-holder! (get-bag person) person)))

(define-generic-procedure-handler get-things (match-args person?)
  (lambda (person)			
    (get-things (get-bag person))	;这个表达式的定义没找到,没见到定义只见到扩展 2024年10月23日15:35:15 其实是容器带的....
    ))

(广义过程扩展 enter-place! super ((person? person))
	      (super person)
	      (narrate! (list person 进入str (get-location person))
			person)
	      (let ((people (people-here person)))
		(if (pair? people)
		    (say! person (cons 嗨str people)))))

(define-generic-procedure-handler send-message!	;缺失了这个,导致非玩家角色行动后的各种旁白,tell!都会匹配到默认gp-handler,接着抛异常 2024年1月22日21:44:05
  (match-args message? person?)
  (lambda (message person)
    (display-message message (get-port 主要screen))))

(define (when-alive callback)
  ;; 定义了但是没被引用....
  (lambda (person)
    (if (> (get-health person) 0)
        (callback person))))

(define (people-here person)
  (delv person (people-in-place (get-location person))))

(define (things-here person)
  (things-in-place (get-location person)))

(define (vistas-here person)
  (get-vistas (get-location person)))

(define (exits-here person)
  (get-exits (get-location person)))

(define (peoples-things person)
  (append-map get-things (people-here person)))

(define (suffer! hits person)
  (guarantee positive? hits)
  (say! person (list 痛苦嚎叫str hits 无法承受的打击str))
  (set-health! person (- (get-health person) hits))
  (if (< (get-health person) 1)
      (die! person)))

(define (die! person)
  (for-each (lambda (thing)
              (drop-thing! thing person))
            (get-things person))
  (announce!
   人物挂掉的旁白)
  (set-health! person 0)
  (move! person (get-heaven) person))

(define (resurrect! person health)
  (guarantee positive? health)
  (set-health! person health)
  (move! person (get-origin person) person))

;;; Bags

(define-sdf-property bag:holder
  holder
  'predicate
  (lambda (x) (or (not x) (person? x)))
  'default-value #f)

(define-type bag (container?)  (bag:holder))

;;; Autonomous people (non-player characters)

(define-sdf-property autonomous-agent:restlessness
  restlessness
  'predicate bias?)

(define-sdf-property autonomous-agent:acquisitiveness
  acquisitiveness
  'predicate bias?)

(define-type autonomous-agent (person?) (autonomous-agent:restlessness
					 autonomous-agent:acquisitiveness))

(define-generic-procedure-handler set-up!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (super agent)
    (register-with-clock! agent (get-clock))))

(define-generic-procedure-handler tear-down!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (unregister-with-clock! agent (get-clock))
    (super agent)))

(define (move-and-take-stuff! agent)
  (if (flip-coin (get-restlessness agent))
      (move-somewhere! agent))
  (if (flip-coin (get-acquisitiveness agent))
      (take-something! agent)))

(define (move-somewhere! agent)
  (let ((exit (random-choice (exits-here agent))))
    (if exit
        (take-exit! exit agent))))

(define (take-something! agent)
  (let ((thing
         (random-choice (append (things-here agent)
                                (peoples-things agent)))))
    (if thing
        (take-thing! thing agent))))

(define-clock-handler autonomous-agent? move-and-take-stuff!)

;;; Students

(define-type student (autonomous-agent?) ())
;;; House masters

(define-sdf-property house-master:irritability
  irritability
  'predicate bias?)

(define-type house-master (autonomous-agent?)
  (house-master:irritability))

(define (irritate-students! master)
  (let ((students (filter student? (people-here master))))
    (if (flip-coin (get-irritability master))
        (if (pair? students)
            (begin
              (say! master
                    房管的训诫台词)
              (for-each (lambda (student)
                          (narrate! (list student 回家str
                                          (get-origin student))
                                    student)
                          (move! student
                                 (get-origin student)
                                 student))
                        students))
            (say! master
                  房管的自言自语))
        (if (pair? students)
            (say! master
                  房管的认怂台词)))))

(define-clock-handler house-master? irritate-students!)

;;; Trolls

(define-sdf-property troll:hunger
  hunger
  'predicate bias?)

(define-type troll (autonomous-agent?) (troll:hunger))

(define (eat-people! troll)
  (if (flip-coin (get-hunger troll))
      (let ((people (people-here troll)))
        (if (null? people)
            (narrate! (list (possessive troll) 巨魔肚子咕噜声)
                      troll)
            (let ((victim (random-choice people)))
              (narrate! (list troll 咬了str victim 一口str)
                        troll)
              (suffer! (random-number 3) victim))))))

(define-clock-handler troll? eat-people!)

;;; Avatars

(define-sdf-property avatar:screen
  screen
  'predicate screen?)

(define-type avatar (person?) (avatar:screen))

(define-generic-procedure-handler send-message!
  (match-args message? avatar?)
  (lambda (message avatar)
    (send-message! message (get-screen avatar))))

(广义过程扩展 enter-place! super ((avatar? avatar))
	       (super avatar)
	       (look-around avatar)
	       (tick! (get-clock))
	       )

(define (look-around avatar)
  (tell! (list 你处在str (get-location avatar))
         avatar)
  (let ((my-things (get-things avatar)))
    (if (pair? my-things)
        (tell! (cons 你的包里有str my-things)
               avatar)))
  (let ((things
         (append (things-here avatar)
                 (people-here avatar))))
    (if (pair? things)
        (tell! (cons 你在这能看到str things)
               avatar)))
  (let ((vistas (vistas-here avatar)))
    (if (pair? vistas)
        (tell! (cons 你见到了str vistas)
               avatar)))
  (tell! (let ((exits (exits-here avatar)))
           (if (pair? exits)
               (cons 你可以从这些地方离开str
                     (map get-direction exits))
               无法离开提示))
         avatar))

;;; Motion
(define (take-thing! thing person)
  (move! thing (get-bag person) person))

(define (drop-thing! thing person)
  (move! thing (get-location person) person))

(define (take-exit! exit mobile-thing)
  (generic-move! mobile-thing
                 (get-from exit)
                 (get-to exit)
                 mobile-thing))

(define (move! thing destination actor)
  (generic-move! thing
                 (get-location thing)
                 destination
                 actor))

(define generic-move!
  (most-specific-generic-procedure 'generic-move! 4 (lambda (obj container container2 hold)
						      (display obj))))

;;; TODO: guarantee that THING is in FROM.
;;; Also that the people involved are local.

;; coderef: generic-move:default
(define-generic-procedure-handler generic-move!
  (match-args thing? container? container? person?) ;将移动用于不可移动物体时  2024年2月19日21:39:24
  (lambda (thing from to actor)
    (tell! (list thing 不可移动提示)
           actor)))

;; coderef: generic-move:steal
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from))
          (new-holder (get-holder to)))
      (cond ((eqv? from to)
             (tell! (list new-holder 正持有提示
                          mobile-thing)
                    actor))
            ((eqv? actor former-holder)
             (narrate! (list actor
                             提示-把str mobile-thing
                             提示-给了str new-holder)
                       actor))
            ((eqv? actor new-holder)
             (narrate! (list actor
			     提示-从str former-holder
                             提示-拿走了str mobile-thing
                             )
                       actor))
            (else
             (narrate! (list actor
			     提示-从str former-holder
                             提示-拿走了str mobile-thing
                             提示-并str 提示-给了str new-holder)
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder (list 提示-我很失望str)))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list 提示-哇喔str 提示-你从哪搞到的?str)))
      (if (not (eqv? from to))
          (move-internal! mobile-thing from to)))))

(define non-person-mobile-thing?
  (conjoin mobile-thing? (complement person?)))

(set-predicate<=! non-person-mobile-thing? mobile-thing?)

;; coderef: generic-move:take  有问题,导致person也可以被take....
(define-generic-procedure-handler generic-move!
  (match-args non-person-mobile-thing? place? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((new-holder (get-holder to)))
      (cond ((eqv? actor new-holder)
             (narrate! (list actor
                             提示-捡起了str mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             提示-捡起了str mobile-thing
                             提示-并str 提示-给了str new-holder)
                       actor)))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list 提示-哇哦str 提示-谢了str 提示-伙计!str)))
      (move-internal! mobile-thing from to))))

;; coderef: generic-move:drop
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? place? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
             (narrate! (list actor
                             提示-扔掉了str mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             提示-从str former-holder
                             提示-拿走了str mobile-thing
                             提示-并str 提示-扔掉了str)
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder
                (list 提示-你干嘛str! )))
      (move-internal! mobile-thing from to))))

(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? place? person?)
  (lambda (mobile-thing from to actor)
    (cond ((eqv? from to)
           (tell! (list mobile-thing 提示-已经str 提示-在str from 提示-了str)
                  actor))
          (else
           (tell! (list "How do you propose to move"
                        mobile-thing
                        "without carrying it?")
                  actor)))))

;; coderef: generic-move:person
(define-generic-procedure-handler generic-move!
  (match-args person? place? place? person?)
  (lambda (person from to actor)
    (let ((exit (find-exit from to)))
      (cond ((or (eqv? from (get-heaven))
                 (eqv? to (get-heaven)))
             (move-internal! person from to))
            ((not exit)
             (tell! (list 提示-没有str 提示-从str from
                          提示-到str to 提示-的str 提示-出口str)
                    actor))
            ((eqv? person actor)
             (narrate! (list person 提示-从str
                             (get-direction exit) 离开str)
                       from)
             (move-internal! person from to))
            (else
             (tell! (list 提示-你str 提示-无法str 提示-强迫str
                          person
                          提示-移动str)
                    actor))))))

(define (find-exit from to)
  (find (lambda (exit)
          (and (eqv? (get-from exit) from)
               (eqv? (get-to exit) to)))
        (get-exits from)))

(define (move-internal! mobile-thing from to)
  (leave-place! mobile-thing)		;定义了但是没扩展过的过程
  (remove-thing! from mobile-thing)
  (set-location! mobile-thing to)
  (add-thing! to mobile-thing)
  (enter-place! mobile-thing)		;只对person?和avatar?扩展了,主要是进来之后讲台词,tick-tock
  )


;;; 原world部分

(define the-clock)
(define all-places)
(define heaven)
(define all-people)
(define my-avatar)

(define (start-adventure my-name)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (random-choice all-places)
		       主要screen))
  (whats-here))

(define (get-all-places)
  all-places)

(define (get-heaven)
  heaven)

(define (get-clock)
  the-clock)

;;; User interface
(define (done)
  (printf "~%done~%"))

(define (go direction)
  (let ((exit
         (find-exit-in-direction direction
                                 (get-location my-avatar))))
    (if exit
        (take-exit! exit my-avatar)
        (narrate! (list 提示-没有str direction 提示-的str 提示-出口str)
                  my-avatar)))
  (done))

(define (take-thing name)
  (let ((thing (find-thing name (here))))
    (if thing
        (take-thing! thing my-avatar)))
  (done))

(define (drop-thing name)
  (let ((thing (find-thing name my-avatar)))
    (if thing
        (drop-thing! thing my-avatar)))
  (done))

(define (look-in-bag #:optional person-name)
  (let ((person
         (if (default-object? person-name)
             my-avatar
             (find-person person-name))))
    (if person
        (tell! (let ((referent (local-possessive person))
                     (things (get-things person)))
                 (if (pair? things)
                     (cons* referent 提示-包str  things)
                     (list referent 提示-包str 提示-是str 提示-空str 提示-的str)))
               my-avatar)))
  (done))

(define (whats-here)
  (look-around my-avatar)
  (done))

(define (say . message)
  (say! my-avatar message)
  (done))

(define (tell person-name . message)
  (tell! message (find-person person-name))
  (done))

(define (hang-out ticks)
  ;; 仅有定义,没被引用 2024年7月27日11:13:07
  ;; 交互开始后(hang-out 3)用来跳过三个回合 2024年7月27日11:17:17
  (do ((i 0 (+ i 1)))
      ((not (< i ticks)))
    (tick! (get-clock)))
  (done))

;;; Support for UI

(define (here)
  (get-location my-avatar))

(define (find-person name)
  (let ((person
         (find-object-by-name name (people-here my-avatar))))
    (if (not person)
        (tell! (list 这str 提示-没有str 叫str name 提示-的str)
               my-avatar))
    person))

(define (find-thing name person-or-place)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell! (cons* 这str 提示-没有str 叫str
                      name 提示-的str
                      (person-or-place-name person-or-place))
               my-avatar))
    thing))

(define (person-or-place-things person-or-place)
  (if (place? person-or-place)
      (all-things-in-place person-or-place)
      (get-things person-or-place)))

(define (person-or-place-name person-or-place)
  (if (place? person-or-place)
      (list 这str)
      (list 提示-在str (local-possessive person-or-place) 提示-包str 提示-里str)))

(define (local-possessive person)	;返回人名  2024年1月28日18:02:59
  (if (eqv? person my-avatar)
      提示-你str
      (possessive person)))


(define (create-mit)
  (let ((great-dome (create-place 'great-dome))
        (little-dome (create-place 'little-dome))
        (lobby-10 (create-place 'lobby-10))
        (10-250 (create-place '10-250))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (infinite (create-place 'infinite-corridor))

        (bldg-26 (create-place 'bldg-26))
        (cp32 (create-place 'bldg-32-cp-hq))
        (tunnel (create-place 'lab-supplies))

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32D (create-place 'dreyfoos-tower))
        (student-street (create-place 'student-street))
        (great-court (create-place 'great-court))
        (bldg-54 (create-place 'green-building))
        (the-dot (create-place 'the-dot))
        (dorm-row (create-place 'dorm-row)))

    (can-go-both-ways lobby-10 'up 'down 10-250) ;这个会报错 Exception in sort: (((#<procedure predicate at tagging.scm:2964>) . #<procedure at adventure-objects.scm:2764>)) is not a procedure 2024年1月8日22:08:00
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)

    ; Add line-of-sight into the mix
    (can-see bldg-54 32G)
    (can-see bldg-54 32D)
    (can-see bldg-54 great-dome)
    (can-see bldg-54 little-dome)
    (can-see bldg-54 great-court)
    (can-see bldg-54 the-dot)
    (can-see lobby-10 great-court)
    (can-see great-dome great-court)
    (can-see-both-ways 32D 32G)
    (can-see-both-ways great-dome little-dome)
    (can-see-both-ways lobby-10 infinite)
    (can-see-both-ways lobby-7 infinite)
    (can-see-both-ways infinite bldg-26)
    (can-see-both-ways lobby-10 lobby-7)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (make 回血药剂? 'name '小红瓶 'location student-street)
    (make 刀具? 'name '水果刀 'location student-street '装备部位 #f '攻击力 1 '攻击距离 10)
    (create-mobile-thing 'engineering-book barker-library)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          dorm-row)))

(define (create-people places)
  (append (create-students places)
          ;;(create-profs places)
          ;;(create-president places)
          (create-house-masters places)
          (create-trolls places)))

(define (create-students places)	;Exception in clock-things: #<void> is not of type #<record type <clock>>,某个期望是<clock>实例的地方出现了<void> 2024年1月11日19:46:38
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

;; (define (create-profs places)
;;   (map (lambda (name)
;;          (create-professor name
;;                            (random-choice places)
;;                            1/3
;;                            1/3))
;;        '(rob-miller eric-grimson)))

;; (define (create-president places)
;;   (create-president 'rafael-reif
;;                     (random-choice places)
;;                     (random-bias 3)
;;                     (random-bias 3)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-place name)
  (make-place 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name place restlessness hunger)
  (make-troll 'name name
              'location place
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name place screen)
  (make-avatar带装备栏 'name name
		       'location place
		       'screen screen))

(define (can-go-both-ways from direction reverse-direction to) 
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))

;; (create-mit)
;; (define from (create-place 'lobby-10))
;; (define db (instance-data-bindings (tagged-data-data from
;; 						     )))

;; (define places-mit (create-mit))

;; (create-students places-mit)

;; (start-adventure '爱宠莎莎)

;;;
(定义匹配度优先广义过程 更新! 2 (constant-generic-procedure-handler #f))
;; (define 游戏累积时间间隔 0) 因为使用了固定的时间间隔,所以完全不需要获取累计时间间隔,这样看来直接tiktok就可以了 2024年11月2日21:21:12

(define 游戏状态 0)
(define 之前空间层级 0)
(define 当前空间层级 0)
(define 全部空间层级 2)
(define 下扩层数 1)
(define 上扩层数 0)
(define 前一游戏状态 0)
;; (define 前一需要渲染的实体 'nothing)   ;; (set! 需要渲染的实体 前一需要渲染的实体) ;会变成引用同一个内存,前一个和现在一模一样

(define (负数偏移mod a b 下偏移量)
  (-  (mod (+ a 下偏移量) b) 下偏移量))

(define (空间层级更新! n)
  (set! 当前空间层级 (负数偏移mod (+ n 当前空间层级) 全部空间层级 下扩层数))
  当前空间层级
  )

(define (取决于空间层级的更新! n)
  (case n
    [(0) (0层渲染包更新!)
     ]
    [(-1) (-1层渲染包更新!)]))

(define (0层渲染包更新!)
  (clear! 需要渲染的实体)
  (place渲染包更新! (get-location my-avatar) 游戏字体 需要渲染的实体 (get-x坐标 当前place坐标) (get-y坐标 当前place坐标) 红色)
  (for-each (lambda (exit)
	      (exit渲染包更新! exit 游戏字体 需要渲染的实体 当前place坐标 绿色))
	    (get-exits (get-location my-avatar))))

(define (-1层渲染包更新!)
  ;; 因为要输入按键信息,更新玩家的位置,要能够取出玩家的渲染数据 2024年10月9日11:25:44
  
  (clear! 需要渲染的实体)
  (obj-str矩形渲染包更新! (get-location my-avatar) 游戏字体 需要渲染的实体 (中点 (- 窗口宽 room-w)) (中点 (- 窗口高 room-h)) room-w room-h 蓝色  make-文本-矩形UI-空间)
  (obj-str矩形ls随机位置渲染包更新! (things-here my-avatar) 游戏字体 需要渲染的实体 (- 窗口宽 (中点 (- 窗口宽 room-w)) thing-w) (- 窗口高 thing-h (中点 (- 窗口高 room-h))) thing-w thing-h 橙色  random make-文本-矩形关联实体UI-物品)
  (obj-str矩形-关联实体渲染包更新! my-avatar 游戏字体 需要渲染的实体 (中点 (- 窗口宽 people-w)) (中点 (- 窗口高 people-h)) people-w people-h 红色   make-玩家UI)
  (obj-str矩形ls随机位置渲染包更新! (people-here my-avatar) 游戏字体 需要渲染的实体 (- 窗口宽 (中点 (- 窗口宽 room-w)) people-w) (- 窗口高 people-h (中点 (- 窗口高 room-h))) people-w people-h 绿色  random make-文本-矩形关联实体UI-人物)
  
  ;; (obj-str矩形ls随机位置渲染包更新! (vistas-here my-avatar) 游戏字体 需要渲染的实体 (- 窗口宽 vistas-w) (- 窗口高 vistas-h) vistas-w vistas-h 白色 (game-render-get sdf-adven) random)
  )

(define (物品栏渲染包更新!)
  (clear! 需要渲染的实体)
  (测试输出 (map get-name (get-things (get-bag my-avatar)))) ;切出物品栏再切回来之后,已经装备的东西依然出现在了物品栏 2024年10月27日22:11:35
  (实体-adder 需要渲染的实体 (make 被渲染的物品栏? 'name '被渲染的物品栏
				   '实体 (物品ls->可渲染rect物品容器  (get-things my-avatar) 0 物品栏物品数 物品栏列数 物品栏左上角x 物品栏左上角y 物品栏物品宽 物品栏物品高 游戏字体)))
  (实体-adder 需要渲染的实体 ( ;; type-instantiator
			      make
				 文本-矩形UI-按钮? '显示的文本 (创建字符串-ctcr 丢弃str)
			      'rect (make-sdl-rect 物品栏左上角x (- 物品栏左上角y (* 2 物品栏物品高)) 物品栏物品宽 物品栏物品高)
			      'color 蓝色
			      'game-字体 游戏字体))
  (实体-adder 需要渲染的实体 ( ;; type-instantiator
			      make
				 文本-矩形UI-按钮? '显示的文本 (创建字符串-ctcr 使用str)
				 'rect (make-sdl-rect (+ 物品栏左上角x 物品栏物品高 10) (- 物品栏左上角y (* 2 物品栏物品高)) 物品栏物品宽 物品栏物品高)
				 'color 蓝色
				 'game-字体 游戏字体))

  (实体-adder 需要渲染的实体 (make 被渲染的物品栏? 'name '被渲染的装备栏
				   '实体 (装备栏UI生成 my-avatar)))
  )

(define (物品ls->可渲染rect物品容器 物品ls 起始位置 总数 每行个数 initx inity w h 字体)
  (let loop ((待转换ls (切片 物品ls 起始位置 (min (- (length 物品ls) 起始位置) 总数)))
	     (x initx)
	     (y inity)
	     (当前行个数 0)
	     (acc '()))
    (cond ((null? 待转换ls) acc)
	  ((<= 当前行个数 每行个数) (loop (cdr 待转换ls)
					 (+ x w)
					 y
					 (+ 当前行个数 1)
					 (cons (make 文本-矩形关联实体UI-物品?
						 '显示的文本 (创建字符串-ctcr (symbol->string (get-name (car 待转换ls))))
						 'entity (car 待转换ls)
						 'rect (make-sdl-rect x y w h)
						 'color 物品栏UI未选中颜色) acc)))
	  (else
	   (loop 待转换ls
		 initx
		 (+ y h)
		 0
		 acc)))))

(define (装备栏UI生成 角色)
  ;; 已经有装备的装备栏,注意名称的显示 2024年10月23日15:48:11
  (let ((装备栏 (get-装备栏 角色)))
    ;; 如果有太多装备物品的话,这个过程效率会很低
    (map 
     (lambda (name rect)
       (let ((装备物品 (find-obj-by 装备栏 装备于? name)))
	 ;; (测试输出 (get-name 装备物品))
	 (if 装备物品
	     (make 文本-矩形关联实体UI-装备部位? 'name name
		   'entity 装备物品
		   '显示的文本 (创建字符串-ctcr (symbol->string (get-name 装备物品)))
		   'rect rect 'color 物品栏UI未选中颜色)
	     (make 文本-矩形关联实体UI-装备部位? 'name name
		   'entity '未装备
		   '显示的文本 (创建字符串-ctcr (symbol->string name)) ;应该扩展一下原有的实现,把借鉴某个属性同supplier结合 2024年10月28日19:54:59
		   'rect rect 'color 物品栏UI未选中颜色)
	     )))
     已知的装备部位 (构造装备栏渲染矩形ls))))

(define (装备于? 部位 装备物品)
  (eqv? (get-装备部位 装备物品) 部位))

(define (-1层执行更新!)
  ;; 需要扩展更新玩家角色的坐标了...2024年9月28日22:16:19
  (cond
   ((sdl-event-key-down? SDLK-TAB)
    (set! 前一游戏状态 游戏状态)
    (set! 游戏状态 '物品栏状态)
    (物品栏渲染包更新!)
    ;; (测试输出 (map (lambda (obj) (get-rect obj)) (filter 文本-矩形UI? (get-实体 需要渲染的实体)))) ;可以筛选出来
    )
   (else
    (let ((玩家ls (获取某类对象 需要渲染的实体 玩家UI?)))
      (if (null? 玩家ls)
	  (begin (测试输出 (map get-name (get-实体 需要渲染的实体)))
		 )
	  (玩家更新! (car 玩家ls) 需要渲染的实体)))
    (层级更新!))))

(define (获取某类对象 含实体的实例 类型谓词)
  (filter 类型谓词 (get-实体 含实体的实例)))

(define (从容器获取某类对象 获取容器的过程 含容器的实例 类型谓词)
  (filter 类型谓词 (获取容器的过程 含容器的实例)))

(define (玩家更新! obj env)
  (let ((之前朝向 (get-朝向 obj)))
    (cond ((sdl-event-key-down? SDLK-UP) (set-速度! obj (* -1 玩家速度模值 单位虚数)) (set-朝向! obj 0-i))
	  ((sdl-event-key-down? SDLK-DOWN) (set-速度! obj (* 玩家速度模值 单位虚数)) (set-朝向! obj 0+i))
	  ((sdl-event-key-down? SDLK-RIGHT) (set-速度! obj  玩家速度模值) (set-朝向! obj 1))
	  ((sdl-event-key-down? SDLK-LEFT) (set-速度! obj (* -1 玩家速度模值)) (set-朝向! obj -1))
	  ((and (sdl-mouse-button-left-click?)
		(= 0 (get-当前普攻冷却时间 obj)))
	   (let ((武器ui (创建角色手持武器UI obj)))
	     (set-当前普攻冷却时间! obj (get-普攻冷却时间 obj)) ;点击且普攻已经冷却,因为需要一个挥刀的时间,
	     (附件UI-adder obj 武器ui)
	     (for-each
	      (lambda (x)
		(cond ((一一碰撞? x 武器ui)	;碰撞触发的更新变得复杂起来了,不重复检测,碰到可捡起物品,和水果刀碰到NPC
		       (攻击! (get-entity obj) (get-entity 武器ui) (get-entity x)))
		      (else '())))
	      (获取某类对象 env 文本-矩形关联实体UI-人物?))))
	  (else
	   (let ((武器UIls (从容器获取某类对象 get-附件UI obj 角色武器UI?)))
	     (when (not (null? 武器UIls))
	       (for-each (lambda (武器ui)
			   (if (<= (get-当前普攻冷却时间 obj) 0)
			       (附件UI-remover obj 武器ui)
			       (武器UI旋转! 武器ui (/ (get-朝向 obj) 之前朝向)
					    (+ (get-x obj) (中点 (- (get-w obj) (get-w 武器ui)))
					       (* 单位虚数 (+ (get-y obj) (中点 (- (get-h obj) (get-h 武器ui))))))) ;应该用当前朝向/之前的朝向得到角度....
			       )) 武器UIls)))
	   )))
  (when (> (get-当前普攻冷却时间 obj) 0)
    (set-当前普攻冷却时间! obj (max 0 (- (get-当前普攻冷却时间 obj) △t))))
  (移动! obj (get-速度 obj))
  (set-速度! obj 0)
  (for-each
   (lambda (x)
     (cond ((一一碰撞? x obj)	;碰撞触发的更新变得复杂起来了,不重复检测,碰到可捡起物品,和水果刀碰到NPC
	    (take-thing! (get-entity x) (get-entity obj))
	    (实体-remover 需要渲染的实体 x))
	   (else '())))
   (获取某类对象 env 文本-矩形关联实体UI-物品?)))

(define 界面更新过程 (lambda ()
		       (测试输出 '其他状态)))

(define (主状态更新!)
  (cond ((sdl-event-key-down? SDLK-UP) (go-扩展渲染 'north))
	((sdl-event-key-down? SDLK-DOWN) (go-扩展渲染 'south))
	((sdl-event-key-down? SDLK-RIGHT) (go-扩展渲染 'east))
	((sdl-event-key-down? SDLK-LEFT) (go-扩展渲染 'west))
	((sdl-event-key-down? SDLK-U) (go-扩展渲染 'up))
	((sdl-event-key-down? SDLK-D) (go-扩展渲染 'down))
	((sdl-event-key-down? SDLK-I) (go-扩展渲染 'in))
	((sdl-event-key-down? SDLK-O) (go-扩展渲染 'out))
	((sdl-event-key-down? SDLK-S) (go-扩展渲染 'skew))
	(else (层级更新!))
	)
  )

(define (层级更新!)
  (cond ((sdl-event-key-down? SDLK-M) (set! 游戏状态 '消息界面打开)
	 (set! 之前空间层级 当前空间层级))
	((sdl-event-key-down? SDLK-EQUALS) (set! 游戏状态 (空间层级更新! 1))
	 (取决于空间层级的更新! 当前空间层级))
	((sdl-event-key-down? SDLK-MINUS) (set! 游戏状态 (空间层级更新! -1))
	 (取决于空间层级的更新! 当前空间层级)
	 )
	(else 'nothing)))

(define (物品栏状态更新!)
  (cond
   ;; 选择当前物品
   ((sdl-event-key-down? SDLK-UP) (go-扩展渲染 'north))
   ((sdl-event-key-down? SDLK-DOWN) (go-扩展渲染 'south))
   ((sdl-event-key-down? SDLK-RIGHT) (go-扩展渲染 'east))
   ((sdl-event-key-down? SDLK-LEFT) (go-扩展渲染 'west))
   ((sdl-event-key-down? SDLK-TAB) (set! 游戏状态 前一游戏状态)
  
    (-1层渲染包更新!)
    ;; (测试输出 (list 'tab2 游戏状态 (get-实体 需要渲染的实体)))
    )
   ;; 鼠标指针碰到且点击左键则设为选中
   ;; ()
   (else (set! 鼠标坐标 (+ (sdl-event-mouse-motion-x) (* 单位虚数 (sdl-event-mouse-motion-y))))
	 (物品栏状态下的渲染对象更新! 需要渲染的实体 'env)
	 )
   ))

(define (矩形被选中? 鼠标指针c rectobj)
  ;; (测试输出 (sdl-event-mouse-button-button))
  (and (一一碰撞? 鼠标指针c rectobj)
       (sdl-mouse-button-left-click?)))

(define (消息界面打开的更新!)
  (cond ((sdl-event-key-down? SDLK-M) (set! 游戏状态 之前空间层级))
	((equal? (sdl-event-mouse-wheel-y) 1) ;上滑
	 (set-消息板最后一行在log的位置! 主要消息板 (max (- (get-消息板最后一行在log的位置 主要消息板) (get-消息板每页字符串数 主要消息板)) 0))
	 (消息板字符串更新! 主要消息板))
	((equal? (sdl-event-mouse-wheel-y) -1) ;向下滑
	 (set-消息板最后一行在log的位置! 主要消息板 (min (+ (get-消息板最后一行在log的位置 主要消息板) (get-消息板每页字符串数 主要消息板)) (length (get-log 主要消息板))))
	 (消息板字符串更新! 主要消息板))
	(else 'nothing)))

(define (状态更新0!)
  ;; 
  (case 游戏状态
    [(主要状态) (主状态更新!)]
    [(消息界面打开) (消息界面打开的更新!)]
    [(物品栏状态) (物品栏状态更新!)]
    [else ;; (set! 游戏状态 '主要状态)
     (测试输出 '其他状态)]))

(define (状态更新!)
  ;; 这个通过状态编码的更新方式,在重拾代码之后很难进入状态,因为每次更新都要注意给状态赋值 2024年10月9日20:59:35
  (case 游戏状态
    [(0) (主状态更新!)]
    [(-1) (-1层执行更新!)]
    [(消息界面打开) (消息界面打开的更新!)]
    [(物品栏状态) (物品栏状态更新!)]
    [else ;; (set! 游戏状态 '主要状态)
     (测试输出 (list "未知状态" 游戏状态))]))

;;; 扩展后的实体



(define-type 装备栏 (bag?) ())

;;; 带装备栏的avatars
(define-sdf-property person:装备栏	;如果有必要就直接定义一个由 可装备物品 构成的容器 2024年10月16日22:14:23
  装备栏
  'predicate 装备栏?			
  'default-supplier
  (lambda () (make-装备栏 'name 	;把这里搞成了bag,但是也能用 2024年10月19日15:14:51
			  '装备栏		;这里类似bag 2024年10月18日16:57:41
			  )))

(define-type avatar带装备栏 (avatar?) (person:装备栏))

(define-generic-procedure-handler set-up! (match-args avatar带装备栏?)
  (lambda (super person)
    (super person)
    (set-holder! (get-装备栏 person) person)))

;; (define-type )

;; (广义过程扩展 get-装备栏 ((avatar带装备栏? avatar带装备栏))
;; 	      (get-things (get-装备栏 avatar带装备栏))
;; 	      )

;; (define (create-avatar带装备栏 name place screen)
;;   (make-avatar带装备栏 'name name
;; 		       'location place
;; 		       'screen screen))


;;; 方案1,根据功能不同,定义各种子类型,扩展各种方法:不同的鼓槌打鼓
;;; 方案2,给功能性物品增加一个属性,保存λ的列表
(define-sdf-property gobj:功能容器
  功能列表
  'predicate (is-list-of procedure?)			;暂时设置为所有物品,如有必要改成可移动非人物品 2024年10月14日20:56:00
  'default-value '()
  )

(define-type 可使用物品 (thing?) (gobj:功能容器))

;;; 方案1:分别实现不同部位的装备槽,方便避免戴两个同样位置装备的情况,同时可以根据装备位置不同发挥不同作用,
;;; 三刀流问题,外星人/虫族问题,

;; (define-sdf-property gobj:装备物品
;;   被装备的物品
;;   'predicate (is-list-of thing?)			;暂时设置为所有物品,如有必要改成可移动非人物品 2024年10月14日20:56:00
;;   'default-value '()
;;   )

;; (define-type 装备部位 (container?) (gobj:装备物品))
;; (define-type 装备槽-头部 (装备槽?) ())
;; (define-type 装备槽-面部 (装备槽?) ())

;;; 方案2:给装备设置装备部位的属性,但是竹蜻蜓确实可以装在屁股上
(define (装备状态? 装备状态)
  (or (装备部位? 装备状态) (eq? 装备状态 #f)))

(define (装备部位? object)
  (if (memv object 已知的装备部位) #t #f))

(register-predicate! 装备部位? '装备部位)

(define 已知的装备部位
  '(头顶 面部 颈部 上身 左腿 右腿 左脚 右脚 左臂 右臂 左手 右手 未装备))

(define-sdf-property gobj:装备部位
  装备部位
  'predicate 装备部位?			;暂时设置为所有物品,如有必要改成可移动非人物品 2024年10月14日20:56:00
  )

(define-type 可装备物品 (可使用物品? mobile-thing?) (gobj:装备部位))

;;; 使用 主体 物品 客体
;;; 使用 主体 物品
;;; 设置 主体 物品 客体
;;; 如果在不同部位使用效果不同的话,最好再加入一个参数和对应的不同类型,这样更方便扩展.

;;; 任意门
;;; 空气炮
;;; 放大灯
;;; 黑白圆珠笔
;;; 地雷
;;; 飞鞋
;;; 药品
(定义匹配度优先广义过程 设置! 3 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 使用! 2 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 对某物使用! 3 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 卸下! 3 (constant-generic-procedure-handler #f))


(define-type 回血药剂 (可使用物品? mobile-thing?) ())	;为了UI点了使用之后只对能直接使用的物品奏效 2024年10月27日20:34:08

(广义过程扩展 使用! ((person? 主体) (回血药剂? 物品))
	      (narrate! (list 主体 "使用了" 物品 "恢复了1点生命值") 主体)
	      (set-health! 主体 (+ (get-health 主体) 1))
	      (remove-thing! (get-bag 主体) 物品)
	      )

(define (use-thing name)
  (let ((thing (find-thing name (get-bag my-avatar))))
    (when thing
      (使用! my-avatar thing))))

(define-generic-procedure-handler generic-move!
  (match-args 可装备物品? bag? 装备栏? person?)
   (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
	     (narrate! (list actor
                             "装备了" mobile-thing)
                       actor)
	     (move-internal! mobile-thing from to)
	     )
            (else
             (narrate! (list actor
                             "从" former-holder "的背包"
			     "取走了" mobile-thing
                             "并扔掉")
                       actor)
	     (say! former-holder
                    (list "你干嘛~!"))))
      )))

(define-generic-procedure-handler generic-move!
  (match-args 可装备物品? 装备栏? bag? person?)
   (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
	     (narrate! (list actor
                             "卸下了" mobile-thing "放回了背包里")
                       actor)
	     (move-internal! mobile-thing from to)
	     )
            (else
             (narrate! (list actor
                             "从" former-holder "的装备栏"
			     "取走了" mobile-thing
                             "并扔掉")
                       actor)
	     (say! former-holder
                    (list "你咋能卸掉我的装备!"))))
      )))

(define (获取已装备物品 可装备物品角色)
  (get-things (get-装备栏 可装备物品角色)))

(广义过程扩展 设置! ((avatar带装备栏? 主体) (可装备物品? 装备) (装备部位? 部位))
	      ;; 没有被装备的情况,装备部位
	      ;; 装备栏放着物品还是装备部位? 断腿问题
	      ;;
	      (set-装备部位! 装备 部位)
	      (let ((装备栏部位
		     (filter (lambda (obj)
			       (eqv? (get-装备部位 obj) 部位))
			     (获取已装备物品 主体))))
		(for-each (lambda (obj)	;如果部位上已经有了装备,就移回背包 2024年10月23日20:54:46
			    (move! obj (get-bag 主体) 主体)) 装备栏部位)
		(move! 装备 (get-装备栏 主体) 主体))
	      )

(define (装备/卸下! name 部位 getfoo 装备/卸下foo)
  (let ((thing (find-thing name (getfoo my-avatar))))
    (when thing
      (装备/卸下foo my-avatar thing 部位))))

(define (装备! name 部位)
    (let ((thing (find-thing name (get-bag my-avatar))))
      (when thing
	(设置! my-avatar thing 部位))))

(广义过程扩展 卸下! ((avatar带装备栏? 主体) (可装备物品? 装备) (装备部位? 部位))
	      (when (eqv? 部位 (get-装备部位 装备))
		(set-装备部位! 装备 '未装备identifier)
		(move! 装备 (get-bag 主体) 主体)))

(define (卸下装备! name 部位)
    (let ((thing (find-thing name (get-装备栏 my-avatar))))
      (when thing
	(卸下! my-avatar thing 部位))))

(define (创建可使用物品 name location λls)
  (make-thing 'name name
              'location location
	      '功能列表 λls))

(广义过程扩展 使用! ((person? 主体) (可使用物品? 物品))
	      (let loop ((待应用过程 (get-功能列表 物品)))
		(if (null? 待应用过程)
		    'done
		    (begin (apply (car 待应用过程) (list 主体 物品))
			   (loop (cdr 待应用过程))))))

(define-sdf-property 攻击物品:攻击力
  攻击力
  'predicate (is-list-of procedure?)			;暂时设置为所有物品,如有必要改成可移动非人物品 2024年10月14日20:56:00
  )

(define-sdf-property 武器物品:攻击距离
  攻击距离
  'predicate (is-list-of procedure?)		 	;暂时设置为所有物品,如有必要改成可移动非人物品 2024年10月14日20:56:00
  )

(define-type 造成伤害的物品 (可装备物品?) (攻击物品:攻击力))
(define-type 武器 (造成伤害的物品?) (武器物品:攻击距离))

(define-type 刀具 (武器?) ())


(定义匹配度优先广义过程 攻击! 3 (lambda () '未定义的攻击))

(广义过程扩展 攻击! ((person? 主体) (刀具? 伤人物品) (person? 客体))
	      (let ((伤害值 (get-攻击力 伤人物品)))
		(say! 客体(list 痛苦嚎叫str 伤害值 提示-的str 伤害值str 让我很难受str))
		(set-health! 客体 (- (get-health 客体) 伤害值)))
	      (if (< (get-health 客体) 1) ;这个判断是否狗带的表达式应该作为统一的后续处理 2024年10月29日20:18:06
		  (die! 客体)))

(define (get-手部装备 某人)
  (or (find-obj-by (get-装备栏 某人) 装备于? '右手)
      (find-obj-by (get-装备栏 某人) 装备于? '左手)))

(define (用某物攻击某人 物品名称 对方名称)
  (攻击! my-avatar (get-手部装备 my-avatar) (find-object-by-name 对方名称 (people-here my-avatar))))

;;; 渲染的部分
(定义匹配度优先广义过程 渲染! 2 (constant-generic-procedure-handler #f))

(define (go-扩展渲染 direction)
  (go direction)
  (0层渲染包更新!)
  )

(define 蓝色 (make-sdl-color 0 0 255 0))
(define 靛色 (make-sdl-color 0 255 255 0))
(define 红色 (make-sdl-color 255 0 0 0))
(define 绿色 (make-sdl-color 0 255 0 0))
(define 黄色 (make-sdl-color 255 255 0 0))
(define 橙色 (make-sdl-color 255 128 0 0))
(define 青色 (make-sdl-color 128 255 0 0))

(define 物品栏UI选中颜色 黄色)
(define 物品栏UI未选中颜色 蓝色)

;;;按功能增加一层,方便修改和替换
(define-sdf-property gobj:w
  w
  'predicate integer?
  'default-value 0
  )

(define-sdf-property gobj:h
  h
  'predicate integer?
  'default-value 0
  )

(define-sdf-property gobj:object
  entity
  'predicate (lambda (x) (or (symbol? x) (object? x)))
  'default-value '未关联实体
  )

(define (实体未关联? 关联实体的对象)
  (and (关联实体的UI? 关联实体的对象)
       (symbol? (get-entity 关联实体的对象)))
  )

(define-type 关联实体的UI (UI类型?)	;这种将其它类型作为属性的方式,会导致要另外实现一套接口来穿透... 2024年10月24日22:52:57
  (gobj:object))

(define 无名关联实体的UI?
  (conjoin 关联实体的UI? (complement object?)))

(广义过程扩展 get-name ((无名关联实体的UI? obj))
	      (get-name (get-entity obj)))

(广义过程扩展 get-description ((关联实体的UI? obj))
	      (get-description (get-entity obj)))

;; (define-type 文本-矩形UI (文本-矩形UI?)
;;   (;; object:name
;;    )
;;   )

(define-type 文本-矩形关联实体UI (文本-矩形UI? 关联实体的UI?) ())

;; (广义过程扩展 get-name ((文本-矩形关联实体UI? obj))
;; 	      (get-name (get-entity obj)))

(定义匹配度优先广义过程 get-r 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-g 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-b 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-a 1 (constant-generic-procedure-handler #f))

(广义过程扩展 get-r ((UI类型? obj))
	      (sdl-color-r (get-color obj)))

(广义过程扩展 get-g ((UI类型? obj))
	      (sdl-color-g (get-color obj)))

(广义过程扩展 get-b ((UI类型? obj))
	      (sdl-color-b (get-color obj)))

(广义过程扩展 get-a ((UI类型? obj))
	      (sdl-color-a (get-color obj)))

(广义过程扩展 get-r ((sdl-color? obj))
	      (sdl-color-r obj))

(广义过程扩展 get-g ((sdl-color? obj))
	      (sdl-color-g  obj))

(广义过程扩展 get-b ((sdl-color? obj))
	      (sdl-color-b  obj))

(广义过程扩展 get-a ((sdl-color? obj))
	      (sdl-color-a  obj))

(定义匹配度优先广义过程 get-x 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-y 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-w 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 get-h 1 (constant-generic-procedure-handler #f))

(define 含rectobj? 含rectUI?)

(广义过程扩展 get-x ((含rectobj? obj))
	      (sdl-rect-x (get-rect obj)))

(广义过程扩展 get-y ((含rectobj? obj))
	      (sdl-rect-y (get-rect obj)))

(广义过程扩展 get-w ((含rectobj? obj))
	      (sdl-rect-w (get-rect obj)))

(广义过程扩展 get-h ((含rectobj? obj))
	      (sdl-rect-h (get-rect obj)))

(广义过程扩展 get-x ((sdl-rect? obj))
	      (sdl-rect-x obj))

(广义过程扩展 get-y ((sdl-rect? obj))
	      (sdl-rect-y obj))

(广义过程扩展 get-w ((sdl-rect? obj))
	      (sdl-rect-w obj))

(广义过程扩展 get-h ((sdl-rect? obj))
	      (sdl-rect-h obj))

(定义匹配度优先广义过程 set-x! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-y! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-w! 1 (constant-generic-procedure-handler #f))
(定义匹配度优先广义过程 set-h! 1 (constant-generic-procedure-handler #f))

(广义过程扩展 set-x! ((含rectobj? obj) (real? v))
	      (sdl-rect-x-set! (get-rect obj) v))

(广义过程扩展 set-y! ((含rectobj? obj) (real? v))
	      (sdl-rect-y-set! (get-rect obj) v))

(定义匹配度优先广义过程 移动! 2 (constant-generic-procedure-handler #f))

(广义过程扩展 移动! ((含rectobj? obj) (complex? v))
	      (set-x! obj (+ (get-x obj) (坐标x v)))
	      (set-y! obj (+ (get-y obj) (坐标y v)))
	      )

(广义过程扩展 移动! ((含rectobj? obj) (坐标对象? v))
	      (set-x! obj (+ (get-x obj) (get-x坐标 v)))
	      (set-y! obj (+ (get-y obj) (get-y坐标 v)))
	      )

(定义匹配度优先广义过程 一一碰撞? 2 (constant-generic-procedure-handler #f))

(广义过程扩展 一一碰撞? ((含rectobj? obj1) (含rectobj? obj2))
	      (碰撞? (get-x obj1) (get-y obj1) (get-w obj1) (get-h obj1)
		     (get-x obj2) (get-y obj2) (get-w obj2) (get-h obj2)
		     )
	      )

(广义过程扩展 一一碰撞? ((complex? obj1) (含rectobj? obj2))
	      (碰撞? (real-part obj1) (imag-part obj1) 1 1
		     (get-x obj2) (get-y obj2) (get-w obj2) (get-h obj2)
		     )
	      )

(define-type 文本-矩形UI-物品 (文本-矩形UI?) ())
(define-type 文本-矩形UI-人物 (文本-矩形UI?) ())
(define-type 文本-矩形UI-玩家 (文本-矩形UI?) ())
(define-type 文本-矩形UI-空间 (文本-矩形UI?) ())
(define-type 文本-矩形UI-按钮 (文本-矩形UI?) ())

(define-type 文本-矩形关联实体UI-物品 (文本-矩形关联实体UI?) ())
(define-type 文本-矩形关联实体UI-人物 (文本-矩形关联实体UI?) ())
(define-type 文本-矩形关联实体UI-玩家 (文本-矩形关联实体UI?) ())
(define-type 文本-矩形关联实体UI-按钮 (文本-矩形关联实体UI?) ())

(define-type 文本-矩形关联实体UI-装备部位 (文本-矩形关联实体UI-按钮? object?) ())

(define 无名UI类型?
  (conjoin UI类型? (complement object?)))

(广义过程扩展 get-name ((无名UI类型? 含文本UI实例))
	      (get-字符串 (get-显示的文本 含文本UI实例)))

;;; 加入手持冷兵器的效果
(define-sdf-property UI:跟随目标
  跟随目标
  'predicate 含rectUI?
  'default-value '未跟随具体目标
  )

(define-type 关联实体的矩形UI (含rectUI? 关联实体的UI?) ())

;; (define-sdf-property UI:跟随目标
;;   跟随目标
;;   'predicate 含rectUI?
;;   'default-value '未跟随具体目标
;;   )

(define-sdf-property UI:基础矩形	;用于方便实现
  基础矩形				
  'predicate 含rectUI?
  'default-value '无基础矩形
  )

;;; 直接引用一个被跟随的UI,或者将这个类型作为被跟随UI的容器中的实例
;;; 前一方案直接将跟随UI加入渲染的实体,单独实现移动和渲染的方案,移除时也需要直接从渲染的实体移除,玩家go die等情况会有点麻烦
;;; 后一方案需要修改被跟随UI的移动过程,
(define-type 关联实体矩形附件UI (含rectUI? 关联实体的UI?) (UI:跟随目标 UI:基础矩形))

(define-type 角色武器UI (关联实体矩形附件UI?) ())

(define-sdf-property UI:附件容器
  (附件UI adder remover)
  'predicate (is-list-of UI类型?)
  'default-value '())

(define-type 带附件UI (UI类型?) (UI:附件容器))

(property-modifier-extend UI:附件容器 带附件UI? UI类型? 附件UI-adder adder-modifier)
(property-modifier-extend UI:附件容器 带附件UI? UI类型? 附件UI-remover remover-modifier)

(define-type 文本-矩形关联实体带附件UI (文本-矩形关联实体UI? 带附件UI?) ())

(define-sdf-property UI:朝向
  朝向
  'predicate complex?
  'default-value 0-i)

(define-type 玩家UI (文本-矩形关联实体带附件UI?) (UI:朝向))

(define-sdf-property 容器:实体
  (实体 adder remover)
  'predicate (is-list-of object?)
  'default-value '())

(define-type 容器 (object?) (容器:实体))

(define-type 被渲染的容器 (容器?) ())

(define 需要渲染的实体 (make-被渲染的容器 'name '被渲染的容器
					  '实体 '()))

(property-modifier-extend 容器:实体 被渲染的容器? 文本-矩形UI? 实体-adder adder-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 文本-矩形UI? 实体-remover remover-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 容器? 实体-adder adder-modifier)
(property-modifier-extend 容器:实体 被渲染的容器? 容器? 实体-remover remover-modifier)

(define-type 被渲染的物品栏 (被渲染的容器?) ())

(define-type 被渲染的装备栏 (被渲染的容器?) ())

(定义匹配度优先广义过程 clear! 1 (constant-generic-procedure-handler #f))
(广义过程扩展 clear! ((容器? obj))
	      (set-实体! obj '()))

(定义链式广义过程 render! 1 (constant-generic-procedure-handler '没有定义渲染方式))

(广义过程扩展 render! super ((含文本UI? obj))
	      (super obj)
	      (let ((w (get-w obj))
		    (h (get-h obj)))
		(矩形区域渲染字符串 (get-game-字体 (get-显示的文本 obj)) (current-renderer) (get-字符串 (get-显示的文本 obj))
				    (舍入取整 (+ (get-x obj) (中点 w))) (舍入取整 (get-y obj))
				    (get-r obj) (get-g obj) (get-b obj) (get-a obj)
				    '居中 w h 0 8 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE)))

(广义过程扩展 render! super ((含rectUI? obj))
	      (super obj)
	      (sdl-set-render-draw-color! (current-renderer) (get-r obj) (get-g obj) (get-b obj) (get-a obj))
	      (sdl-render-draw-rect (current-renderer) (get-rect obj))
	      )

(广义过程扩展 render! super ((文本-矩形UI? obj))
	      (super obj)
	      ;; (sdl-set-render-draw-color! (current-renderer) (get-r obj) (get-g obj) (get-b obj) (get-a obj))
	      ;; (sdl-render-draw-rect renderer (get-rect obj))
	      ;; (let ((w (get-w obj))
	      ;; 	    (h (get-h obj)))
	      ;; 	(矩形区域渲染字符串 (get-game-字体 (get-显示的文本 obj)) (current-renderer) (get-字符串 (get-显示的文本 obj))
	      ;; 			    (舍入取整 (+ (get-x obj) (中点 w))) (舍入取整 (get-y obj))
	      ;; 			    (get-r obj) (get-g obj) (get-b obj) (get-a obj)
	      ;; 			    '居中 w h 0 8 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE))
	      )
(define 消息面板边框颜色 蓝色)
(define 消息面板文本颜色 白色)

(定义匹配度优先广义过程 render-匹配! 1 (constant-generic-procedure-handler '没有定义的匹配优先渲染方式))

(广义过程扩展 render! super ((消息板? obj))
	      (sdl-set-render-draw-color! (current-renderer) (get-r 消息面板边框颜色) (get-g 消息面板边框颜色) (get-b 消息面板边框颜色) (get-a 消息面板边框颜色))
	      (sdl-render-draw-rect (current-renderer) (get-rect obj))
	      (let ((w (get-w obj))
		    (h (get-h obj)))
		;; (sdl-set-render-draw-color! renderer 255 255 255 125)
		(矩形区域渲染字符串 (get-game-字体 (get-显示的文本 obj)) (current-renderer) (get-字符串 (get-显示的文本 obj)) (get-x obj) (get-y obj)
				    (get-r obj) (get-g obj) (get-b obj) (get-a obj)
				    '左对齐 w h 0 8 'x 'x 1 1 0.0 NULL SDL_FLIP_NONE))
	      )

(广义过程扩展 render! super ((被渲染的容器? obj))
	      (super obj)
	      (for-each (lambda (obj)
			  (render! obj))
			(get-实体 obj)))

;;; 因为要控制玩家移动,单纯的可渲染sdl类型不够用,而且这个东西在列表里面,不好取出,所以保存一个更新过程进入闭包
(define-sdf-property gobj:更新过程
  更新过程!
  'predicate procedure?
  'default-value (lambda (x) x)
  )

(define-type 自带更新过程的可渲染矩形? (文本-矩形UI?)
  (gobj:更新过程))

(define (创建自带更新过程的可渲染矩形 name rect color 字体 更新foo)
  (make-自带更新过程的可渲染矩形 'name (symbol->string name) 'rect rect 'color color 'game-字体 字体 '更新过程 更新foo))

(define (玩家角色更新! obj)
  (cond ((sdl-event-key-down? SDLK-UP) (go-扩展渲染 'north))
	((sdl-event-key-down? SDLK-DOWN) (go-扩展渲染 'south))
	((sdl-event-key-down? SDLK-RIGHT) (go-扩展渲染 'east))
	((sdl-event-key-down? SDLK-LEFT) (go-扩展渲染 'west))
	(else 'nothing)))

(定义匹配度优先广义过程 物品栏状态下的渲染对象更新! 2 (constant-generic-procedure-handler #f))
(广义过程扩展 物品栏状态下的渲染对象更新! ((容器? obj) (any-object? env))
	      (for-each (lambda (obj)
			  (物品栏状态下的渲染对象更新! obj env))
			(get-实体 obj)))

(define (选中后变更颜色! obj)
   (cond
    ((矩形被选中? 鼠标坐标 obj)
     (if (eqv? 物品栏UI未选中颜色 (get-color obj))
	 (set-color! obj 物品栏UI选中颜色)
	 (set-color! obj 物品栏UI未选中颜色)))
    (else '())))

(define (物品栏UIrect选中? obj)
  (eqv? obj 物品栏UI选中颜色))

(广义过程扩展 物品栏状态下的渲染对象更新! ((文本-矩形UI? obj) (any-object? env))
	      (选中后变更颜色! obj)
	      )

(广义过程扩展 物品栏状态下的渲染对象更新! ((文本-矩形UI-按钮? obj) (any-object? env))
	      (选中后变更颜色! obj)
	      ;; (测试输出 0.5)
	      (cond
	       ((and (eqv? 物品栏UI选中颜色 (get-color obj))
		     (eqv? 丢弃str (get-name obj)))
		(从UI包获取目标UI容器并act! 需要渲染的实体 '被渲染的物品栏 从物品栏丢弃!)
		)
	       ((and (eqv? 物品栏UI选中颜色 (get-color obj))
		     (eqv? 使用str (get-name obj)))
		(从UI包获取目标UI容器并act! 需要渲染的实体 '被渲染的物品栏 从物品栏使用!)
		)
	       (else '())))

(define (从UI包获取目标UI容器并act! UI包 目标name actfoo)
  (let ((目标容器 (find-object-by-name 目标name (get-实体 UI包))))
    (when 目标容器
      ;; (测试输出 (get-实体 目标容器))
      (actfoo 目标容器))))

(define (颜色eqv? 颜色 对象)
  (eqv? 颜色 (get-color 对象)))
 
(define (从物品栏丢弃! 物品栏容器)
  (for-each  (lambda (obj)
	       (实体-remover 物品栏容器 obj)
	       (drop-thing! (get-entity obj) my-avatar)) ;如果有必要,就换成当前操作的角色
	     (filter (lambda (obj)
		       ;; (测试输出 (get-name obj))
		       (eqv? (get-color obj) 物品栏UI选中颜色)) (get-实体 物品栏容器))))

(define (从物品栏使用! 物品栏容器)
  (for-each (lambda (obj)
	      (实体-remover 物品栏容器 obj)
	      (使用! my-avatar (get-entity obj)))
	    (filter (lambda (obj)
		      (and (可使用物品? (get-entity obj))
			   (eqv? (get-color obj) 物品栏UI选中颜色))) (get-实体 物品栏容器))))

(定义匹配度优先广义过程 find-obj-by 3 (constant-generic-procedure-handler #f))

(define list类对象? (disjoin 容器? container?))

(广义过程扩展 get-实体 ((container? sdf容器))
	      (get-things sdf容器))

(广义过程扩展 find-obj-by ((list类对象? 容器) (procedure? 谓词) (any-object? 索引))
	      (find (lambda (object)
		      (谓词 索引 object))
		    (get-实体 容器)))

(define (颜色且装备部位一致? 装备部位颜色ls object)
  (and (equal? (car 装备部位颜色ls) (get-装备部位 object))
       (eqv? (cadr 装备部位颜色ls) (get-color object))))

(define 装备部位未装备物品? 实体未关联?)

(define (物品栏ui装备物品! 装备部位 装备 物品栏UI)
  ;; (let ((当前装备名称str (symbol->string (get-name 装备))))) ;不需要这个
  (设置! my-avatar (get-entity 装备) (get-name 装备部位))
  (测试输出 (map get-name (获取已装备物品 my-avatar)))
  (set-显示的文本! 装备部位 (get-显示的文本 装备))
  (set-entity! 装备部位 (get-entity 装备)) ;可能还需要扩展字符 2024年10月20日18:50:58
  (set-color! 装备部位 物品栏UI未选中颜色)
  (实体-remover 物品栏UI 装备)		;移除物品栏部分的渲染 2024年10月21日11:42:58
  )

(define (物品栏UI物品部分后续新增! 物品栏UI 装备实体)
  (实体-adder (find-object-by-name '被渲染的物品栏 (get-实体 需要渲染的实体)) ;这里麻烦了,需要获取最后一个矩形,构造一个,名字也要搞对
	      (let ((最后一个rect (get-物品栏UI最后一格 物品栏UI))
		    )
		(make 文本-矩形关联实体UI-物品?
		  'entity 装备实体
		  '显示的文本 (创建字符串-ctcr (symbol->string (get-name 装备实体)))
		  'rect (if (>= (/ (- (get-x 最后一个rect) 物品栏左上角x) 物品栏物品宽) (- 物品栏列数 1)) ;判断是否换行 2024年10月22日15:59:58
			    (make-sdl-rect  物品栏左上角x (+ 物品栏物品宽 (get-x 最后一个rect)) 物品栏左上角y 物品栏物品宽 物品栏物品高)
			    (make-sdl-rect (+ 物品栏物品宽 (get-x 最后一个rect)) (get-y 最后一个rect) 物品栏物品宽 物品栏物品高)
			    )
		  'color 物品栏UI未选中颜色
		  )))
  )

(define (物品栏ui卸下装备物品! 装备部位 物品栏UI)
  ;; (let ((当前装备名称str (symbol->string (get-name 装备))))) ;不需要这个
  (卸下! my-avatar (get-entity 装备部位) (get-name 装备部位))
  (物品栏UI物品部分后续新增! 物品栏UI (get-entity 装备部位))
  (set-仅字符串! 装备部位 (get-name 装备部位))
  (set-entity! 装备部位 '未装备)
  (set-color! 装备部位 物品栏UI未选中颜色)
  )

(define (坐标更东北? 含xy1 含xy2)
  (and (>= (get-x 含xy1) (get-x 含xy2))
       (>= (get-y 含xy1) (get-y 含xy2))))

(define (get-坐标最东北的 含xyls 当前值)
  (cond ((null? 含xyls) 当前值)
	((坐标更东北? (car 含xyls) 当前值) (get-坐标最东北的 (cdr 含xyls) (car 含xyls)))
	(else (get-坐标最东北的 (cdr 含xyls) 当前值))))

(define (get-物品栏UI最后一格 物品栏UI)
  (get-坐标最东北的 (get-实体 物品栏UI)   (make 文本-矩形UI-物品?
					    'name 最东北默认str
					    'rect (make-sdl-rect (- 物品栏左上角x 物品栏物品宽) 物品栏左上角y 物品栏物品宽 物品栏物品高)
					    'color 物品栏UI未选中颜色
					    'game-字体 游戏字体)))

(define (是可装备物且颜色eqv? 颜色 obj)
  (and (可装备物品? (get-entity obj))	
       (颜色eqv? 颜色 obj)
       ))

(广义过程扩展 物品栏状态下的渲染对象更新! ((文本-矩形关联实体UI-装备部位? obj) (any-object? env))
	      (选中后变更颜色! obj)
	      (cond
	       ((eqv? 物品栏UI选中颜色 (get-color obj))
		(let* ((物品栏UI (find-object-by-name '被渲染的物品栏 (get-实体 需要渲染的实体)))	;null?
		       (装备 (find-obj-by 物品栏UI 是可装备物且颜色eqv? 物品栏UI选中颜色)))
		  (测试输出 (get-name obj))
		  (if (装备部位未装备物品? obj)
		      (when 装备	;没装备,同时选中了物品,就装备上
			(物品栏ui装备物品! obj 装备 物品栏UI)
			)
		      (cond (装备 (测试输出 (or 装备 (get-name 装备)))
			     (物品栏ui卸下装备物品! obj 物品栏UI)
			     (物品栏ui装备物品! obj 装备 物品栏UI)) ;装备了物品,同时选中了物品,就替换
			    ((矩形被右键选中? 鼠标坐标 obj) (物品栏ui卸下装备物品! obj 物品栏UI)) ;单纯卸下装备
			    ))))
	       (else '())))


(define (矩形被右键选中? 鼠标指针c rectobj)
  ;; (测试输出 (sdl-event-mouse-button-button))
  (and (一一碰撞? 鼠标指针c rectobj)
       (sdl-mouse-button-right-click?)))

(define (创建角色手持武器UI 角色UI)
  (let* ((手部装备 (get-手部装备 (get-entity 角色UI)))
	 )
    (if 手部装备
	(let* ((部位 (get-装备部位 手部装备))
	       (朝向 (get-朝向 角色UI))
	       (攻击距离 (get-攻击距离 手部装备))
	       (旋转相对中点w (中点 (- (get-w 角色UI) *武器UI贴合面宽度-纵向*))) ;因为横向和纵向一样,所以能直接简化为旋转 2024年11月7日10:51:49
	       (旋转相对中点h (中点 (- (get-h 角色UI) *武器UI贴合面宽度-横向*)))
	       (基础矩形 (case 朝向	;相对于角色的偏移量xy和自身规格wh
			   [(0+i 0-i)
			    (make-sdl-rect 旋转相对中点w (case 朝向
							   [(0-i) (- 攻击距离)]
							   [(0+i) (get-h 角色UI)]
							   )
					   *武器UI贴合面宽度-纵向* 攻击距离)]
			   [(-1 1)
			    (make-sdl-rect  (case 朝向
					      [(-1) (- 攻击距离)]
					      [(1) (get-h 角色UI)]
					      )
					    旋转相对中点h  攻击距离 *武器UI贴合面宽度-横向*)]
			   [else
			    (make-sdl-rect 0 (* (imag-part 朝向) (+ 旋转相对中点h (中点 攻击距离))) *武器UI贴合面宽度-纵向* 攻击距离)]
			   )) ;w和h会因为朝向,
	       )
	  (make-角色武器UI 'entity 手部装备
			   '基础矩形 基础矩形
			   'rect (make-sdl-rect (+ (get-x 角色UI)  (get-x 基础矩形)) (+ (get-y 角色UI)  (get-y 基础矩形)) (get-w 基础矩形) (get-h 基础矩形))
			   '跟随目标 角色UI))
	'某种武术))
  )

(define (武器UI旋转! 武器ui 角度c 中心点c)
  (let ((旋转后c (旋转 (rectxy->c 武器ui) 中心点c 角度c))
	(旋转后w (case 角度c
		   [(0+i 0-i) (get-h 武器ui)]
		   [else (get-w 武器ui)]))
	(旋转后h (case 角度c
		   [(0+i 0-i) (get-w 武器ui)]
		   [else (get-h 武器ui)])))
    (set-rect! 武器ui (make-sdl-rect (real-part 旋转后c) (imag-part 旋转后c) 旋转后w 旋转后h)) 
    ))

(define (中心点坐标获取 含rectUI)
  (+ (get-x 含rectUI) (中点 (get-w 含rectUI))
     (* 单位虚数 (+ (get-y 含rectUI) (中点 (get-h 含rectUI))))))

(define (xy->c x y)
  (+ x (* 单位虚数 y)))

(define (rectxy->c rect)
  (xy->c (get-x rect) (get-y rect)))

(广义过程扩展 render! super ((带附件UI? obj))
	      (super obj)
	      (for-each (lambda (obj)
			  (render! obj)) (get-附件UI obj))
	      )

;; (广义过程扩展 render! super ((关联实体矩形附件UI? obj))
;; 	      (super obj)
;; 	      (sdl-set-render-draw-color! renderer (get-r obj) (get-g obj) (get-b obj) (get-a obj))
;; 	      (sdl-render-draw-rect renderer (get-rect obj))	;渲染时计算可能太蠢,不如在玩家UI移动后就直接更新伴随玩家的UI
;; 	      )

(广义过程扩展 get-速度 ((玩家UI? obj))
	      (get-速度 (get-entity obj)))

(广义过程扩展 set-速度! ((玩家UI? obj) (complex? v))
	      (set-速度! (get-entity obj) v))

(广义过程扩展 get-普攻冷却时间 ((玩家UI? obj))
	      (get-普攻冷却时间 (get-entity obj)))

(广义过程扩展 get-当前普攻冷却时间 ((玩家UI? obj))
	      (get-当前普攻冷却时间 (get-entity obj)))

(广义过程扩展 set-当前普攻冷却时间! ((玩家UI? obj) (complex? v))
	      (set-当前普攻冷却时间! (get-entity obj) v))

(广义过程扩展 移动! ((玩家UI? obj) (坐标对象? v)) ;应该也链式一下
	      (set-x! obj (+ (get-x obj) (get-x坐标 v)))
	      (set-y! obj (+ (get-y obj) (get-y坐标 v)))
	      (for-each (lambda (obj)
			  (移动! obj v))
			(get-附件UI obj)
			)
	      )

(define (中点 值)
  (/ 值 2))

(define 窗口宽 1280)
(define 窗口高 960)
(define place-w 100)
(define place-h place-w)

(define thing-w 100)
(define thing-h 62)

(define people-w 100)			;为了简化UI的处理 2024年11月5日21:06:34
(define people-h 100)

(define room-w (floor (* 0.9 窗口宽)))
(define room-h (floor (* 0.9 窗口高)))

(define 物品栏物品宽 100)
(define 物品栏物品高 100)
(define 物品栏列数 (floor (/ (- 窗口宽 (* 物品栏物品宽 2)) 物品栏物品宽)))
(define 物品栏行数 (floor (/ (- (中点 窗口高) (* 物品栏物品高 2)) 物品栏物品高)))
(define 物品栏物品数 (* 物品栏列数 物品栏行数))
(define 物品栏左上角x  (中点 (- 窗口宽 (* 物品栏列数 物品栏物品宽))))
(define 物品栏左上角y  (中点 (+  窗口高 物品栏物品高)))

(define 半窗口高 (中点 窗口高))
(define 半窗口宽 (中点 窗口宽))

(define 鼠标坐标 0+0i)

(define 装备栏UIx (map (lambda (x)
			 ;; 相对于给定x偏移对应量,得到列表
			 (+ x 半窗口宽 (中点 半窗口宽))) '(-50 -40 -30 -72 -74 4 -84 4 -106 76 -116 76)))

(define 装备栏UIy (map (lambda (y)
			 (+ y 半窗口高)) '(-432 -410 -328 -306 -162 -162 -20 -20 -306 -306 -154 -154)))

(define 装备栏UIw '(100 80 60 144 70 70 80 80 30 30 40 40))
(define 装备栏UIh '(20 80 20 144 140 140 20 20 150 150 40 40))

(define (构造装备栏渲染矩形ls)
  (map make-sdl-rect
       装备栏UIx 装备栏UIy 装备栏UIw 装备栏UIh))


(define 当前place坐标 (创建坐标对象 (+ (中点 (- 窗口宽 place-w)) (* (中点 (- 窗口高 place-h)) 单位虚数))))

;;; 渲染带武器的角色需要
(define *角色武器UI偏移x* (/ people-w 4))
(define *角色武器UI偏移y* (/ people-h 4))
(define *武器UI贴合面宽度-纵向* *角色武器UI偏移x*)
(define *武器UI贴合面宽度-横向* *角色武器UI偏移y*)
(define *武器UI竖直轴偏移量* (中点 *武器UI贴合面宽度-纵向*))
(define *武器UI水平轴偏移量* (中点 *武器UI贴合面宽度-横向*))
(define *人物UI竖直对称轴* (中点 people-w))
(define *人物UI水平对称轴* (中点 people-h))

(define (direction->坐标 direction)
  (创建坐标对象 
   (case direction
     [(up) (* 3 单位虚数 (- place-h))]
     [(down) (* 3 单位虚数 place-h)]
     [(east) (* place-w)]
     [(west) (* (- place-w))]
     [(north) (* 单位虚数 (- place-h))]
     [(south) (* 单位虚数  place-h)]
     [(in) (+ (* 3 place-w) (* 3 单位虚数 place-w))]
     [(out) (+ (* -3 place-w) (* -3 单位虚数 place-w))]
     [(skew) (+ (* 3 place-w) (* -3 单位虚数 place-w))]
     [else 0])))

;;; UI交互后会影响背后逻辑实体的,应该全都关联实体 2024年10月27日10:15:12
(define (obj-str矩形渲染包更新! obj 字体 需要渲染的实体 x y w h color  文本-矩形UI构造foo)
  ;; 为了能通过单一过程构造多种不同标签的文本-矩形UI,同时避免手写多个构造器 2024年10月9日20:25:00
  (let ((name (get-name obj)))
    (实体-adder 需要渲染的实体 (文本-矩形UI构造foo '显示的文本 (创建字符串-ctcr (symbol->string name))
						  'rect (make-sdl-rect x y w h)
						  'color color)))
  )

(define (obj-str矩形-关联实体渲染包更新! obj 字体 需要渲染的实体 x y w h color  文本-矩形关联实体UI构造foo)
  ;; 为了能通过单一过程构造多种不同标签的文本-矩形UI,同时避免手写多个构造器 2024年10月9日20:25:00
  (let ((name (get-name obj)))
    (实体-adder 需要渲染的实体 (文本-矩形关联实体UI构造foo '显示的文本 (创建字符串-ctcr (symbol->string name))
							   'entity obj
							   'rect (make-sdl-rect x y w h)
							   'color color)))
  )

(define (obj-str矩形ls随机位置渲染包更新! objls 字体 需要渲染的实体 x y w h color  randomfoo 文本-矩形UI构造foo)
  (for-each (lambda (obj)
	      (obj-str矩形-关联实体渲染包更新! obj 字体 需要渲染的实体 (randomfoo x) (randomfoo y) w h color  文本-矩形UI构造foo))
	    objls)
  )

(define (place渲染包更新! place 字体 需要渲染的实体 x y color )
  (obj-str矩形渲染包更新! place 字体 需要渲染的实体 x y place-w place-h color  make-文本-矩形UI)
  )

(define (exit渲染包更新! exit 字体 需要渲染的实体 原点place坐标 color )
  (let ((坐标 (+ (get-坐标 原点place坐标) (get-坐标 (direction->坐标 (get-direction exit))))))
    (place渲染包更新! (get-to exit) 字体 需要渲染的实体 (坐标x 坐标) (坐标y 坐标) color))
  )

(define (things渲染包更新! things 字体 需要渲染的实体 x y color )
  (obj-str矩形ls随机位置渲染包更新! things 字体 需要渲染的实体 (- 窗口宽 thing-w) (- 窗口高 thing-h) thing-w thing-h color  random make-文本-矩形关联实体UI-物品))

(define (NPCS渲染包更新! people 字体 需要渲染的实体 x y color )
  (obj-str矩形ls随机位置渲染包更新! people 字体 需要渲染的实体 (- 窗口宽 people-w) (- 窗口高 people-h) people-w people-h color  random make-文本-矩形关联实体UI-人物))

(define 帧率 160)
(define △t (/ 1000 帧率))
(define NULL)

(define 玩家速度模值 (/ 1000 帧率))

(define 字体path "C:/Windows/Fonts/simfang.ttf")
(define 颜色位深 32)

(define wintitle "MIT巨魔")
(define sdf-adven (创建game SDL-INIT-EVERYTHING IMG_INIT_EVERYTHING (bitwise-ior MIX_INIT_FLAC MIX_INIT_MP3 MIX_INIT_OGG)
			    △t wintitle SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED  窗口宽 窗口高))

(define 游戏字体 (创建字体 字体path 24 480 颜色位深))
(set! *current-font* 游戏字体)

(define 主要消息板 (make-消息板 
				'rect (make-sdl-rect 20 20 390 (- 窗口高 20))
				))

(define 丢弃str "丢弃")
(define 使用str "使用")
(define 最东北默认str "最东北的虚拟物品")
;; (map (lambda (strls)))
;; (字体扩展字符! 游戏字体 (list->set (string->list
;; 				    (string-append 丢弃str 使用str 最东北默认str
;; 						   ))) (game-render-get sdf-adven))
(for-each (lambda (str)
	    (字体扩展字符! 游戏字体 (list->set  (string->list str)) (game-render-get sdf-adven)))
	  (append (map symbol->string 已知的装备部位)
		  (list 丢弃str 使用str)))

(define (条件渲染!)
  (let ((renderer (game-render-get sdf-adven)))
    (case 游戏状态
      ;; [(主要状态) (render! 需要渲染的实体 renderer)]
      [(消息界面打开) (render! 需要渲染的实体)
       (render! 主要消息板)
       ]
      [else 
       (render! 需要渲染的实体)])))

;; ;;; 维护一个被更新的实体容器然后for-each clock-tick!,接受两个参数,对象和环境这样试试
;; ;;; 维护一个被渲染的实体容器然后for-each render!

(set-port! 主要screen
	   ;; (current-output-port)
	   主要消息板
	   )

(start-adventure '留子)
;; (set! my-avatar (create-avatar带装备栏 '柯南 (random-choice all-places) 主要screen))

(define (core game)
  (let* ((脉冲间隔 0)
	 (累积时间间隔 0)
	 (时间间隔 (get-每帧毫秒 game))
	 (计时器0 (make-计时器))
	 (renderer (game-render-get game))
	 )
    (启动 计时器0)
    (0层渲染包更新!)
    (lambda (m)
      (set! 脉冲间隔 (获取时间戳 计时器0)) ;大的离谱..... 2024年7月29日01:18:26
      ;; (set! 时间间隔 (获取时间戳 计时器0))
      (启动 计时器0) 	;重置计时器,下次获取时间坐标时就是时间间隔
      (set! 累积时间间隔 (+ 脉冲间隔 累积时间间隔))
     
      (let loop ((lag 累积时间间隔))
	(if (>= lag 时间间隔)
	    (begin
	      ;; (测试输出 (list '当前累积时间 (floor lag)))
	      (状态更新!)
	      (loop (- lag 时间间隔)))
	    (set! 累积时间间隔 lag)))
      
      ;; (obj更新 游戏状态 '() '() 谓词-act并联映射 游戏状态谓词-actls)
      ;; (测试输出 (list '累积时间 累积时间间隔))
      ;; (测试输出 (list '脉冲间隔 脉冲间隔))
      (sdl-set-render-draw-color! renderer 0 0 0 0)
      (sdl-render-clear renderer)
      (条件渲染!)
      (sdl-render-present renderer)
      ;; (当前渲染器)
      )))

(define (run)
  (游戏循环 sdf-adven
	    (core sdf-adven)))

;;; 测试用例
;; (define-sdf-property 几何:w
;;   w
;;   'predicate real?
;;   'default-value 0)

;; (define-sdf-property 几何:h
;;   h
;;   'predicate real?
;;   'default-value 0)

;; (define-sdf-property 几何:边长
;;   边长
;;   'predicate real?
;;   'default-value 0)

;; (define-type 矩形 () (几何:w 几何:h))
;; (define-type 菱形 () (几何:边长))
;; (define-type 正方形 (矩形? 菱形?) ())

;; (定义链式广义过程 求面积 1 (constant-generic-procedure-handler #f))

;; (广义过程扩展 求面积 super ((矩形? 矩形))
;; 	      (super 矩形)
;; 	      (测试输出 '矩形)
;; 	      (* (get-w 矩形) (get-h 矩形)))

;; (广义过程扩展 求面积 super ((菱形? 菱形))
;; 	      (super 菱形)
;; 	      (测试输出 '菱形)
;; 	      (* 1 1))

;; (广义过程扩展 求面积 super ((正方形? 矩形))
;; 	      (super 矩形)
;; 	      (测试输出 '矩形)
;; 	      (* (get-w 矩形) (get-h 矩形)))
;; (run)

;;; 放大灯和镜子
;;; AI
;;; cl的热更新
;;; 网络
(sdl-net-init)

;; 定义服务器监听函数
(define (start-server port)
  ;; deepseek 极强,牛逼 2025年1月25日23:10:58
  ;; 初始化 SDL_net
  (when (not (zero? (sdl-net-init)))
    (error 'SDLNet_Init (SDLNet_GetError)))

  ;; 创建 IPaddress 结构体指针
  (define ip-ptr (make-IPaddress))

  ;; 解析主机地址（服务器模式，host 为 #f）
  (if (zero? (SDLNet_ResolveHost ip-ptr #f port))
      (begin
        ;; 尝试打开 TCP 监听套接字
        (define server-socket (SDLNet_TCP_Open ip-ptr))
        (if (not (null-pointer? server-socket))
            (begin
              (printf "Server listening on port ~a.~%" port)
              ;; 此处可添加接受客户端连接的逻辑
              (SDLNet_TCP_Close server-socket))
            (error 'SDLNet_TCP_Open (SDLNet_GetError))))
      (error 'SDLNet_ResolveHost (SDLNet_GetError)))
  
(define address1 (make-sdl-net-ipaddress 0 0))
(define ipc (make-sdl-net-ipaddress 0 443))
(sdl-net-resolve-host! address1 "" 15366) ;返回0,成功了
(sdl-net-resolve-host! ipc "www.baidu.com" 443)

(sdl-net-resolve-ip address1)		;返回了本机名....
(sdl-net-tcp-open ipc)			;这个作为客户端的是ok的

(sdl-net-tcp-open address1)		;这个返回空指针 2025-1-25 22:32:45
(sdl-net-get-error) ;; -> "Couldn't connect to remote host"

;;; 空气炮:光标,旋转的素材,文本贴图的自动生成.
