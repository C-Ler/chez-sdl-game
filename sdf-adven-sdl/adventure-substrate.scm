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

(define-property object:name name)

(define-property object:description description
  'default-to-property object:name)	;这个十分特殊,用宏展开和别的不一样,不需要加'号

(define-type object ()  (object:name object:description))

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
  (narrate! (append (list person "says:") message)
            person))

(define (announce! message)
  (for-each (lambda (place)
              (send-message! message place))
            (get-all-places))
  (if debug-output
      (send-message! message debug-output)))

(define (display-message message port)
  (guarantee message? message 'display-message)
  (if (pair? message)
      (begin
        (fresh-line port)
        (display-item (car message) port)
        (for-each (lambda (item)
                    (display " " port)
                    (display-item item port))
                  (cdr message)))))

(define (display-item item port)
  (display (if (object? item) (get-name item) item) port))

(define (message? object)
  (list? object))

(register-predicate! message? 'message)

(define (possessive person)
  (string-append (display-to-string (get-name person))
                 "'s"))

;;; Screen

(define-property screenport port 'predicate output-port?
  'default-supplier current-output-port)

(define-type screen (object?)  (screenport))

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
  (for-each (lambda (thing) (clock-tick! thing)) ;同时给每个随时间改变的对象clock-tick! 随时钟更新状态 2024年7月26日23:29:37
            (clock-things clock)))

(define clock-tick!
  (chaining-generic-procedure 'clock-tick! 1 ;链式广义过程 2024年7月26日23:30:00
    (constant-generic-procedure-handler #f)))

(define (define-clock-handler type action) ;如果一个type有多个action该怎么办? 2024年5月11日19:58:16
  ;; 这个action是个过程,被封装到handler里面,多个action完全可以合并成一个 2024年7月26日23:39:06
  (define-generic-procedure-handler clock-tick!
    (match-args type)
    (lambda (super object)
      (super object)
      (action object))))

