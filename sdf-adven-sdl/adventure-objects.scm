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

;;; Object types for Adventure game

(define-property thing:location			;property
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
  (let ((scr (make-screen 'name 'the-screen))) ;原本只是个#f,接着报错了,怀疑是捡起东西后提示某物到哪过于简陋 2024年1月23日21:49:58
    (lambda (message thing)
      (display (type-properties thing))
      (display-message message (get-port scr)))))

;; (define-generic-procedure-handler send-message!
;;   (match-args message? object?)
;;   (let ((scr (make-screen 'name 'the-screen))) ;原本只是个#f,接着报错了,怀疑是捡起东西后提示某物到哪过于简陋 2024年1月23日21:49:58
;;     (lambda (message thing)
;;       ;; (display (type-properties thing))
;;       (display-message message (get-port scr)))))

;;; Containers

(define-property container:things
  things
  'predicate (is-list-of thing?)
  'default-value '())

(define-type container (object?) (container:things))

(define add-thing!
  (property-adder container:things container? thing?))

(define remove-thing!
  (property-remover container:things container? thing?))

;;; Exits

(define-property exit:from
  from
  'predicate (lambda (x) (place? x)))

(define-property exit:to
  to
  'predicate (lambda (x) (place? x)))

(define-property exit:direction
  direction
  'predicate direction?)

(define-type exit (object?) (exit:from exit:to exit:direction))

(define-generic-procedure-handler set-up! (match-args exit?)
  (lambda (super exit)
    (super exit)
    (add-exit! (get-from exit) exit)))

;;; Places

(define-property place:vistas
  vistas
  'predicate (lambda (x)
               (and (list? x) (every place? x)))
  'default-value '())

(define-property place:exits
  exits
  'predicate (lambda (x)
               (and (list? x) (every place? x)))
  'default-value '())

(define-type place (container?) (place:vistas place:exits))

(define add-vista!
  (property-adder place:vistas place? place?))

(define add-exit!
  (property-adder place:exits place? exit?))

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

(define-property mobile-thing:origin
  ;; 这个属性在定义类型时默认值就是location这个属性的值
  origin
  'predicate place?
  'default-to-property thing:location)

(define-type mobile-thing (thing?) (mobile-thing:origin))

(define enter-place!
  (chaining-generic-procedure 'enter-place! 1
    (constant-generic-procedure-handler #f)))

(define leave-place!
  (most-specific-generic-procedure 'leave-place! 1
    (constant-generic-procedure-handler #f)))

;;; People

(define-property persohealth
  health
  'predicate exact-integer?
  ''default-value 3)

(define-property persobag
  ;; 默认值是(lambda () (make-bag 'name 'bag)的返回值,这个参数传入时不会被求值... 2024年8月11日17:34:20
  bag
  'predicate (lambda (x) (bag? x))
  'default-supplier
  (lambda () (make-bag 'name 'bag)))	;所有人物的bag都特么叫my-bag,得想个办法,改成xx的bag这样  2024年1月28日18:17:47

(define-type person (mobile-thing?) (persohealth persobag))


(define-generic-procedure-handler set-up! (match-args person?)
  (lambda (super person)
    (super person)
    (set-holder! (get-bag person) person)))

(define-generic-procedure-handler get-things (match-args person?)
  (lambda (person)
    (get-things (get-bag person))))

(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (narrate! (list person "enters" (get-location person))
              person)
    (let ((people (people-here person)))
      (if (pair? people)
          (say! person (cons "Hi" people))))))

(define-generic-procedure-handler send-message!	;缺失了这个,导致非玩家角色行动后的各种旁白,tell!都会匹配到默认gp-handler,接着抛异常 2024年1月22日21:44:05
  (match-args message? person?)
  (let ((scr (make-screen 'name 'the-screen)))
    (lambda (message person)
      (display-message message (get-port scr)))))

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
  (say! person (list "Ouch!" hits "hits is more than I want!"))
  (set-health! person (- (get-health person) hits))
  (if (< (get-health person) 1)
      (die! person)))

(define (die! person)
  (for-each (lambda (thing)
              (drop-thing! thing person))
            (get-things person))
  (announce!
   '("An earth-shattering, soul-piercing scream is heard..."))
  (set-health! person 0)
  (move! person (get-heaven) person))

(define (resurrect! person health)
  (guarantee positive? health)
  (set-health! person health)
  (move! person (get-origin person) person))

;;; Bags

(define-property bag:holder
  holder
  'predicate
  (lambda (x) (or (not x) (person? x)))
  'default-value #f)

(define-type bag (container?)  (bag:holder))

;;; Autonomous people (non-player characters)

(define-property autonomous-agent:restlessness
  restlessness
  'predicate bias?)

(define-property autonomous-agent:acquisitiveness
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

(define-property house-master:irritability
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
                    '("What are you doing still up?"
                      "Everyone back to their rooms!"))
              (for-each (lambda (student)
                          (narrate! (list student "goes home to"
                                          (get-origin student))
                                    student)
                          (move! student
                                 (get-origin student)
                                 student))
                        students))
            (say! master
                  '("Grrr... When I catch those students...")))
        (if (pair? students)
            (say! master
                  '("I'll let you off this once..."))))))

(define-clock-handler house-master? irritate-students!)

;;; Trolls

(define-property troll:hunger
  hunger
  'predicate bias?)

(define-type troll (autonomous-agent?) (troll:hunger))

(define (eat-people! troll)
  (if (flip-coin (get-hunger troll))
      (let ((people (people-here troll)))
        (if (null? people)
            (narrate! (list (possessive troll) "belly rumbles")
                      troll)
            (let ((victim (random-choice people)))
              (narrate! (list troll "takes a bite out of" victim)
                        troll)
              (suffer! (random-number 3) victim))))))

(define-clock-handler troll? eat-people!)

;;; Avatars

(define-property avatar:screen
  screen
  'predicate screen?)

(define-type avatar (person?) (avatar:screen))

(define-generic-procedure-handler send-message!
  (match-args message? avatar?)
  (lambda (message avatar)
    (send-message! message (get-screen avatar))))

(define-generic-procedure-handler enter-place!
  (match-args avatar?)
  (lambda (super avatar)
    (super avatar)
    (look-around avatar)
    (tick! (get-clock))))

(define (look-around avatar)
  (tell! (list "You are in" (get-location avatar))
         avatar)
  (let ((my-things (get-things avatar)))
    (if (pair? my-things)
        (tell! (cons "Your bag contains:" my-things)
               avatar)))
  (let ((things
         (append (things-here avatar)
                 (people-here avatar))))
    (if (pair? things)
        (tell! (cons "You see here:" things)
               avatar)))
  (let ((vistas (vistas-here avatar)))
    (if (pair? vistas)
        (tell! (cons "You can see:" vistas)
               avatar)))
  (tell! (let ((exits (exits-here avatar)))
           (if (pair? exits)
               (cons "You can exit:"
                     (map get-direction exits))
               '("There are no exits..."
                 "you are dead and gone to heaven!")))
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
    (tell! (list thing "is not movable")
           actor)))

;; coderef: generic-move:steal
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from))
          (new-holder (get-holder to)))
      (cond ((eqv? from to)
             (tell! (list new-holder "is already carrying"
                          mobile-thing)
                    actor))
            ((eqv? actor former-holder)
             (narrate! (list actor
                             "gives" mobile-thing
                             "to" new-holder)
                       actor))
            ((eqv? actor new-holder)
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and gives it to" new-holder)
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder (list "Yaaaah! I am upset!")))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list "Whoa! Where'd you get this?")))
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
                             "picks up" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "picks up" mobile-thing
                             "and gives it to" new-holder)
                       actor)))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list "Whoa! Thanks, dude!")))
      (move-internal! mobile-thing from to))))

;; coderef: generic-move:drop
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? place? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
             (narrate! (list actor
                             "drops" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and drops it")
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder
                (list "What did you do that for?")))
      (move-internal! mobile-thing from to))))

(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? place? person?)
  (lambda (mobile-thing from to actor)
    (cond ((eqv? from to)
           (tell! (list mobile-thing "is already in" from)
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
             (tell! (list "There is no exit from" from
                          "to" to)
                    actor))
            ((eqv? person actor)
             (narrate! (list person "leaves via the"
                             (get-direction exit) "exit")
                       from)
             (move-internal! person from to))
            (else
             (tell! (list "You can't force"
                          person
                          "to move!")
                    actor))))))

(define (find-exit from to)
  (find (lambda (exit)
          (and (eqv? (get-from exit) from)
               (eqv? (get-to exit) to)))
        (get-exits from)))

(define (move-internal! mobile-thing from to)
  (leave-place! mobile-thing)
  (remove-thing! from mobile-thing)
  (set-location! mobile-thing to)
  (add-thing! to mobile-thing)
  (enter-place! mobile-thing))
