

(import
  (except (sdf sdf) is-non-empty-list-of is-list-of is-pair-of complement conjoin disjoin)
  (sdf sdf-udp)
  )

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

;;;; Properties,用了record实现,但是因为二次修饰了,导致后面定义一个属性就得手工构造record本来可以自动构造的东西....  2024年3月4日19:55:16
(define-record-type (<property> %make-property property?)
  (fields (immutable name property-name)
	  (immutable predicate property-predicate)
	  (immutable default-supplier property-default-supplier)
	  ;; (immutable convert-supplier property-convert-supplier)
	  )
  )

(define (make-property name . plist)	;构造属性的方法,对record做了封装,plist可以换成hashtable  2023年12月31日16:26:26
  (guarantee symbol? name)		;相较于书中的示例代码,少了提示的文本 2023年12月31日16:14:51
  (guarantee property-list? plist)	;判断接受的参数是否符合形式
  (%make-property name
                  (get-predicate-property plist)
                  (get-default-supplier-property plist)))

(define (property-list? object)
  ;; 用于make-property的guarantee
  (and (plist? object)
       (<= (count (lambda (keyword)	;(count number? '(1 2)) -> 2  2024年1月22日19:46:18
                    (not (default-object?
                           (plist-value object keyword))))
                  property-default-keywords)
           1)))

(define (get-predicate-property plist)
  ;; 用于make-property的实现
   (let ((predicate (plist-value plist 'predicate)))
     (if (not (default-object? predicate))
         predicate
         any-object?)))

(define (get-default-supplier-property plist)
  ;; 用于make-property的实现,三种默认情况,冒险游戏中,应该将包的名称定义为某某的包这样,supplier需要同to-property一样,取得其他属性,转化值
  (let ((value (plist-value plist 'default-value))
        (supplier (plist-value plist 'default-supplier)) ;求值supplier,得到属性值,比如属性是某个sdf的type时
        (property (plist-value plist 'default-to-property)) ;直接指定属性为同一类型下的其他属性一样的值
	)
    (cond ((not (default-object? value))
           (lambda (lookup) value))	;有点迷惑  2023年12月31日16:28:14
          ((not (default-object? supplier))
           (lambda (lookup) (supplier)))
          ((not (default-object? property))
           (lambda (lookup) (lookup property)))
          (else #f))))

(define property-default-keywords
  ;; 用于构造property-list?,单纯的将get-default-supplier-property符号key放进了一个列表 2024年8月10日16:59:22
  '(default-value default-supplier default-to-property))

(define property-keywords
  ;; 作用不明确,应该是展开成了predicate default-value default-supplier default-to-property,但是没在别处被引用 2024年8月10日17:01:39
  `(predicate ,@property-default-keywords))

(define (property-optional? property)
  ;; parse-plist的构造引用了 2024年8月10日17:02:29
  (if (property-default-supplier property) #t #f))

;; (define-record-printer <property>	;需要引用utlis中定义的过程,暂时先注释掉 2023年12月31日21:49:26
;;   (lambda (property)
;;     (list (property-name property))))

;;;; Types
(define type?)
(define %type-properties)
(define %set-type-properties!)

(let ((association (make-metadata-association)))
  (set! type? (association 'has?))
  (set! %type-properties (association 'get))
  (set! %set-type-properties! (association 'put!)))

(define (make-type name properties)	;基于属性谓词的类型构造,type就是名字加属性list构造的gp,实际使用的时候被create-xxtype 封装  2024年1月22日19:51:23
  (guarantee-list-of property? properties)
  (let ((type (simple-abstract-predicate name instance-data?)))
    (%set-type-properties! type properties)
    type))

(define (get-binding property instance)
  ;; 返回一个过程,无参数调用返回属性的值,有参数调用(binding value)可以给属性赋值 2024年8月10日17:33:09
  (instance-data-binding property (tagged-data-data instance)))	

;;; Simplified interface for text -- GJS

(define (get-property-value property object) ;获取实例的指定属性  2024年1月22日21:01:17
  ;; property-getter的底层实现 2024年8月10日17:34:52
  ((get-binding property object)))

(define (set-property-value! property object value) ;给实例的指定属性赋值 2024年1月22日21:01:32
  ;; 虽然提供了这个封装,但是没用上,因为实现set-type-property!时加入了debug的表达式 2024年8月10日17:36:02
  ((get-binding property object) value))

(define (type-properties type)		;某一type的全部属性,封装了下面的过程 2024年1月22日21:01:55
  (append-map %type-properties
              (cons type (all-supertypes type))))

(define (all-supertypes type)		;某一类的所有基类 2024年8月10日17:36:16
  (filter type? (all-predicate-supersets type)))
;;;; Instantiation

(define (type-instantiator type)	;某一type实例的构造器,构造了type之后应该自动生成一个对应的,原代码目前没实现  2024年1月22日21:03:20
  ;; 4月已经用syntax-case扩展了定义类型时自动构造构造器的能力 2024年9月4日23:37:22
  (let ((constructor (predicate-constructor type)) 
        (properties (type-properties type)))
    (lambda plist
      (let ((object
             (constructor (parse-plist plist properties))))
        (set-up! object)
        object))))

(define (make type . plist)
  (apply (type-instantiator type) plist))

;;; TODO: use properties as the keys in the plist.
(define (parse-plist plist properties)	;这个plist形如'(k1 v1 k2 v2 ...)
  ;; 用于解析plist,实现type-instantiator 2024年8月10日17:37:54
  (define (lookup-value property)
    (let ((value (plist-value plist (property-name property))))
      (if (default-object? value)
          (begin
            (if (not (property-optional? property))
                (error 'parse-plist "Missing required property:"
                       (property-name property)
                       plist))
            ((property-default-supplier property) lookup-value))
          value)))

  (make-instance-data
   (map (lambda (property)
          (cons property (lookup-value property)))
        properties)))

(define set-up!				;error-generic-procedure-handler: Inapplicable generic procedure: with irritants (get-tag-shared (#[#{simple-tag ggb5i5qobi1g1l6270n43mnon-63} #[...]]))
  ;; 用于实现type-instantiator,提供链式调用的过程扩展,比如给背包加入东西,还有加入clock的更新 2024年8月10日17:42:13
  (chaining-generic-procedure 'set-up! 1
    (constant-generic-procedure-handler #f)))

(define tear-down!
  ;; 用于实现type-instantiator,提供链式调用的过程扩展,比如给背包移除东西,还有移除clock的更新 2024年8月10日17:42:08
  (chaining-generic-procedure 'tear-down! 1
    (constant-generic-procedure-handler #f)))

;;;; Instance data

(define instance-data?			;type的实例是一个gp谓词过程,用于构造属性 2024年1月22日19:58:52
  ;; 用于make-type的实现 
  (simple-abstract-predicate 'instance-data procedure?))

(define make-instance-data		;用于parse-plist,实例化type用 2024年1月22日21:07:32
  (let ((constructor
         (predicate-constructor instance-data?)))
    (lambda (bindings)
      (constructor
       (lambda* (#:optional property)
         (if (default-object? property)
             (map car bindings)
             (let ((p (assv property bindings)))
               (if (not p)
                   (error 'make-instance-data "Unknown property:" property))
               (lambda* (#:optional new-value)
			(if (default-object? new-value)
			    (cdr p)
			    (set-cdr! p new-value))))))))))

(define instance-data-bindings
  (predicate-accessor instance-data?))	;这个东西返回的过程报错 incorrect number of arguments to #<procedure at adventure-substrate.scm:5110> ,这种问题十分难以定位,过于恶心 2024年1月10日20:41:35

(define (instance-data-properties instance-data)
  ;; 用于扩展tagged-data-description 2024年8月10日17:45:28
  ((instance-data-bindings instance-data)))

(define (instance-data-binding property instance-data)
  ;; 用于实现get-binding
  ((instance-data-bindings instance-data) property))

;;;; Methods
(define (%binding-set-prefix property new-value old-value object)
  ;; 用于构造getter和setter,在debug-output启用时会抛出信息 2024年8月11日11:39:27
  (if debug-output			;依赖debug-output的行为,启用这个又依赖screen类型 2024年8月10日11:27:38
      (begin
        (send-message! (list ";setting" (possessive object)
                             (property-name property)
                             "to" new-value)
                       debug-output)
        (send-message! (list ";previnstance-data-bindingious value was" old-value)
                       debug-output))))

(define (property-modifier property type value-predicate
                           noun modifier) ;本质上是基于已经存在的值,进行增减,比如下面两个  2024年3月6日22:04:03
  ;; 这个过程也并不完善,但是目前没法塞进宏里面,因为这个modifier会接受参数noun来定义不同的广义过程
  (let ((procedure
         (most-specific-generic-procedure
          (symbol-append (property-name property) '- noun)
          2
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type value-predicate)
      (lambda (object item)
        (let* ((binding (get-binding property object))
               (old-value (binding))
               (new-value (modifier item old-value))) ;核心在于这里,通过传入不同的modifier过程,实现不同的更新效果 2024年3月6日22:05:02
          (%binding-set-prefix property new-value old-value
                               object)
          (binding new-value))))
    procedure))

(define (property-adder property type value-predicate)
  (property-modifier property type value-predicate 'adder
                     (lambda (value values)
                       (lset-adjoin eqv? values value))))

(define (property-remover property type value-predicate)
  (property-modifier property type value-predicate 'remover
                     (lambda (value values)
                       (delv value values))))

;;; Messaging
(define debug-output #f)

(define (enable-debugging)
  (if (not debug-output)
      (set! debug-output (make-screen 'name 'debug))))

(define (disable-debugging)
  (if debug-output
      (set! debug-output #f)))

;;; 
(define-syntax 定义匹配度优先广义过程
  (syntax-rules ()
    [(_ name arity default-handler)
     (define name
       (most-specific-generic-procedure (quote name) arity default-handler))]))

(define-syntax 广义过程扩展
  (syntax-rules (getter)
    [(_ name ((pred1 arg1) (pred2 arg2) ...) body ...)
     (define-generic-procedure-handler name
       (match-args pred1 pred2 ...)
       (lambda (arg1 arg2 ...)
	 body ...))]
    ))

(define-syntax define-sdf-property
  ;; 如果属性名称一样的话,后来定义的会覆盖之前定义的 2024年9月22日12:00:14
  (lambda (x)
    (syntax-case x ()
      [(_ alias (name modifier-noun ...) keyword value ...)
       (with-syntax ([(modifier ...) (map (lambda (x)
					    (datum->syntax #'name (symbol-append (syntax->datum #'name) '- (syntax->datum x))))
					  #'(modifier-noun ...))])
	 #'(begin
	     (define-sdf-property alias name keyword value ...)
	     ;; (测试输出 'modifier)
	     ;; ...
	     (define modifier (most-specific-generic-procedure
			       'modifier 2
			       (lambda (type item)
				 (display (string-append "can't " (symbol->string 'modifier))) ;可以更具体些 2024年4月3日22:14:41
				 (display type)
				 (display "with")
				 (display item)
				 (newline))
			       ))
	     ...
	     ))]
      [(_ alias name)
       (with-syntax ([access (datum->syntax #'name (symbol-append 'get- (syntax->datum #'name)))]
		     [assign (datum->syntax #'name (symbol-append 'set- (syntax->datum #'name) '!))])
	 #'(begin
	     (define alias (make-property 'name))
	     
	     (define access (most-specific-generic-procedure
			     'access
			     1
			     (lambda (object)
			       (display (string-append "can't " (symbol->string 'access))) ;可以更具体些 2024年4月3日22:14:41
			       (display object)
			       (newline))))
	     

	     (define assign (most-specific-generic-procedure
			     'assign
			     2
			     (lambda (object value1)
			       (display (string-append "can't " (symbol->string 'assign))) ;可以更具体些 2024年4月3日22:14:41
			       (display object)
			       (display "with")
			       (display value1)
			       (newline))))
	     
	     ))]
      [(_ alias name keyword value ...)
       (with-syntax ([access (datum->syntax #'name (symbol-append 'get- (syntax->datum #'name)))]
		     [assign (datum->syntax #'name (symbol-append 'set- (syntax->datum #'name) '!))])
	 #'(begin
	     (define alias (make-property 'name keyword value ...)) ;不要试图使用'keyword或者(quote keyword)的形式来节省调用时的'号...2024年9月13日16:10:51
	     
	     (define access (most-specific-generic-procedure
			     'access
			     1
			     (lambda (object)
			       (display (string-append "can't " (symbol->string 'access))) ;可以更具体些 2024年4月3日22:14:41
			       (display object)
			       (newline))))
	    

	     (define assign (most-specific-generic-procedure
			     'assign
			     2
			     (lambda (object value1)
			       (display (string-append "can't " (symbol->string 'assign))) ;可以更具体些 2024年4月3日22:14:41
			       (display object)
			       (display "with")
			       (display value1)
			       (newline))))
	     
	     ))]
  
      )))

(define (property-modifier-extend property type value-predicate modifier-gp modifier)
  (if (generic-procedure? modifier-gp)
      (define-generic-procedure-handler modifier-gp
	(match-args type value-predicate)
	(lambda (object item)
          (let* ((binding (get-binding property object))
		 (old-value (binding))
		 (new-value (modifier item old-value))) ;核心在于这里,通过传入不同的modifier过程,实现不同的更新效果 2024年3月6日22:05:02
            (%binding-set-prefix property new-value old-value
				 object)
            (binding new-value))))))

(define (symbol-append . s)
  (string->symbol (apply string-append (map symbol->string s))))

(define (bound? sym)
  (memq sym (environment-symbols (interaction-environment))))

(define-syntax define-type
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
	(datum->syntax template-id
		       (string->symbol
			(apply string-append
			       (map (lambda (x)
				      (if (string? x)
					  x
					  (symbol->string (syntax->datum x))))
				    args))))))
    (syntax-case x ()
      [(_ name (superset ...)
	  (property ...))
      
       (with-syntax ([type-name (gen-id #'name #'name "?")]
		     [constructor (gen-id #'name "make-" #'name)]
		     [(access ...)
		      (map (lambda (x)
			     (datum->syntax #'name
					    (symbol-append 'get-
							   (property-name (eval (syntax->datum x))))))
			   #'(property ...)
		     	   )
		      
		      ]
		     [(assign ...)
		      (map (lambda (x)
			     (datum->syntax #'name
					    (symbol-append 'set-
							   (property-name (eval (syntax->datum x)))
							   '!)))
			   #'(property ...))]
		     )
	 #`(begin
	     (define type-name
	       (make-type 'name (list property ...)
			  ))

	     (set-predicate<=! type-name superset)
	     ...
	     
	     (define constructor
	       (type-instantiator type-name))

	     (define-generic-procedure-handler access
	       (match-args type-name)
	       (lambda (object)
		 (get-property-value property object)))
	     ...
	     
	     (define-generic-procedure-handler assign
	       (match-args type-name (property-predicate property))
	       (lambda (object value)
	         (let ((binding (get-binding property object))) ;获取这个binding到调用的过程,没有任何问题
		   (%binding-set-prefix property value (binding) object)	;纯粹用来方便debug的... 2024年3月4日18:48:31
		   (binding value))))
	     ...
	     
	     )
	 )]
      ;; [(_ name (superset ...))
      ;;  (define-type name (superset ...) ())]
      )))

