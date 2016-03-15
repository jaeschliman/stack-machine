(ns ^:figwheel-always stack-machine.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [clojure.string :as string]
             [cljs.core.async :as async :refer
              [<! >! chan close! put! alts! timeout]])
     (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (dom/h1 nil (:text data)))))
  app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


;; vectors with a tag as the first element are used to implement
;; primitives such as built-in functions, closures, etc.
(defn prim-str [thing]
  (if (vector? thing)
    (let [[tag name] thing]
      (str "#<" tag " " name ">"))
    (str thing)))

(defn trace! [& args]
  (let [res (string/join " " (map prim-str args))]
    (println res)
    res))

(def root-env (atom {:bindings {} :parent nil}))
(def prims {'+ + '- - '> > '< < 'number? number? 'trace! trace!})
(defn add-primitive-fn [sym]
  (reset! root-env (assoc-in @root-env [:bindings sym] [:prim sym])))
(add-primitive-fn '+)
(add-primitive-fn '-)
(add-primitive-fn '>)
(add-primitive-fn '<)
(add-primitive-fn 'number?)
(add-primitive-fn 'trace!)

;; special operations are special first class functions
;; which take the machine as first argument and run built-in fn
;; (and take the rest of their arguments evaluated)
(def special-ops (atom {}))
(defn add-special-op [sym fn]
  (reset! special-ops (assoc @special-ops sym fn)))

;; special forms are built-ins by name (e.g. if, quote)
;; which have custom evaluation semantics
(def special-forms (atom {}))
(defn add-special-form [sym fn]
  (reset! special-forms (assoc @special-forms sym fn)))

;; tests the value before application
(defn prim-op? [it]
  (and (vector? it)
       (= :prim (first it))
       (get prims (second it))))

;; tests the value before application
(defn special-op? [it]
  (and (vector? it)
       (= :special (first it))
       (get @special-ops (second it))))

;; tests the form before evaluation
(defn special-form? [form]
  (and (list? form)
       (get @special-forms (first form))))

;; identifies a closure
(defn closure? [it]
  (and (vector? it) (= (first it) :closure)))

(defn lookup-symbol [env sym]
  (if (contains? (:bindings env) sym)
    (get-in env [:bindings sym])
    (if-let [parent (:parent env)]
      (lookup-symbol parent sym))))

;; create an empty machine with empty value and control stacks,
;; and the current root environment
(defn machine []
  { :stack [] :env @root-env :value-stack []})

(defn print-machine [m]
  (println "--------------------")
  (println "  control stack:" (:stack m))
  (println "  value   stack:" (:value-stack m))
  (println "  environment:  " (:env m)))


;; helpers to create the stack modifier and reader functions
;; (defined below). the functions all take a machine state,
;; and sometimes an additional value, and return a new machine
;; state. this allows easy composition with the thrush combinator
(defn pusher [key]
  (fn [m v]
    (let [stack (key m)]
      (assoc m key (conj stack v)))))
(defn swapper [key]
  (fn [m v]
    (let [stack (key m)]
      (assoc m key (conj (pop stack) v)))))
(defn popper [key]
  (fn [m]
    (let [stack (key m)]
      (assoc m key (pop stack)))))
(defn topper [key]
  (fn [m]
    (let [stack (key m)]
      (last (get m key)))))

;; our pushers and poppers etc
(def push-instr (pusher :stack))
(def push-value (pusher :value-stack))
(def swap-instr (swapper :stack))
(def swap-value (swapper :value-stack))
(def pop-instr  (popper :stack))
(def pop-value  (popper :value-stack))
(def top-instr  (topper :stack))
(def top-value  (topper :value-stack))

;; the step function defines the various primitives that can run
;; on the control stack (defined below). it is the function used
;; to transition the machine from one state to the next
(defmulti step
  (fn [m]
    (let [instr (last (:stack m))]
      (if (vector? instr)
        (first instr)
        instr))))

;; our 'eval' function. starts with a bare machine,
;; pushes the supplied form and the eval instruction,
;; then runs `step' over the machine until it halts
(defn evl [form]
  (let [m (-> (machine)
              (push-value form)
              (push-instr :eval))]
    (loop [state m]
      (let [next (step state)]
        (if (= next state) state
            (recur next))))))

;; the same as `eval' but slowly prints each intermediate state
;; to the console (can help with debugging)
(defn evl-slow [form]
  (let [m (-> (machine)
              (push-value form)
              (push-instr :eval))]
    (go
      (loop [state m]
        (print-machine state)
        (let [next (step state)]
          (if (= next state) state
              (do (<! (timeout 1000))
                  (recur next))))))))


;; when there is nothing on the control stack, do nothing.
(defmethod step nil [m] m)

;; the eval instruction
(defmethod step :eval [m]
  (let [v (top-value m)     ;;take (not pop) the top value
        m (-> m pop-instr)] ;;pop :eval
    (cond
      ;; self-evaluating forms return the value unchanged
      (string? v) m
      (number? v) m
      (= v true)  m
      (= v false) m
      (vector? v) m ;;closures etc are self-evaluating

      ;; symbols are swapped with their value in the current
      ;; environment
      (symbol? v)
      (let [v' (lookup-symbol (:env m) v)]
        (swap-value m v'))

      ;; for lists:
      (list? v)
      (if-let [special-form (special-form? v)]
        ;; apply special form if found
        (-> m
            (pop-value) ;; pop the list
            (special-form v))
        ;; otherwise eval each member and apply first to rest
        (-> m
            (pop-value) ;;pop the list

            ;; 2. apply first of list to rest
            (push-instr :apply)
            ;; 1. evaluate members of list
            (push-instr :evalist)
            ;; (evalist expects two arguments, the forms remaining
            ;; to be evaluated, and the forms which have already been
            ;; evaluated)
            (push-value v)
            (push-value [])))

      ;;otherwise, bogus value
      :else (throw (str "unknown value: " v)))))

(defmethod step :evalist [m]
  ;; evalist expects two arguments, the forms remaining
  ;; to be evaluated, and the forms which have already been
  ;; evaluated.
  (let [m    (pop-instr m)  ;; :evalist
        done (top-value m)  ;; the already evaluated values
        m'   (pop-value m) 
        rem  (top-value m') ;; the forms remaining to be evaluated
        m''  (pop-value m')]
    (if (empty? rem)
      ;; if none remain to eval just push the result
      ;; (expects the next instr. to be waiting on the stack when done)
      (-> m'' (push-value done))
      ;; otherwise peel off one form and set up its evaluation
      (let [[next & rem'] rem]
        (-> m''
            ;;3. loop thru remaining
            (push-instr :evalist)
            (push-value rem')
            ;;2. push evaluated result into done
            (push-instr :push)
            (push-value done)
            ;;1. evaluate next result
            (push-instr :eval)
            (push-value next))))))

(defmethod step :push [m]
  ;; conj the top of the value stack onto the value below it
  (let [m (pop-instr m)
        v (top-value m)
        m' (pop-value m)
        vec (top-value m')]
    (swap-value m' (conj vec v))))

(defn special-form-begin [m v]
  (-> m
      (push-value (rest v))
      (push-instr :progn)))
(add-special-form 'begin special-form-begin)

(defmethod step :progn [m]
  (let [[form & rest] (top-value m)
        m (-> m pop-instr pop-value)]
    (if-not rest
      (-> m (push-instr :eval) (push-value form))
      (-> m
          (push-instr :progn)
          (push-value rest)
          (push-instr :discard)
          (push-instr :eval)
          (push-value form)))))

(defmethod step :discard [m]
  (-> m pop-instr pop-value))


(defmethod step :block [m]
  (-> m pop-instr))

(defmethod step :apply [m]
  (let [form        (top-value m)
        [op & args] form
        m           (-> m pop-instr pop-value)]
    ;; [:prim ]
    (if-let [fn (prim-op? op)]
      (let [result (apply fn args)]
        (push-value m result))
      ;; [:special...]
      (if-let [op-fn (special-op? op)]
        (op-fn m op args)
        ;; [:closure...]
        (if (closure? op)
          (let [[_ name closure-env closure-vars body] op
                env (:env m)
                ;; bind for recursion if closure is named
                closed (if name (assoc-in closure-env [:bindings name] op) closure-env)]
            (-> m
                (push-instr [:block name m])
                (push-instr :set-env)
                (push-value env)
                (push-instr :swap)
                (push-instr :progn)
                (push-value body)
                (push-instr :push-env)
                (push-value closure-vars)
                (push-value args)
                (push-instr :set-env)
                (push-value closed)))
          (throw (str "don't know how to apply: "
                      form)))))))

(defmethod step :swap [m]
  (let [m   (pop-instr m)
        a   (top-value m)
        m'  (pop-value m)
        b   (top-value m')
        m'' (pop-value m')]
    (-> m''
        (push-value a)
        (push-value b))))

(defn special-form-let [m v]
  (let [[_ bindings & body] v
        vars (map first bindings)
        forms (map second bindings)]
    (-> m
        (push-instr :pop-env)
        (push-instr :progn)
        (push-value body)
        (push-instr :push-env)
        (push-value vars)
        (push-instr :evalist)
        (push-value forms)
        (push-value []))))
(add-special-form 'let special-form-let)

(defn special-form-lambda [m v]
  (let [[_ vars & body] v]
    (-> m (push-value
           [:closure nil (:env m) vars body]))))
(add-special-form 'lambda special-form-lambda)

(defmethod step :pop-env [m]
  (let [parent (:parent (:env m))]
    (-> m
        (pop-instr)
        (assoc :env parent))))

(defmethod step :push-env [m]
  (let [m    (pop-instr m)
        vals (top-value m)
        m'   (pop-value m)
        vars (top-value m')
        m''  (pop-value m')
        bindings (zipmap vars vals)
        env {:parent (:env m'') :bindings bindings}]
    (assoc m'' :env env)))

(defmethod step :set-env [m]
  (let [env (top-value m)]
    (-> m
        (pop-instr)
        (pop-value)
        (assoc :env env))))

(defn special-form-if [m form]
  (let [[_ test then else] form]
    (-> m
        (push-value [then else])
        (push-value test)
        (push-instr :if)
        (push-instr :eval))))
(add-special-form 'if special-form-if)

(defmethod step :if [m]
  (let [m           (pop-instr m)
        bool        (top-value m)
        m'          (pop-value m)
        [then else] (top-value m')
        m''         (pop-value m')]
    (if bool
      (-> m'' (push-value then) (push-instr :eval))
      (-> m'' (push-value else) (push-instr :eval)))))

(defn special-form-define [m form]
  (if (list? (second form))
    (let [[_ [name & args] & body] form
          env (:env m)
          closure [:closure name env args body]
          env' (assoc-in env [:bindings name] closure)]
      (-> m
          (assoc :env env')
          (push-value closure)))
    (let [[_ name eval] form]
      (-> m
          (push-instr :define)
          (push-value name)
          (push-instr :eval)
          (push-value eval)))))
(add-special-form 'define special-form-define)

(defmethod step :define [m]
  ;; extends the current env w/ sym and val on the stack
  (let [m (pop-instr m)
        val (top-value m)
        m' (pop-value m)
        sym (top-value m')
        m'' (pop-value m')]
    (-> m''
        (assoc-in [:env :bindings sym] val)
        (push-value val))))

;; like let, but with a special reset op provided
;; (with-reset reset ((a 10) (b 20)) ...)
;; this is the looping primitive in the language.
(defn special-form-with-reset [m form]
  (let [[_ reset-name bindings & body] form
        vars  (map first bindings)
        forms (map second bindings)
        ;; the special form for the reset op -- includes the body
        op [:special 'reset reset-name vars m body]
        ;; include the reset op as a bound var
        vars' (conj vars reset-name)
        forms' (conj forms op)
        ]
    (-> m
        (push-instr :pop-env)
        ;; execute the body
        (push-instr :progn)
        (push-value body)
        ;; eval and bind the provided vars
        (push-value vars')
        (push-instr :push-env)
        (push-instr :evalist)
        (push-value forms')
        (push-value []))))
(add-special-form 'with-reset special-form-with-reset)

;; the implementation of the reset op provided by with-reset
(defn special-op-reset [m op args]
  (let [[_ _ reset-name vars machine-state body] op
        vars' (conj vars reset-name)
        args' (conj args op)
        ]
    ;; return to previous machine state
    (-> machine-state
        (push-instr :pop-env)
        ;; execute the body
        (push-instr :progn)
        (push-value body)
        ;; bind the vars to the fresh values
        (push-instr :push-env)
        (push-value vars')
        ;; already eval'd
        (push-value args'))))
(add-special-op 'reset special-op-reset)

(defn special-form-return-from [m form]
  (let [[_ label result-form] form
        block? #(and (vector? %)
                     (= :block (first %))
                     (= label  (second %)))
        blocks (seq (filter block? (:stack m)))]
    (if (= 0 (count blocks))
      (throw (str "no label" label "found"))
      (let [block (last blocks)
            [_ _ state] block]
        (-> state
            (push-value result-form)
            (push-instr :eval))))))
(add-special-form 'return-from special-form-return-from)

(defn check [form expected]
  (.groupCollapsed js/console (str form))
  (println "==============================")
  (println form)
  (println "------------------------------")
  (let [res (evl form) val (top-value res)]
    ;; (println res)
    (println "=>" val)
    (.groupEnd js/console)
    (.assert js/console (= val expected)
             (str "unexpected result:" val
                  " is not equal to expected:" expected))))

(defn run-checks []
  (println "running checks")
  (check 1 1)
  (check "hello" "hello")
  (check '+ [:prim '+])
  (check '(+ 2 2) 4)
  (check '(+ 6 (+ 2 2)) 10)
  (check '(- 6 (+ 2 2)) 2)
  (check '(begin 1 2 3) 3)
  (check '(+ (begin 1 2) (begin 3 4)) 6)
  (check '(let ((x 10)) +) [:prim '+])
  (check '(let ((x 10)) x) 10)
  (check '(let ((x 10) (y 20))
            (+ x y))
         30)
  (check '(let ((x 10) (y 20))
            (let ((x y) (a y))
              (+ x a)))
         40)
  (check '(let ((x 10) (y 20))
            (let ((x y) (a x))
              (+ x a)))
         30)
  (check '(lambda (x) x)
         [:closure nil @root-env '(x) '(x)])
  (check '((lambda (x) x) 10) 10)
  (check '((let ((y 10))
             (lambda () y))) 10)
  (check '(let ((adder (lambda (n) (lambda (x) (+ n x)))))
            (let ((plus4 (adder 4)))
              (plus4 5)))
         9)
  (check 'true true)
  (check 'false false)
  (check '(if true 10 20) 10)
  (check '(if false 10 20) 20)
  (check '(if (begin true) (+ 5 5) (+ 10 10)) 10)
  (check '(if (begin false) (+ 5 5) (+ 10 10)) 20)

  (check '(> 3 4) false)
  (check '(< 3 4) true)

  (check '(define (id x) x) [:closure 'id @root-env '(x) '(x)])

  (check '(begin
           (define (sub x)
             (if (> x 2)
               (sub (- x 1))
               x))
           (sub 5))
         2)

  (check '(begin
           (define x 10)
           x) 10)
  (check '(begin
           (define x 10)
           (let ((x 20))
             x))
         20)
  
  (check '(with-reset jump ((x 10))
            x) 10)

  (check '(with-reset jump ((n 10))
            (if (> n 1)
              (jump (- n 1))
              n)) 1)

  (check
   '(begin
     (define (jumper y)
       (with-reset jump-back ((x y))
         (if (> x 10)
           42
           jump-back)))
     (let ((jump (jumper 5)))
       (trace! "jump =" jump)
       (if (number? jump)
         jump
         (jump 100))))
   42)

  (check
   '(begin
     (define (foo)
       (return-from foo 10)
       11)
     (foo))
   10)

  (check
   '(begin
     (define (foo)
       (return-from foo (+ 3 4))
       11)
     (foo))
   7)

  (check
   '(begin
     (define (bar)
       (return-from foo 12)
       34)
     (define (foo)
       (bar)
       56)
     (foo))
   12)

  (println "all checks run."))

(run-checks)
