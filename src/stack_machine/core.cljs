(ns ^:figwheel-always stack-machine.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
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


(def root-env (atom {:bindings {} :parent nil}))
(def prims {'+ + '- -})
(defn add-primitive-fn [sym]
  (reset! root-env (assoc-in @root-env [:bindings sym] [:prim sym])))
(add-primitive-fn '+)
(add-primitive-fn '-)
(def special-forms (atom {}))
(defn add-special-form [sym fn]
  (reset! special-forms (assoc @special-forms sym fn)))

;; tests the value before application
(defn prim-op? [it]
  (and (vector? it)
       (= :prim (first it))
       (get prims (second it))))


;; tests the form before evaluation
(defn special-form? [form]
  (and (list? form)
       (get @special-forms (first form))))

(defn lookup-symbol [env sym]
  (if (contains? (:bindings env) sym)
    (get-in env [:bindings sym])
    (if-let [parent (:parent env)]
      (lookup-symbol parent sym))))

(defn machine []
  { :stack [] :env @root-env :value-stack []})
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

(def push-instr (pusher :stack))
(def push-value (pusher :value-stack))
(def swap-instr (swapper :stack))
(def swap-value (swapper :value-stack))
(def pop-instr  (popper :stack))
(def pop-value  (popper :value-stack))
(def top-instr  (topper :stack))
(def top-value  (topper :value-stack))

(declare step)
(defn evl [form]
  (let [m (-> (machine)
              (push-value form)
              (push-instr :eval))]
    (loop [state m]
      (let [next (step state)]
        (if (= next state) state
            (recur next))))))

(defn print-machine [m]
  (println "--------------------")
  (println "  control stack:" (:stack m))
  (println "  value   stack:" (:value-stack m))
  (println "  environment:  " (:env m)))

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

(defmulti step (fn [m] (last (:stack m))))

(defmethod step nil [m] m)
(defmethod step :eval [m]
  (let [v (top-value m)
        m (-> m pop-instr)]
    (cond
      (number? v) m
      (symbol? v)
      (let [v' (lookup-symbol (:env m) v)]
        (println (str "replacing " v " with " v'))
        (swap-value m v'))
      (list?   v)
      (if-let [special-form (special-form? v)]
        (special-form (pop-value m) v)
        (-> m
            (pop-value)
            (push-instr :apply)
            (push-instr :evalist)
            (push-value v)
            (push-value [])))
      :else (throw "unknown value"))))

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

(defmethod step :evalist [m]
  ;; [(forms) [empty vec]] -- (evaluated forms)
  ;; i.e the top of the stack is used to store the forms as they're
  ;; eval'ed
  (let [m    (pop-instr m)
        done (top-value m)
        m'   (pop-value m)
        rem  (top-value m')
        m''  (pop-value m')]
    (if (empty? rem)
      ;; expects the next instr. to be waiting on the stack when done
      (-> m'' (push-value done))
      (let [[next & rem'] rem]
        (-> m''
            ;;loop thru remaining
            (push-instr :evalist)
            (push-value rem')
            ;;push evaluated result into done
            (push-instr :push)
            (push-value done)
            ;;evaluate next result
            (push-instr :eval)
            (push-value next))))))

(defmethod step :push [m]
  (let [m (pop-instr m)
        v (top-value m)
        m' (pop-value m)
        vec (top-value m')]
    (swap-value m' (conj vec v))))

(defmethod step :apply [m]
  (let [[op & args] (top-value m)
        m (-> m pop-instr pop-value)]
    (if-let [fn (prim-op? op)]
      (let [result (apply fn args)]
        (push-value m result))
      (throw "don't know how to apply"))))

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


(defn check [form expected]
  (.groupCollapsed js/console (str form))
  (println "==============================")
  (println form)
  (println "------------------------------")
  (let [res (evl form) val (top-value res)]
    (println res)
    (println "=>" val)
    (.groupEnd js/console)
    (.assert js/console (= val expected)
             (str "unexpected result:" val " is not equal to expected:" expected))))

(defn run-checks []
  (println "running checks")
  (check 1 1)
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
  (println "all checks run."))

(run-checks)
