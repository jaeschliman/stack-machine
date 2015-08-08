(ns ^:figwheel-always stack-machine.core
    (:require[om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]))

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


(def root-env (atom {}))
(def prims {'+ + '- -})
(defn add-primitive-fn [sym]
  (reset! root-env (assoc @root-env sym [:prim sym])))
(add-primitive-fn '+)
(add-primitive-fn '-)

(defn prim-op? [it]
  (and (vector? it)
       (= :prim (first it))
       (get prims (second it))))

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

(defmulti step (fn [m] (last (:stack m))))

(defmethod step nil [m] m)
(defmethod step :eval [m]
  (let [v (top-value m)
        m (-> m pop-instr)]
    (cond
      (number? v) m
      (symbol? v)
      (let [v' (get-in m [:env v])]
        (println (str "replacing " v " with " v'))
        (swap-value m v'))
      (list?   v)
      (-> m
          (push-instr :evalist)
          (pop-value)
          (push-value v)
          (push-value []))
      :else (throw "unknown value"))))

(defmethod step :evalist [m]
  (let [m    (pop-instr m)
        done (top-value m)
        m'   (pop-value m)
        rem  (top-value m')
        m''  (pop-value m')
        ]
    (if (empty? rem)
      (-> m'' (push-instr :apply) (push-value done))
      (let [[next & rem'] rem]
        (-> m''
            (push-instr :evalist)
            (push-value rem')
            (push-instr :push)
            (push-value done)
            (push-instr :eval)
            (push-value next))))))

(defmethod step :push [m]
  (let [m (pop-instr m)
        v (top-value m)
        m' (pop-value m)
        vec (top-value m')]
    (swap-value m' (conj vec v))))

(defmethod step :apply [m] m
  (let [[op & args] (top-value m)
        m (-> m pop-instr pop-value)]
    (if-let [fn (prim-op? op)]
      (let [result (apply fn args)]
        (push-value m result))
      (throw "don't know how to apply"))))

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

(check 1 1)
(check '+ [:prim '+])
(check '(+ 2 2) 4)
(check '(+ 6 (+ 2 2)) 10)
(check '(- 6 (+ 2 2)) 2)
