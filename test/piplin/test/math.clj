(ns piplin.test.math
  (:use clojure.test)
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not])
  (:use [piplin types math modules sim connect protocols])
  (:use [piplin.types bits boolean enum numbers core-impl binops uintm])
  (:import clojure.lang.ExceptionInfo))

(deftest j-long-test
  (is (= 3 (+ 1 2)))
  (is (= 6 (* 2 3)))
  (is (thrown? ExceptionInfo (promote :j-long "lx"))))

(deftest bit-width-of-non-type-test
  (is (thrown? ExceptionInfo (bit-width-of ((uintm 8) 0)))))

(deftest float-test
  (is (= 3.5 (+ 1.5 2))))

(deftest =test
  (is (= 1 1 1))
  (is (not (= 1 1 2)))
  (is (= "hello" "hello"))
  (is (= [1 \a] [1 \a]))
  (is (< 3 7))
  (is (> 0 -1.2))
  (is (>= -3.3 -3.3))
  (is (not (<= 4 1)))
  (is (not= 1 2))
  (is (not (not= 0 0))))

(deftest uintm-test
  (letfn [(um8 [v] (instance (uintm 8) v))]
    (is (= (+ (um8 200) 1) (um8 201)))
    (is (= (+ (um8 200) 60) (um8 4)))
    (is (= (+ 1 2 3 (um8 4)) (um8 10)))
    (is (= (instance (uintm 8) -1 :constrain) (um8 255)))
    (is (= (instance (uintm 8) 256 :constrain) (um8 0)))
    (is (thrown? ExceptionInfo (promote (uintm 3) "lx")))
    (is (thrown? ExceptionInfo (promote (uintm 3) -1)))
    (is (thrown? ExceptionInfo (promote (uintm 3) 100)))
    (is (= (- 0 (um8 1)) (um8 255)))))

(deftest binop-error-test
  (let [e (error "hi")]
    (is (= (try-errors (+ 0 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 2 3 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 (throw+ e) 3 (throw+ e))) e))
    (is (= (try-errors (+ (throw+ e) 0)) e))
    (is (= (try-errors (+ (throw+ e) (throw+ e))) e))
    (is (= (try-errors (bit-cat (throw+ e) (throw+ e))) e))))

(deftest bits-test
  (let [b8 #(promote (bits 8) %)]
    (is (= (value (b8 0xa0)) [1 0 1 0 0 0 0 0]))
    (is (= (value (bit-cat (b8 0xa0) (bit-cat (b8 0xfe))))
           [1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0]))
    (is (= (value (bit-slice (bit-cat (b8 0xf) (b8 0xf0)) 4 12))
           [1 1 1 1 1 1 1 1]))
    (is (= (value ((make-sim-fn (bit-slice (bit-cat (uninst (b8 0xf)) (b8 0xf0)) 4 12))))
           [1 1 1 1 1 1 1 1]))
    (is (= (bit-and (b8 0xf) 0x3c) (b8 0xc)))
    (is (= (bit-or 0xf0 (b8 0xf)) (b8 0xff)))
    (is (= (bit-xor (instance (uintm 8) 0x3c) 0xf)
           ((uintm 8) 0x33))))
  (is (= (get-bits true) [1]))
  (is (= (get-bits false) [0]))
  (is (= (serialize true) (cast (bits 1) 1)))
  (is (= (serialize false) (cast (bits 1) 0)))
  (is (= (count (value (serialize
                         (promote (enum #{:a :b}) :a))))
         1)) 
  (is (= (count (value (serialize
                         (promote (enum #{:a :b :c}) :a))))
         2)))  

(deftest sim-uintm-bits-test
  (let [mod (module [:outputs [c (promote (uintm 8) 0)
                               d (promote (bits 4) 0)]]
                    (connect c (+ c 1))
                    (connect d (bit-slice (serialize c) 0 4)))
        sim (make-sim mod)
        init-state (first sim)
        init-fns (second sim)]
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [:c])
           (instance (uintm 8) 10)))
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [:d])
           (instance (bits 4) [1 0 0 1])))
    (is (= (get (exec-sim init-state
                          init-fns
                          18)
                [:d])
           (instance (bits 4) [0 0 0 1])))))

(deftest sim-cast-test
  (let [mod (module [:outputs [odd false
                               c (promote (uintm 3) 0)]]
                    (let [new-c (inc c)]
                      (connect odd (cast (anontype :boolean) 
                                         (bit-slice (serialize new-c) 0 1)))
                      (connect c new-c)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 1)
                [:c])
           (promote (uintm 3) 1)))

    (is (= (get (exec-sim state fns 1)
                [:odd])
           true))
    (is (= (get (exec-sim state fns 2)
                [:odd])
           false))
    (is (= (get (exec-sim state fns 3)
                [:odd])
           true))

    (is (= (get (exec-sim state fns 30)
                [:c])
           (instance (uintm 3) 30 :constrain)))
    (is (= (get (exec-sim state fns 30)
                [:odd])
           false))))

(deftest sim-equals-test
  (let [mod (module [:outputs [triggered false]
                     :feedback [c ((uintm 2) 0)]]
                    (let [c' (inc c)]
                      (connect c c')
                      (connect triggered (= c' ((uintm 2) 3)))))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 1)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 2)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 3)
                [:triggered])
           true))
    (is (= (get (exec-sim state fns 4)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 5)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 6)
                [:triggered])
           false))
    (is (= (get (exec-sim state fns 7)
                [:triggered])
           true))))

(deftest enum-initialize-test
  (let [e (enum #{:a :b :c})]
    (enum (:keymap e)))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 2) 0)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      2 (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 3) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a 0
                      :b (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum #{:a 1 "a"}))))
