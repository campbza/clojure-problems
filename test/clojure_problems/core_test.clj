(ns clojure-problems.core-test
  (:require [clojure.test :refer :all]
            [clojure-problems.core :refer :all]))

;; Problem 19
(deftest last-element-test
  (is (= 5 (last-element [1 2 3 4 5])))
  (is (= 3 (last-element '(5 4 3))))
  (is (= "d" (last-element ["b" "c" "d"])))
  (is (= nil (last-element []))))

;; Problem 20
(deftest penultimate-element-test
  (is (= 4 (penultimate-element (list 1 2 3 4 5))))
  (is (= "b" (penultimate-element ["a" "b" "c"])))
  (is (= [1 2] (penultimate-element [[1 2] [3 4]])))
  (is (= nil (penultimate-element []))))

;; Problem 21
(deftest nth-element-test
  (is (= 6 (nth-element '(4 5 6 7) 2)))
  (is (= :a (nth-element [:a :b :c] 0)))
  (is (= 2 (nth-element [1 2 3 4] 1)))
  (is (= [5 6] (nth-element '([1 2] [3 4] [5 6]) 2))))

;; Problem 22
(deftest my-count-test
  (is (= 5 (my-count '(1 2 3 3 1))))
  (is (= 11 (my-count "Hello World")))
  (is (= 3 (my-count [[1 2] [3 4] [5 6]])))
  (is (= 1 (my-count '(13))))
  (is (= 3 (my-count '(:a :b :c)))))

;; Problem 46
(deftest flipping-out-test
  (is (= 3 ((flipping-out nth) 2 [1 2 3 4 5])))
  (is (= true ((flipping-out >) 7 8)))
  (is (= 4 ((flipping-out quot) 2 8)))
  (is (= [1 2 3] ((flipping-out take) [1 2 3 4 5] 3))))

;; Problem 54
(deftest my-partition-test
  (is (= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (is (= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (is (= (my-partition 3 (range 8)) '((0 1 2) (3 4 5)))))

;; Problem 55
(deftest my-frequencies-test
  (is (= (my-frequencies [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (is (= (my-frequencies [:b :a :b :a :b]) {:a 2, :b 3}))
  (is (= (my-frequencies '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

;; Problem 56
(deftest my-distinct-test
  (is (= (my-distinct [1 2 1 3 1 2 4]) [1 2 3 4]))
  (is (= (my-distinct [:a :a :b :b :c :c]) [:a :b :c]))
  (is (= (my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (is (= (my-distinct (range 50)) (range 50))))

;; Problem 58
(deftest my-comp-test
  (is (= [3 2 1] ((my-comp rest reverse) [1 2 3 4])))
  (is (= 5 ((my-comp (partial + 3) second) [1 2 3 4])))
  (is (= true ((my-comp zero? #(mod % 8) +) 3 5 7 9)))
  (is (= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;; Problem 59
(deftest my-juxt-test
  (is (= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4)))
  (is (= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello")))
  (is (= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

;; Problem 60
(deftest my-reductions-test
  (is (= [0 1 3 6 10] (take 5 (my-reductions + (range)))))
  (is (= [[1] [1 2] [1 2 3] [1 2 3 4]] (my-reductions conj [1] [2 3 4])))
  (is (= (reduce * 2 [3 4 5]) 120) (last (my-reductions * 2 [3 4 5]))))

;; Problem 61
(deftest make-map-test
  (is (= (make-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (make-map [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (make-map [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

;; Problem 62
(deftest my-iterate-test
  (is (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (my-iterate inc 0)) (take 100 (range))))
  (is (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

;; Problem 63
(deftest my-group-by-test
  (is (= (my-group-by #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
         {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
         {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

;; Problem 69
(deftest my-merge-with-test
  (is (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
         {:a 4, :b 6, :c 20}))
  (is (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
         {1 7, 2 10, 3 15}))
  (is (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
         {:a [3 4 5], :b [6 7], :c [8 9]})))

;; Problem 132
(deftest insert-between-test
  (is (= '(1 :less 6 :less 7 4 3) (insert-between < :less [1 6 7 4 3])))
  (is (= '(2) (insert-between > :more [2])))
  (is (= [0 1 :x 2 :x 3 :x 4]  (insert-between #(and (pos? %) (< % %2)) :x (range 5))))
  (is (empty? (insert-between > :more ())))
  (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
         (take 12 (->> [0 1]
                       (iterate (fn [[a b]] [b (+ a b)]))
                       (map first)             ; fibonacci numbers
                       (insert-between (fn [a b] ; both even or both odd
                                         (= (mod a 2) (mod b 2)))
                                       :same))))))

;; Problem 158
(deftest decurry-test
  (is (= 10 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (+ a b c d))))))
             1 2 3 4)))
  (is (= 24 ((decurry (fn [a]
                        (fn [b]
                          (fn [c]
                            (fn [d]
                              (* a b c d))))))
             1 2 3 4)))
  (is (= 25 ((decurry (fn [a]
                        (fn [b]
                          (* a b))))
             5 5))))
