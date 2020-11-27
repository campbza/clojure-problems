(ns clojure-problems.core)

(defn last-element
  "
  Problem 19: Write a function which returns the last element in a sequence.
  http://www.4clojure.com/problem/19
  "
  [[x & xs]]
  (if xs (last-element xs) x))

(defn penultimate-element
  "
  Problem 20: Write a function which returns the second to last element
  from a sequence.
  http://www.4clojure.com/problem/20
  "
  [[x y & xs]]
  (if xs (recur (cons y xs)) x))

(defn nth-element
  "
  Problem 21: Write a function which returns the Nth element from a sequence.
  http://www.4clojure.com/problem/21
  "
  [xs n]
  ((zipmap (range (count xs)) xs) n))

(defn my-count
  "
  Problem 22: Write a function which returns the total number of elements
  in a sequence.
  http://www.4clojure.com/problem/22
  "
  [xs]
  (reduce (fn [acc _] (inc acc)) 0 xs))

(defn rotate-seq [n xs])

(defn flipping-out
  "
  Problem 46: Write a higher-order function which flips the order of the
  arguments of an input function.
  http://www.4clojure.com/problem/46
  "
  [f]
  (fn [x y] (f y x)))

(defn my-partition
  "
  Problem 54: Write a function which returns a sequence of lists of x items
  each. Lists of less than x items should not be returned.
  http://www.4clojure.com/problem/54
  "
  [n ls]
  (when (<= n (count ls))
    (cons  (take n ls) (my-partition n (drop n ls)))))

(defn my-frequencies
  "
  Problem 55: Write a function which returns a map containing the number of
  occurences of each distinct item in a sequence.
  http://www.4clojure.com/problem/55
  "
  [coll]
  (reduce (fn [acc v]
            (if (acc v)
              (update-in acc [v] inc)
              (assoc acc v 1)))
          {}
          coll))

(defn my-distinct
  "
  Problem 56: Write a function which removes the duplicates from a sequence.
  Order of the items must be maintained.
  http://www.4clojure.com/problem/56
  "
  [coll]
  (:ret (reduce (fn [acc v]
                  (if ((:visited acc) v)
                    acc
                    (-> acc
                        ;; Why does 4Clojure not know `update`..?
                        (update-in [:ret] conj v)
                        (update-in [:visited] conj v))))
                {:ret [] :visited #{}}
                coll)))

(defn my-comp
  "
  Problem 58: Write a function which allows you to create function compositions.
  The parameter list should take a variable number of functions, and create a
  function that applies them from right-to-left.
  http://www.4clojure.com/problem/58
  "
  [& fns]
  (fn [& args]
    (reduce (fn [acc f] (f acc))
            (apply (first (reverse fns)) args)
            (rest (reverse fns)))))

(defn my-juxt
  "
  Problem 59: Take a set of functions and return a new function that takes a
  variable number of arguments and returns a sequence containing the result of
  applying each function left-to-right to the argument list.
  http://www.4clojure.com/problem/59
  "
  [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(defn my-reductions
  "
  Problem 60: Write a function which behaves like reduce, but returns each
  intermediate value of the reduction. Your function must accept either two or
  three arguments, and the return sequence must be lazy.
  "
  ([f coll]
   (my-reductions f (first coll) (rest coll)))
  ([f prev coll]
   (if (seq (rest coll))
     (lazy-seq (cons prev (my-reductions f (f prev (first coll)) (rest coll))))
     (list prev (f prev (first coll))))))

(defn make-map
  "
  Problem 61: Write a function which takes a vector of keys and a vector of
  values and constructs a map from them.
  http://www.4clojure.com/problem/61
  "
  [ks vs]
  (apply merge (map #(assoc {} %1 %2) ks vs)))

(defn my-iterate
  "
  Problem 62: Given a side-effect free function f and an initial value x write a
  function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f
  (f x))), etc.
  http://www.4clojure.com/problem/62
  "
  [f x]
  (lazy-seq (cons x (my-iterate f (f x)))))

(defn my-group-by
  "
  Problem 63: Given a function f and a sequence s, write a function which
  returns a map. The keys should be the values of f applied to each item in s.
  The value at each key should be a vector of corresponding items in the order
  they appear in s.
  http://www.4clojure.com/problem/63
  "
  [f coll]
  (reduce (fn [acc v]
            (let [k (f v)]
              (if (acc k)
                ;; Weird, 4Clojure doesn't recognize `update`
                ;; but does recognize `update-in`..
                (update-in acc [k] concat [v])
                (assoc acc k [v]))))
          {}
          coll))

(defn my-merge-with
  "
  Problem 69: Write a function which takes a function f and a variable number of
  maps. Your function should return a map that consists of the rest of the maps
  conj-ed onto the first. If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) should be combined with the mapping in the
  result by calling (f val-in-result val-in-latter).
  http://www.4clojure.com/problem/69
  "
  [f m0 & ms]
  (reduce (fn [acc m]
            (merge acc
                   (reduce (fn [acc1 [k v]]
                             (if (acc k)
                               (assoc acc1 k (f (acc k) v))
                               (assoc acc1 k v)))
                           {}
                           m)))
          m0
          ms))

(defn insert-between
  "
  Problem 132: Write a function that takes a two-argument predicate, a value,
  and a collection; and returns a new collection where the value is inserted
  between every two items that satisfy the predicate.
  http://www.4clojure.com/problem/132
  "
  [pred v coll]
  (->> coll
       (partition 2 1 nil)
       (map (fn [[x y]]
              (cond
                (nil? y) (list x)
                (pred x y)  (list x v)
                :else (list x))))
       (apply concat)))

(defn decurry
  "
  Problem 158: Write a function that accepts a curried function of unknown arity
  n. Return an equivalent function of n arguments.
  http://www.4clojure.com/problem/158
  "
  [f]
  (fn [& args]
    (reduce (fn [acc v] (acc v))
            (f (first args))
            (rest args))))
