(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                   (if (zero? exp)
                     acc
                     (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [[x & xs]]
                   (if (empty? xs)
                     x
                     (recur xs)))]
    (if-not (empty? a-seq)
      (helper a-seq))))

(defn seq= [xs ys]
  (cond
    (and (empty? xs) (not-empty ys)) false
    (and (not-empty xs) (empty? ys)) false
    (and (empty? xs) (empty? ys)) true
    :else (if (= (first xs) (first ys))
            (seq= (rest xs) (rest ys))
            false)))

(defn find-first-index [pred a-seq]
  (let [helper (fn [i pred xs]
                   (if-not (empty? xs)
                     (if (pred (first xs))
                       i
                       (recur (inc i) pred (rest xs)))))]
    (helper 0 pred a-seq)))

(defn avg [a-seq]
  (let [helper (fn [total cnt a-seq]
                   (if (empty? a-seq)
                     (/ total cnt) ; TODO: zero check
                     (recur (+ total (first a-seq)) (inc cnt) (rest a-seq))))]
    (helper 0 0 a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [a-set x]
                 (if (contains? a-set x)
                   (disj a-set x)
                   (conj a-set x)))
        helper (fn [a-set a-seq]
                 (if (empty? a-seq)
                   a-set
                   (recur (toggle a-set (first a-seq)) (rest a-seq))))]
    (helper #{} a-seq)))

(defn fast-fibo [n]
  (loop [n' n
         a 0
         b 1]
    (if (zero? n')
      a
      (recur (dec n') b (+ a b)))))

(defn cut-at-repetition [a-seq]
  [":("])

(defn cut-at-repetition [xs]
  (let [f (fn [[x' & xr' :as xs'] [y' & yr' :as ys']]
            (if (empty? ys')
              xs'
              (if (and (not-empty xs') (= x' y'))
                xs'
                (recur (conj xs' y') yr'))))]
    (f [] xs)))
