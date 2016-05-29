(ns looping-is-recursion)
(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn power [base exp]
  (let [helper (fn [acc b n]
                 (if (zero? n)
                   acc
                   (recur (* acc b ) b (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [x xs]
                 (if (empty? xs)
                   x
                   (recur (first xs ) (rest xs))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [b xs ys]
                 (if (and (empty? xs) (empty? ys))
                   b
                   (recur (and b (=(first xs) (first ys)) ) (rest xs) (rest ys) )))]
    (helper (=(count seq1) (count seq2)) seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (cond
   (empty? s)
     nil
   (pred (first s))
     acc
   :else
     (recur (+ 1 acc) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         len (count a-seq)
         s a-seq]
    (if (empty? s)
      (/ acc len)
      (recur (+ acc (first s)) len (rest s))
    )))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s)
      acc
      (recur (toggle acc (first s)) (rest s))
    )))

(defn fast-fibo [n]
  (loop [f 0
         ff 1
         nn n]
    (if (= nn 0)
      f
      (recur  ff (+ f ff)  (dec nn))
    )))

(defn cut-at-repetition [a-seq]
 (loop [acc []
        ts #{}
         s a-seq]
    (if (or (empty? s)(contains? ts (first s)) )
       acc
      (recur (conj acc (first s)) (conj ts (first s)) (rest s))
    )))

