;;Solution1

(defn sum-list-set [list1 set1]
  (let [list1 (map #(if (nil? %) 0 %) list1)
        set1 (map #(if (nil? %) 0 %) set1)
        max-len (max (count list1) (count set1))
        extended-list1 (concat list1 (repeat (- max-len (count list1)) 0))
        extended-set1 (concat set1 (repeat (- max-len (count set1)) 0))]
    (mapv + extended-list1 extended-set1)))
(def list1 '(100 200 nil 400 nil 1 nil))
(def set1 '(100 200 300 nil 400 nil 500 600))

(sum-list-set list1 set1)




;;Solution2

(def matrix [[1 1250 3500]
             [2 1400 3600]
             [3 1100 3400]
             [4 1200 3800]])

(defn add-row-sums [matrix]
  (mapv (fn [row]
          (conj row (reduce + row)))
        matrix))

(def matrix-with-sums (add-row-sums matrix))
(println "Matrix with row sums:")
(doseq [row matrix-with-sums]
  (println row))

(defn row-sum [matrix row-index]
  (if (and (>= row-index 0) (< row-index (count matrix)))
    (reduce + (nth matrix row-index))
    -1))

(defn column-sum [matrix col-index]
  (if (and (>= col-index 0) (< col-index (count (first matrix))))
    (reduce + (map #(nth % col-index) matrix))
    -1))

