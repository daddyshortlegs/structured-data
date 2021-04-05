(ns structured-data)

(defn do-a-thing [x]
  (let [exp (+ x x)]
    (Math/pow exp exp))
  )

(defn spiff [v]
  (let [item1 (get v 0)
        item2 (get v 2)]
    (+ item1 (if (= nil item2) 0 item2))
    )
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[item1 _ item2] v]
    (+ item1 (if (= nil item2) 0 item2))
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[point1 point2] rectangle
        [x1 _] point1
        [x2 _] point2]
    (- x2 x1)))

(defn height [rectangle]
  (let [[point1 point2] rectangle
        [_ y1] point1
        [_ y2] point2]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[point1 point2] rectangle
        [x1 y1] point1
        [x2 y2] point2
        [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-item (fn [x] (get x 1))]
    (map second-item collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [new-set (set a-seq)]
    (not (= (count a-seq) (count new-set)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn[x] (:authors x)) books)))

(defn all-author-names [books]
  (set (map (fn[item] (:name item)) (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years (if (= nil birth-year) "" (str " (" birth-year " - " death-year ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map (fn[author] (author->string author)) authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [num-books (count books)
        prefix (cond
                 (= 1 num-books) "1 book."
                 :else (str num-books " books."))]
    (if (= 0 num-books)
      "No books."
      (str prefix " " (apply str (interpose ". " (map (fn[book] (book->string book)) books))) "."))
    ))

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
