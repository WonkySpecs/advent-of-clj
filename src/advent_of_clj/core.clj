(ns advent_of_clj.core
  (:gen-class))

(use '[clojure.string :only (split-lines split includes?)])

(def numeric-days
	#{"1"})

(defn get-input-vector [day]
	(def str-vec (split-lines (slurp (str "inputs/"day ".in"))))
	(if (contains? numeric-days day)
		(map read-string str-vec)
		str-vec))

(defn find-first-reccuring-freq [change-vec cur-freq changes seen-freqs]
	(if (empty? changes)
		(find-first-reccuring-freq change-vec cur-freq change-vec seen-freqs)
		(do (def new-freq
				(+ (first changes) cur-freq))
			(if (contains? seen-freqs new-freq)
				new-freq
				(recur change-vec new-freq (drop 1 changes) (conj seen-freqs new-freq))))))


(defn count-letters-in-string
	([s counts]
		;If string empty, return counts
		(if (= (count s) 0)
			counts
			;Otherwise, if first letter is a key in 'counts', increment it's value; otherwise add it value 1
			(do (let [c (subs s 0 1)]
				(let [c-count (get counts c)]
				(count-letters-in-string (subs s 1 (count s))
										 (if c-count
										 	(update counts c inc)
										 	(conj counts [c 1]))))))))
	([s]
		(count-letters-in-string s {})))

(defn has-value [letter-count n]
	(if	(contains? (set (vals letter-count)) n)
		1
		0))
	

(defn calc-checksum [input]
	(loop [[string & remaining] input twos 0 threes 0]
		(if (nil? string)
			(* twos threes)
			(let [letter-count (count-letters-in-string string)]
				(recur remaining (+ (has-value letter-count 2) twos) (+ (has-value letter-count 3) threes))))))

(defn one-letter-difference? [str1 str2]
	(loop [[c1 & remaining1] str1 [c2 & remaining2] str2 diff-index -1 cur-index 0]
		(if (nil? c1)
			(if (= diff-index -1)
				false
				diff-index)
			(if (and (not (= c1 c2)) (not (= diff-index -1)))
				false
				(recur remaining1 remaining2 (if (= c1 c2)
												 diff-index
												 cur-index)
											(inc cur-index))))))

(defn subs-without [s i]
	(str (subs s 0 i) (subs s (inc i))))

(defn find-almost-matching-boxes [box-vector]
	(loop [[box1 & boxes-to-compare] box-vector]
		(let [single-letter-diff (loop [[box2 & remaining] boxes-to-compare]
			(if (nil? box2)
				false
				(let [diff (one-letter-difference? box1 box2)]
				(if diff
					diff
					(recur remaining)))))]
			(if single-letter-diff
				(subs-without box1 single-letter-diff)
				(recur boxes-to-compare)))))

(defn parse-claims [input]
	(map
	 (fn parse-claim [claim]
		(let [[- - pos size] (split claim #"\s")]
			(let [[x y] (split (clojure.string/replace (clojure.string/replace pos #"," " ") #":" "") #"\s")]
				{:pos [x y] :size (split size #"x")})))
	 input))

(defn calc-claimed-points [claim]
	(let [[w h] (map read-string (:size claim))]
		(for [x (range w) y (range h)]
			(let [[x-start y-start] (map read-string (:pos claim))]
			[(+ x x-start) (+ y y-start)]))))

(defn calc-conflicts [existing-claimed-points new-claimed-points]
	(defn point-conflict? [new-p]
		(if (contains? existing-claimed-points new-p)
			true
			false))
	(into existing-claimed-points 
		(into {} (map (fn [p] [p (point-conflict? p)]) new-claimed-points))))

(defn calc-conflict-map [claims]
	(loop [[claim & remaining] claims claimed-points {}]
		(let [points-claimed (calc-claimed-points claim)]
			(let [new-claimed-points (calc-conflicts claimed-points points-claimed)]
				(if (nil? remaining)
					new-claimed-points
					(recur remaining new-claimed-points))))))

(defn count-true-values [claimed-points]
	(reduce + (map (fn [v] (if v 1 0)) (vals claimed-points))))

(defn num-conflicting-points [claims]
	(count-true-values (calc-conflict-map claims)))

(defn non-conflicting-claim? [claim conflicts]
	(loop [[point & rem-points] (calc-claimed-points claim)]
		(if (nil? point)
			true
			(if (get conflicts point)
				false
				(recur rem-points)))))

(defn find-non-conflicting-claim [claims]
	(let [conflicts (calc-conflict-map claims)]
		(loop [[claim & rem-claims] claims]
			(if (nil? claim)
				"No non-conflicting claim found"
				(if (non-conflicting-claim? claim conflicts)
					claim
					(recur rem-claims))))))

(defn extract_datetime [s]
	(subs (first (split s #"]")) 1))

(defn minutes-between [time-str-1 time-str-2]
	)

(defn parse-guard-log [log-line]
	(defn guard-num [line]
		(first (split (second (split line #"#")) #"\s")))
	[(cond 
		(includes? log-line "#") (guard-num log-line)
		(includes? log-line "wakes up") "wake"
		(includes? log-line "falls asleep") "sleep")
	 (extract_datetime log-line)])

(defn calc-guard-minute [input]
	(take 10 (map parse-guard-log (sort-by identity input))))

(defn -main
  [& args]
  (def day (subs (first args) 0 1))
  (def input (get-input-vector day))
  (println 
  	(case (first args)
	  "1a" (reduce + input)
	  "1b" (find-first-reccuring-freq input 0 (apply list input) #{})
	  "2a" (calc-checksum input)
	  "2b" (find-almost-matching-boxes input)
	  "3a" (num-conflicting-points (parse-claims input))
	  "3b" (find-non-conflicting-claim (parse-claims input))
	  "4a" (calc-guard-minute input))))