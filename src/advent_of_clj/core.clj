(ns advent_of_clj.core
  (:gen-class))

(use '[clojure.string :only (split-lines split replace)])

(def numeric_days
	#{"1"})

(defn get_input_vector [day]
	(def str_vec (split-lines (slurp (str day ".in"))))
	(if (contains? numeric_days day)
		(map read-string str_vec)
		str_vec))

(defn find_first_reccuring_freq [change_vec cur_freq changes seen_freqs]
	(if (empty? changes)
		(find_first_reccuring_freq change_vec cur_freq change_vec seen_freqs)
		(do (def new_freq
				(+ (first changes) cur_freq))
			(if (contains? seen_freqs new_freq)
				new_freq
				(recur change_vec new_freq (drop 1 changes) (conj seen_freqs new_freq))))))


(defn count_letters_in_string
	([s counts]
		;If string empty, return counts
		(if (= (count s) 0)
			counts
			;Otherwise, if first letter is a key in 'counts', increment it's value; otherwise add it value 1
			(do (let [c (subs s 0 1)]
				(let [c_count (get counts c)]
				(count_letters_in_string (subs s 1 (count s))
										 (if c_count
										 	(update counts c inc)
										 	(conj counts [c 1]))))))))
	([s]
		(count_letters_in_string s {})))

(defn has_value [letter_count n]
	(if	(contains? (set (vals letter_count)) n)
		1
		0))
	

(defn calc_checksum [input]
	(loop [[string & remaining] input twos 0 threes 0]
		(if (nil? string)
			(* twos threes)
			(let [letter_count (count_letters_in_string string)]
				(recur remaining (+ (has_value letter_count 2) twos) (+ (has_value letter_count 3) threes))))))

(defn one_letter_difference? [str1 str2]
	(loop [[c1 & remaining1] str1 [c2 & remaining2] str2 diff_index -1 cur_index 0]
		(if (nil? c1)
			(if (= diff_index -1)
				false
				diff_index)
			(if (and (not (= c1 c2)) (not (= diff_index -1)))
				false
				(recur remaining1 remaining2 (if (= c1 c2)
												 diff_index
												 cur_index)
											(inc cur_index))))))

(defn subs_without [s i]
	(str (subs s 0 i) (subs s (inc i))))

(defn find_almost_matching_boxes [box_vector]
	(loop [[box1 & boxes_to_compare] box_vector]
		(let [single_letter_diff (loop [[box2 & remaining] boxes_to_compare]
			(if (nil? box2)
				false
				(let [diff (one_letter_difference? box1 box2)]
				(if diff
					diff
					(recur remaining)))))]
			(if single_letter_diff
				(subs_without box1 single_letter_diff)
				(recur boxes_to_compare)))))

(defn parse_claims [input]
	(map
	 (fn parse_claim [claim]
		(let [[_ _ pos size] (split claim #"\s")]
			(let [[x y] (split (replace (replace pos #"," " ") #":" "") #"\s")]
				{:pos [x y] :size (split size #"x")})))
	 input))

(defn claimed_points [claim]
	(let [[w h] (map read-string (:size claim))]
		(for [x (range w) y (range h)]
			(let [[x_start y_start] (map read-string (:pos claim))]
			[(+ x x_start) (+ y y_start)]))))

(defn num_conflicting_points [claims]
	(claimed_points (first claims)))

(defn -main
  [& args]
  (def day (subs (first args) 0 1))
  (def input (get_input_vector day))
  (println 
  	(case (first args)
	  "1a" (reduce + input)
	  "1b" (find_first_reccuring_freq input 0 (apply list input) #{})
	  "2a" (calc_checksum input)
	  "2b" (find_almost_matching_boxes input)
	  "3a" (num_conflicting_points (parse_claims input)))))