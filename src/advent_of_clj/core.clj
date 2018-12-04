(ns advent_of_clj.core
  (:gen-class))

(use '[clojure.string :only (join split-lines)])

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
	(doseq [kv letter_count]
		(= (second kv) n)))
	

(defn calc_checksum [input]
	(loop [[string & remaining] input twos 0 threes 0]
		(if (nil? string)
			(* twos threes)
			(let [letter_count (count_letters_in_string string)]
				(recur remaining (+ (has_value letter_count 2) twos) (+ (has_value letter_count 3) threes))))))

(defn -main
  [& args]
  (def day (subs (first args) 0 1))
  (def input (get_input_vector day))
  (println (case (first args)
	  "1a" (reduce + input)
	  "1b" (find_first_reccuring_freq input 0 (apply list input) #{})
	  "2a" (calc_checksum input))))