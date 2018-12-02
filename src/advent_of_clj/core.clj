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

(defn -main
  [& args]
  (def day (subs (first args) 0 1))
  (def input (get_input_vector day))
  (println (case (first args)
	  "1a" (reduce + input)
	  "1b" (find_first_reccuring_freq input 0 (apply list input) #{}))))