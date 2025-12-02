(ns advent-of-code.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn input
  "Parse each line of the input for the day using the parse function."
  ([year day]
   (input year day str/trim))
  ([year day parse]
   (with-open [rdr (io/reader (format "input/%d/%d.input" year day))]
     (->> rdr
          line-seq
          (mapv parse)))))



