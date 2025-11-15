#!/usr/bin/env clojure
;;; Script to convert .techne CommonLisp test files to Clojure

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn convert-line [line]
  "Convert a single line from CommonLisp to Clojure syntax"
  (cond
    ;; Comment lines stay the same
    (str/starts-with? line ";;") line

    ;; Skip load statements
    (str/includes? line "(load ") ""

    ;; Convert defvar to def
    (str/includes? line "(defvar *rekb*")
    "(def rekb (create-rekb \"Random rekb\"))"

    ;; Convert setf with node declarations - just mark for later processing
    (str/starts-with? (str/trim line) "(setf") :start-setf

    ;; End of setf block
    (and (str/starts-with? (str/trim line) ")")
         (not (str/includes? line "assrt-formula")))
    :end-setf

    ;; Node declarations within setf
    (re-find #"node-\d+.*declare-atomic" line)
    (let [match (re-find #"(node-\d+) \(declare-atomic nil \"(.+?)\" :(\w+) \*rekb\*\)" line)]
      (when match
        (str "(def " (nth match 1) " (declare-atomic nil \"" (nth match 2) "\" :" (nth match 3) " rekb))")))

    ;; Convert assrt-formula calls
    (str/includes? line "assrt-formula")
    (-> line
        (str/replace "(assrt-formula " "(assrt-formula ")
        (str/replace "(list " "[")
        (str/replace "*rekb*)" "rekb)")
        (str/replace " *rekb*)" " rekb)")
        (str/replace #"\)(?=\s*;)" "]")
        (str/replace #"\)\s*$" "")
        (str/replace #"\s+$" ""))

    ;; Empty lines
    (str/blank? line) ""

    ;; Default: return as-is
    :else line))

(defn process-setf-block [lines]
  "Process the setf block to extract node definitions"
  (let [node-lines (filter #(re-find #"node-\d+" %) lines)]
    (map (fn [line]
           (let [match (re-find #"(node-\d+) \(declare-atomic nil \"(.+?)\" :(\w+) \*rekb\*\)" line)]
             (when match
               (str "(def " (nth match 1) " (declare-atomic nil \"" (nth match 2) "\" :" (nth match 3) " rekb))"))))
         node-lines)))

(defn convert-file [input-path output-path]
  "Convert a .techne file to Clojure"
  (println (str "Converting " input-path " to " output-path))
  (let [lines (str/split-lines (slurp input-path))
        in-setf (atom false)
        setf-lines (atom [])]
    (with-open [writer (io/writer output-path)]
      ;; Write header
      (.write writer ";; ")
      (.write writer (first lines))
      (.write writer "\n")
      (.write writer "(require '[techne-psm :refer :all])\n\n")
      (.write writer "(def rekb (create-rekb \"Random rekb\"))\n\n")

      ;; Process remaining lines
      (doseq [line (rest lines)]
        (let [result (convert-line line)]
          (cond
            (= result :start-setf)
            (reset! in-setf true)

            (= result :end-setf)
            (do
              ;; Process accumulated setf lines
              (doseq [def-line (process-setf-block @setf-lines)]
                (when def-line
                  (.write writer def-line)
                  (.write writer "\n")))
              (.write writer "\n")
              (reset! in-setf false)
              (reset! setf-lines []))

            @in-setf
            (swap! setf-lines conj line)

            (and (string? result) (not (str/blank? result)))
            (do
              (.write writer result)
              (.write writer "\n"))

            :else nil))))))

;; Main execution
(defn -main []
  (let [examples-dir "examples"
        output-dir "examples/clojure"
        techne-files (filter #(str/ends-with? (.getName %) ".techne")
                            (file-seq (io/file examples-dir)))]

    ;; Skip rand-50.techne since we already converted it manually
    (doseq [file techne-files]
      (let [filename (.getName file)
            basename (str/replace filename #"\.techne$" "")]
        (when (not= basename "rand-50")
          (convert-file (.getPath file)
                       (str output-dir "/" basename ".clj")))))

    (println "\nConversion complete!")))

;; Run if executed as script
(when (= *file* (System/getProperty "babashka.file"))
  (-main))
