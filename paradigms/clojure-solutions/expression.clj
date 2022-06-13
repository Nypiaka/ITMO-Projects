;ДЗ 10------------------------------------------------------------------------------------------------------------------
(defn makeOp [func] (fn [& args]
                      (fn [map]
                        (apply func (mapv (fn [current]
                                            (current map)) args)))))
(def add (makeOp +))
(def multiply (makeOp *))
(def subtract (makeOp -))
(def negate (makeOp -))
(defn pow [arg1, arg2] (fn [map] (Math/pow (arg1 map) (arg2 map))))
(defn log [arg1, arg2] (fn [map] (/ (Math/log (Math/abs (arg2 map))) (Math/log (Math/abs (arg1 map))))))
(def fixDiv (fn ([first & list] (cond (and (zero? first) (= (count list) 0)) (/ 1.0 0.0)
                                      (and (zero? first) (not (= (count list) 0))) 0
                                      (= (count list) 0) (/ 1.0 (double first))
                                      :else (/ (double first) (apply * list))))))
(def divide (makeOp fixDiv))
(defn constant [arg] (fn [map] arg))
(defn variable [arg] (fn [map] (get map arg)))
(def operations1 {'+ add '- subtract '* multiply '/ divide 'negate negate 'pow pow 'log log})
(defn parseRecurs [n] (cond (number? n) (constant n)
                            (symbol? n) (variable (str n))
                            :else (apply (get operations1 (nth n 0)) (mapv parseRecurs (rest n)))
                            ))
(defn parseFunction [expr] (parseRecurs (read-string expr)))
;ДЗ 11------------------------------------------------------------------------------------------------------------------
(definterface OperationInterface
  (evaluate [vars])
  (toStr [])
  (toStrSuff [])
  (diff [var])
  )
(deftype Operation [func, toStrElem, diffRule, opers]
  OperationInterface
  (evaluate [this vars] (apply func (mapv (fn [n] (.evaluate n vars)) (vec opers))))
  (toStr [this] (str "(" toStrElem " " (apply clojure.string/join " " (vector (mapv (fn [n] (.toStr n)) (vec opers)))) ")"))
  (toStrSuff [this] (str "(" (apply clojure.string/join " " (vector (mapv (fn [n] (.toStrSuff n)) (vec opers)))) " " toStrElem ")"))
  (diff [this var] (diffRule (vec opers) var)))
(defn evaluate [exp varr] (.evaluate exp varr))
(defn toString [exp] (.toStr exp))
(defn toStringSuffix [exp] (.toStrSuff exp))
(defn diff [exp var] (.diff exp var))
(declare ZERO)
(deftype Cnst [arg]
  OperationInterface
  (evaluate [this var] arg)
  (toStr [this] (str arg))
  (toStrSuff [this] (str arg))
  (diff [this var] ZERO))
(def Constant (fn [arg] (Cnst. arg)))
(def ZERO (Constant 0))
(deftype Vrbl [arg]
  OperationInterface
  (evaluate [this var] (get var (clojure.string/lower-case (subs arg 0 1))))
  (toStr [this] arg)
  (toStrSuff [this] (str arg))
  (diff [this var] (cond (= arg var) (Constant 1)
                         :else ZERO)))
(defn CreateOperation [func, toStrElem, diffRule] (fn [& args] (Operation. func toStrElem diffRule args)))
(def Variable (fn [arg] (Vrbl. arg)))
(def fixDiv (fn ([first & list] (cond (and (zero? first) (= (count list) 0)) (/ 1.0 0.0)
                                      (and (zero? first) (not (= (count list) 0))) 0
                                      (= (count list) 0) (/ 1.0 (double first))
                                      :else (/ (double first) (apply * list))))))
(def Add (CreateOperation + "+" (fn [args, var] (Add (diff (nth args 0) var) (diff (nth args 1) var)))))
(def Subtract (CreateOperation - "-" (fn [args, var] (Subtract (diff (nth args 0) var) (diff (nth args 1) var)))))
(def Multiply (CreateOperation * "*" (fn [args, var] (Add (Multiply (diff (nth args 0) var) (nth args 1)) (Multiply (diff (nth args 1) var) (nth args 0))))))
(def Divide (CreateOperation fixDiv "/" (fn [args, var] (Divide (Subtract (Multiply (diff (nth args 0) var) (nth args 1)) (Multiply (diff (nth args 1) var) (nth args 0))) (Multiply (nth args 1) (nth args 1))))))
(def Negate (CreateOperation - "negate" (fn [args, var] (Subtract (diff (nth args 0) var)))))
(defn logarithm [& arg] (/ (Math/log (Math/abs (nth arg 1))) (Math/log (Math/abs (nth arg 0)))))
(defn ln [& arg] (Math/log (Math/abs (nth arg 0))))
(def Ln (CreateOperation ln "ln" (fn [args, var] (Multiply (Divide (Constant 1) (nth args 0)) (diff (nth args 0) var)))))
(def Log (CreateOperation logarithm "log" (fn [args, var] (diff (Divide (Ln (nth args 1)) (Ln (nth args 0))) var))))
(defn power [& arg] (Math/pow (nth arg 0) (nth arg 1)))
(def Pow (CreateOperation power "pow" (fn [args, var] (Multiply (Pow (nth args 0) (nth args 1)) (Add (Multiply (diff (nth args 1) var) (Log (Constant Math/E) (nth args 0))) (Multiply (nth args 1) (diff (Log (Constant Math/E) (nth args 0)) var)))))))
(defn andd [& arg] (Double/longBitsToDouble (bit-and (Double/doubleToLongBits (nth arg 0)) (Double/doubleToLongBits (nth arg 1)))))
(defn ord [& arg] (Double/longBitsToDouble (bit-or (Double/doubleToLongBits (nth arg 0)) (Double/doubleToLongBits (nth arg 1)))))
(defn xord [& arg] (Double/longBitsToDouble (bit-xor (Double/doubleToLongBits (nth arg 0)) (Double/doubleToLongBits (nth arg 1)))))
(def BitAnd (CreateOperation andd "&" ()))
(def BitOr (CreateOperation ord "|" ()))
(def BitXor (CreateOperation xord "^" ()))
(def operations
  {'+           Add
   '-           Subtract
   '*           Multiply
   '/           Divide
   'negate      Negate
   (symbol "&") BitAnd
   (symbol "|") BitOr
   (symbol "^") BitXor
   'pow         Pow
   'log         Log
   })
(defn parseRecur [n] (cond (number? n) (Constant n)
                           (symbol? n) (Variable (str n))
                           :else (apply (get operations (nth n 0)) (mapv parseRecur (rest n)))
                           ))
(defn parseObject [expr] (parseRecur (read-string expr)))
;ДЗ 12------------------------------------------------------------------------------------------------------------------
;КОПИПАСТА--------------------------------------------------------------------------------------------------------------
(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
         ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0001})) (str input \u0001)))))
(mapv (_parser (_combine str (_char #{\a \b}) (_char #{\x}))) ["ax" "ax~" "bx" "bx~" "" "a" "x" "xa"])

(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (_map f) parser))
(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce (partial _either) p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(def +parser _parser)

(defn +rules [defs]
  (cond
    (empty? defs) ()
    (seq? (first defs)) (let [[[name args body] & tail] defs]
                          (cons
                            {:name name :args args :body body}
                            (+rules tail)))
    :else (let [[name body & tail] defs]
            (cons
              {:name name :args [] :body body :plain true}
              (+rules tail)))))

(defmacro defparser [name & defs]
  (let [rules (+rules defs)
        plain (set (map :name (filter :plain rules)))]
    (letfn [(rule [{name :name, args :args, body :body}] `(~name ~args ~(convert body)))
            (convert [value]
              (cond
                (seq? value) (map convert value)
                (char? value) `(+char ~(str value))
                (contains? plain value) `(~value)
                :else value))]
      `(def ~name (letfn ~(mapv rule rules) (+parser (~(:name (last rules)))))))))
;МОЙ КОД----------------------------------------------------------------------------------------------------------------
(def *digits
  (+char "0123456789"))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(def *number (+map read-string (+str (+seq (+opt (+char "-")) (+str (+plus *digits)) (+opt (+char ".")) (+opt (+str (+plus *digits)))))))
(def *var-chars (+char "XYZxyz"))
(def *var (+str (+plus *var-chars)))
(def *op-chars (+str (+or (+seq *ws (+char "*/-+^&|") *ws) (+seq *ws (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e") *ws))))
(def parseObjectSuffix
  (letfn [(*expression []
            (delay (+or
                     (+map Constant *number)
                     (+map Variable *var)
                     (+map (comp (fn [n] (get operations n)) symbol) *op-chars)
                     (+map (fn [n] (apply (last n) (drop-last n)))
                           (+seqf cons *ws (+ignore (+char "(")) *ws (*expression) (+plus (+seqn 0 *ws (*expression))) *ws (+ignore (+char ")"))))
                     )))]
    (+parser (+seqn 0 *ws (*expression) *ws))))