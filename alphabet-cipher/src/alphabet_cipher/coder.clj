(ns alphabet-cipher.coder)
;; Ramirez Lopez Alvaro

;; Da el indice de una letra en el alfabeto
(defn indice [c] (- (int c) (int \a)))

;; Usando aritmetica modular, intercambia una letra por otra dependiendo 
;; si se cifrando o descifrando
(defn mod26 [op char1 char2] 
  (char (+ (mod (op (indice char1) (indice char2)) 26) (int \a))))

(defn encode [keyword message]
  (apply str (map #(mod26 + %1 %2) (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map #(mod26 - %2 %1) (cycle keyword) message)))

(defn decipher [cipher message]
  (let [keystream (apply str (map #(mod26 - %1 %2) cipher message))]
    (first (filter #(= message (decode % cipher)) (map #(subs keystream 0 %) (range 1 (count keystream)))))))