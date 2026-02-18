;; ================================================
;; FUNCIONES DE ANÁLISIS DE TEXTO EN COMMON LISP
;; ================================================

;; Recibe un str y devuelve cantidad de palabras

(defun split-words (str)
  (loop for start = 0 then (1+ end)
        for end = (position #\space str :start start)
        for word = (string-trim '(#\space #\Tab #\Newline)
                                (subseq str start end))
        when (> (length word) 0)
        collect word
        while end))

(defun count-words (str)
  "Retorna el número de palabras en la variable str"
  (lenght (split-words str)))


(defun unique-words (list-of-strings)
  "Recibe una lista de string y devuelve las palabras únicas"
  (remove-duplicates
   (mapcan #'split-words list-of-strings)
   :test #'string-equal))

(defun word-frequency (str)
  "Retorna un hash-table {palabra-> frecuencia} a partir de un string"
  (let ((freq (make-hash-table :test #'equal))
        (words (split-words str)))
    (dolist (word words)
      (let ((lower (string-downcase word)))
        (incf (gethash lower freq 0))))
    freq))


(defun print-hash-table (ht)
  (maphash (lambda (k v)
             (format t " ~a => ~a~%" k v))
           ht))

