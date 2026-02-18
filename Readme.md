#DOCUMENTACIÓN

**Nombre del proyecto:** Opinion Analyzer
**Objetivo del proyecto:** Análisis de opiniones en tiempo real, con la
intención de entender como funcionan las empresas donde me gustaría
trabajar

**Tecnologías:** Portacle, Python3, Docker, PostgreSQ

---

##Uso de Portacle

1. Interfáz de Portacle
   ![Frame](images/Frame.png)

2. Controles de interfaz

**Archivos**

> Buscar archivo : Ctrl + x + Ctrl f

Edición

> Guardar archivo: Ctrl + x seguido de Ctrl + s
> Cortar línea o selección: Ctrl + w
> Copiar línea o selección: Alt + w
> Pegar: Ctrl + y
> Deshacer: Ctrl + _ (o Ctrl + /)
> Rehacer: Ctrl + g luego Ctrl + _

**Compilación y ejecución**

> Compilar: Ctrl + c luego Ctrl + c
> Evaluar expresión o buffer: Ctrl + x luego Ctrl + e
> Reiniciar REPL (si aplica): Ctrl + c luego Ctrl + z

**Navegación**

> Ir a inicio de línea: Ctrl + a
> Ir a fin de línea: Ctrl + e
> Subir una línea: Ctrl + p
> Bajar una línea: Ctrl + n
> Buscar texto: Ctrl + s (forward search)
> Repetir búsqueda: Ctrl + r

**Ventanas y buffers**

> Cambiar buffer: Ctrl + x luego b
> Cerrar buffer: Ctrl + x luego k
> Dividir ventana horizontal: Ctrl + x luego 2
> Dividir ventana vertical: Ctrl + x luego 3
> Mover entre ventanas: Ctrl + x luego o

2. Definición de funciones

```split-words
"Utilidad: divide un string en palabras (por espacios)"
(defun split-words (str)
  (loop for start = 0 then (1+ end)
        for end = (position #\Space str :start start)
        for word = (string-trim '(#\Space #\Tab #\Newline)
                                 (subseq str start end))
        when (> (length word) 0)
          collect word
        while end))
```

```count-words
(defun count-words (str)
  "Retorna el número de palabras en STR."
  (length (split-words str)))
```

````unique-words
(defun unique-words (list-of-strings)
"Retorna una lista con todas las palabras únicas de LIST-OF-STRINGS."
(remove-duplicates
(mapcan #'split-words list-of-strings)
:test #'string-equal))
´´´

```word-frequency
(defun word-frequency (str)
  "Retorna un hash-table {palabra -> frecuencia} a partir de STR."
  (let ((freq (make-hash-table :test #'equal))
        (words (split-words str)))
    (dolist (word words)
      (let ((lower (string-downcase word)))
        (incf (gethash lower freq 0))))
    freq))
````

```print-hash-table
(defun print-hash-table (ht)
  (maphash (lambda (k v)
             (format t " ~a => ~a~%" k v))
           ht))
```

```

```
