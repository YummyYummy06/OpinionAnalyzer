# DOCUMENTACIÓN

**Nombre del proyecto:** Opinion Analyzer  
**Objetivo del proyecto:** Análisis de opiniones en tiempo real, con la intención de entender cómo funcionan las empresas donde me gustaría trabajar  
**Tecnologías:** Portacle, Python3, Docker, PostgreSQL

---

## 📋 Tabla de Contenidos

1. [Introducción a Portacle](#uso-de-portacle)
2. [Interfaz y Controles](#controles-de-interfaz)
3. [Funciones Implementadas](#funciones-implementadas)
4. [Ejemplos de Uso](#ejemplos-de-uso)
5. [Pruebas](#ejecutar-pruebas)

---

## 🖥️ Uso de Portacle

### Interfaz de Portacle

![Interfaz de Portacle](images/Frame.png)

Portacle es un entorno de desarrollo integrado (IDE) portátil para Common Lisp que incluye Emacs, SBCL (compilador de Lisp) y SLIME (entorno interactivo).

---

## ⌨️ Controles de Interfaz

### 📁 Archivos

| Acción               | Atajo             |
| -------------------- | ----------------- |
| Buscar/Abrir archivo | `Ctrl+x` `Ctrl+f` |
| Guardar archivo      | `Ctrl+x` `Ctrl+s` |
| Guardar como         | `Ctrl+x` `Ctrl+w` |

### ✏️ Edición

| Acción                   | Atajo                   |
| ------------------------ | ----------------------- |
| Cortar línea o selección | `Ctrl+w`                |
| Copiar línea o selección | `Alt+w`                 |
| Pegar                    | `Ctrl+y`                |
| Deshacer                 | `Ctrl+_` o `Ctrl+/`     |
| Rehacer                  | `Ctrl+g` luego `Ctrl+_` |
| Seleccionar todo         | `Ctrl+x` `h`            |

### ⚙️ Compilación y Ejecución

| Acción                             | Atajo             |
| ---------------------------------- | ----------------- |
| Compilar función actual            | `Ctrl+c` `Ctrl+c` |
| Compilar y cargar archivo completo | `Ctrl+c` `Ctrl+k` |
| Evaluar expresión antes del cursor | `Ctrl+x` `Ctrl+e` |
| Evaluar región seleccionada        | `Ctrl+c` `Ctrl+r` |
| Reiniciar REPL                     | `Ctrl+c` `Ctrl+z` |

### 🧭 Navegación

| Acción                        | Atajo       |
| ----------------------------- | ----------- |
| Ir a inicio de línea          | `Ctrl+a`    |
| Ir a fin de línea             | `Ctrl+e`    |
| Subir una línea               | `Ctrl+p`    |
| Bajar una línea               | `Ctrl+n`    |
| Buscar texto (hacia adelante) | `Ctrl+s`    |
| Buscar texto (hacia atrás)    | `Ctrl+r`    |
| Ir a línea específica         | `Alt+g` `g` |

### 🪟 Ventanas y Buffers

| Acción                     | Atajo             |
| -------------------------- | ----------------- |
| Cambiar buffer             | `Ctrl+x` `b`      |
| Listar todos los buffers   | `Ctrl+x` `Ctrl+b` |
| Cerrar buffer              | `Ctrl+x` `k`      |
| Dividir ventana horizontal | `Ctrl+x` `2`      |
| Dividir ventana vertical   | `Ctrl+x` `3`      |
| Cerrar otras ventanas      | `Ctrl+x` `1`      |
| Mover entre ventanas       | `Ctrl+x` `o`      |

---

## 📚 Funciones Implementadas

### 1. `split-words` (Función Auxiliar)

Divide un string en palabras individuales separadas por espacios.

```lisp
(defun split-words (str)
  "Divide un string en palabras (por espacios)"
  (loop for start = 0 then (1+ end)
        for end = (position #\Space str :start start)
        for word = (string-trim '(#\Space #\Tab #\Newline)
                                 (subseq str start end))
        when (> (length word) 0)
          collect word
        while end))
```

**Parámetros:**

- `str` - String de entrada

**Retorna:**

- Lista de palabras (strings)

**Ejemplo:**

```lisp
(split-words "el gato duerme")
;; => ("el" "gato" "duerme")
```

---

### 2. `count-words`

Cuenta la cantidad total de palabras en un string.

```lisp
(defun count-words (str)
  "Retorna el número de palabras en STR."
  (length (split-words str)))
```

**Parámetros:**

- `str` - String de entrada

**Retorna:**

- Número entero con la cantidad de palabras

**Ejemplo:**

```lisp
(count-words "el gato duerme profundamente")
;; => 4
```

---

### 3. `unique-words`

Extrae todas las palabras únicas de una lista de strings, eliminando duplicados.

```lisp
(defun unique-words (list-of-strings)
  "Retorna una lista con todas las palabras únicas de LIST-OF-STRINGS."
  (remove-duplicates
    (mapcan #'split-words list-of-strings)
    :test #'string-equal))
```

**Parámetros:**

- `list-of-strings` - Lista de strings

**Retorna:**

- Lista de palabras únicas (sin duplicados)

**Ejemplo:**

```lisp
(unique-words '("hola mundo" "mundo cruel" "hola lisp"))
;; => ("hola" "mundo" "cruel" "lisp")
```

---

### 4. `word-frequency`

Calcula la frecuencia de aparición de cada palabra en un string.

```lisp
(defun word-frequency (str)
  "Retorna un hash-table {palabra -> frecuencia} a partir de STR."
  (let ((freq (make-hash-table :test #'equal))
        (words (split-words str)))
    (dolist (word words)
      (let ((lower (string-downcase word)))
        (incf (gethash lower freq 0))))
    freq))
```

**Parámetros:**

- `str` - String de entrada

**Retorna:**

- Hash-table donde las claves son palabras (en minúsculas) y los valores son frecuencias

**Ejemplo:**

```lisp
(word-frequency "el gato y el perro")
;; => hash-table: {"el" => 2, "gato" => 1, "y" => 1, "perro" => 1}
```

---

### 5. `print-hash-table` (Función Auxiliar)

Imprime el contenido de un hash-table de forma legible.

```lisp
(defun print-hash-table (ht)
  "Imprime un hash-table en formato clave => valor"
  (maphash (lambda (k v)
             (format t "  ~a => ~a~%" k v))
           ht))
```

**Parámetros:**

- `ht` - Hash-table a imprimir

**Retorna:**

- NIL (solo imprime en pantalla)

**Ejemplo:**

```lisp
(print-hash-table (word-frequency "el gato y el gato"))
;;   el => 2
;;   gato => 2
;;   y => 1
```

---

## 💡 Ejemplos de Uso

### Ejemplo 1: Analizar un texto simple

```lisp
;; Contar palabras
(count-words "El análisis de opiniones es fascinante")
;; => 5

;; Obtener frecuencias
(setf freq (word-frequency "el gato y el perro y el gato"))
(print-hash-table freq)
;;   el => 3
;;   gato => 2
;;   y => 2
;;   perro => 1
```

### Ejemplo 2: Procesar múltiples textos

```lisp
;; Lista de opiniones
(setf opiniones '("excelente servicio y atención"
                  "servicio lento pero buena atención"
                  "excelente producto"))

;; Obtener todas las palabras únicas
(unique-words opiniones)
;; => ("excelente" "servicio" "y" "atención" "lento" "pero" "buena" "producto")
```

### Ejemplo 3: Análisis completo

```lisp
;; Texto a analizar
(setf texto "La empresa tiene buena cultura empresarial.
             La cultura es importante para el éxito.")

;; Número de palabras
(format t "Total de palabras: ~a~%" (count-words texto))

;; Frecuencias
(format t "~%Frecuencias:~%")
(print-hash-table (word-frequency texto))
```

---

## 🧪 Ejecutar Pruebas

### Preparación

1. **Abrir el archivo** `text_processing.lisp` en Portacle
2. **Compilar y cargar** el archivo completo: `Ctrl+c` `Ctrl+k`
3. Esperar mensaje de confirmación en el REPL

### Ejecutar suite de pruebas

```lisp
(test-all)
```

### Salida esperada

```
========================================
  PRUEBAS DE FUNCIONES DE TEXTO
========================================

--- TEST 1: count-words ---
Texto: "el gato y el perro y el gato"
Número de palabras: 8

--- TEST 2: unique-words ---
Lista de entrada: ("hola mundo" "mundo cruel" "hola lisp")
Palabras únicas: ("hola" "mundo" "cruel" "lisp")

--- TEST 3: word-frequency ---
Texto: "el gato y el perro y el gato"
Frecuencias:
  el => 3
  gato => 2
  y => 2
  perro => 1

========================================
  PRUEBAS COMPLETADAS
========================================
```

---

## 📦 Estructura del Proyecto

```
opinion-analyzer/
├── text_processing.lisp    # Funciones principales
├── images/
│   └── Frame.png           # Captura de interfaz
├── README.md               # Esta documentación
└── docs/
    └── manual.pdf          # Manual extendido
```

---

## 🔧 Solución de Problemas

### Error: "undefined function"

**Causa:** El archivo no está cargado en el REPL.

**Solución:**

```lisp
;; Cargar el archivo
(load "text_processing.lisp")

;; O compilar y cargar desde Emacs
;; Ctrl+c Ctrl+k
```

### Error: "LENGHT undefined"

**Causa:** Error de tipeo en el código fuente.

**Solución:** Verificar que se use `length` (con 'g') no `lenght`.

### REPL no responde

**Solución:**

- Presionar `Ctrl+g` para cancelar operación actual
- O reiniciar REPL: `Ctrl+c` `Ctrl+z`

---

## 📚 Referencias

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Portacle Documentation](https://portacle.github.io/)
- [SLIME Manual](https://common-lisp.net/project/slime/doc/html/)

---

## 👤 Autor

**[Tu Nombre]**  
Proyecto: Opinion Analyzer  
Fecha: [Fecha]

---

## 📄 Licencia

Este proyecto es de código abierto y está disponible bajo la [Licencia MIT](LICENSE).
