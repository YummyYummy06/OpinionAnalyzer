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

## Funciones de Python

### 1. `procesar_linea`

Procesa cada línea del archivo csv

```
def procesar_linea(id_opinion, texto):
 thread_name = threading.current_thread().name
    print(f"[{thread_name}] [ID: {id_opinion}] Procesando: {texto[:50]}...")  # Imprime los primeros 50 caracteres del texto
    time.sleep(0.5) # Simula un tiempo de procesamiento
    print(f"[{thread_name}] [ID: {id_opinion}] Procesamiento completado.\n")
```

**Parámetros**

- id_opinion, texto

**Retorna**

- Imprime por pantalla los threads que se están procesando

---

### 2. `worker`

Función para que el trabajador haga la función de procesar línea

```
def worker(cola_trabajo):
  while True:
       trabajo = cola_trabajos.get()
       if trabajo is None: # Señal para terminar el trabajador
          break
       id_opinion, texto = trabajo
       procesar_linea(id_opinion, texto)
       cola_trabajos.task_done()

```

**Parámetros**

- `cola_trabajos` - Una cola vacía

**Retorna**

- Cada línea ya procesada

---

### 3. `main`

Función principal que lee el archivo y se los pasa a los trabajadores

```
  archivo_csv = 'Opiniones-list.csv'  # Nombre del archivo CSV a leer
     num_threads = 5  # Número de trabajadores para procesar las líneas
     print(f"Iniciando procesamiento de opiniones con {num_threads} hilos...\n")
     inicio = time.time()  # Marca el tiempo de inicio

     try:
          cola_trabajos = Queue()
          # Crear y iniciar los trabajadores
          threads = []
          for i in range(num_threads):
               thread = threading.Thread(target=worker, args=(cola_trabajos,), name=f"Trabajador-{i+1}")
               thread.start()
               threads.append(thread)

          with open(archivo_csv, 'r', encoding='utf-8') as archivo:
               lector = csv.DictReader(archivo, delimiter=';')  # Ajusta el delimitador si es necesario
               for fila in lector:
                    id_opinion = fila['Id']
                    texto = fila['Texto']
                    cola_trabajos.put((id_opinion, texto))  # Agrega la tarea a la cola

          # Esperar a que todos los trabajos terminen
          cola_trabajos.join()

          # Detener los trabajadores
          for _ in range(num_threads):
                cola_trabajos.put(None)  # Señal para que los trabajadores terminen
          for thread in threads:
                thread.join()
          fin = time.time()  # Marca el tiempo de fin
          print(f"\n=== Procesamiento completado en {fin - inicio:.2f} segundos ===")

     except FileNotFoundError:
            print(f"Error: El archivo '{archivo_csv}' no se encontró.")
     except Exception as e:
            print(f"Error al procesar el archivo: {e}")

```

**Parámetros**

- Ninguno, realiza la función directamente

**Retorna**

- Todos los threads procesados

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

### Inyectar datos de csv

- Abrir terminal y escribir `python3 data-ingest.py`

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

**[Miguel Angel Navarro]**  
Proyecto: Opinion Analyzer  
Fecha: [Fecha]

---
