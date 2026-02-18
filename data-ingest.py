#!/usr/bin/env python3
"""
Docstring para data-ingest
Script para ingesta de datos en el proyecto de análisis de datos.

Lee un archivo CSV y procesa cada linea secuencialmente
"""

import csv
import time
import threading
from queue import Queue

def procesar_linea(id_opinion, texto):
    """
    Función para procesar cada línea del archivo CSV.
    En este ejemplo, simplemente imprime el ID de la opinión y el texto.

    Argumentos:
    id_opinion -- El ID de la opinión
    texto -- El texto de la opinión

    """
    thread_name = threading.current_thread().name
    print(f"[{thread_name}] [ID: {id_opinion}] Procesando: {texto[:50]}...")  # Imprime los primeros 50 caracteres del texto
    time.sleep(0.5) # Simula un tiempo de procesamiento
    print(f"[{thread_name}] [ID: {id_opinion}] Procesamiento completado.\n")


def worker(cola_trabajos):
    """
    Función para el trabajador que procesa las líneas de la cola.

    Argumentos:
    cola_trabajos(Queue) -- La cola de tareas a procesar
    """
    while True:
       trabajo = cola_trabajos.get()
       if trabajo is None: # Señal para terminar el trabajador
          break
       id_opinion, texto = trabajo
       procesar_linea(id_opinion, texto)
       cola_trabajos.task_done()
    

def main():   
     """
    Función principal para leer el archivo CSV y procesar cada línea.
    """
     archivo_csv = 'Opiniones-list.csv'  # Nombre del archivo CSV a leer
     num_threads = 5  # Número de trabajadores para procesar las líneas
     print(f"Iniciando procesamiento de opiniones con {num_threads} hilos...\n")
     inicio = time.time()  # Marca el tiempo de inicio

     try:
          cola_trabajos = Queue()
          # Crear y iniciar los trabajadores
          threads = []
          for i in range(num_threads):
               thread = threading.Thread(target=worker, args=(cola_trabajos,), name=f"Trabajador: {i+1}")
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

if __name__ == "__main__":
    main()