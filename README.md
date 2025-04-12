# C2Haskell: Traductor de C a Haskell

**C2Haskell** es un proyecto en desarrollo cuyo objetivo es traducir programas escritos en C a su equivalente en Haskell, respetando las diferencias semánticas y estructurales entre ambos lenguajes. Este traductor está diseñado con un scope acotado, priorizando inicialmente la traducción de estructuras y construcciones comunes y simples del lenguaje C.


> ⚠️ Este proyecto está en una etapa temprana de desarrollo. Las traducciones completas y complejas aún no están soportadas.

## ✨ Objetivo

Facilitar la comprensión y transición de programas escritos en C al paradigma funcional de Haskell, ofreciendo una herramienta automatizada que realice traducciones parciales y legibles.

## 🔧 Características soportadas (hasta ahora)

Actualmente, el traductor soporta la conversión de los siguientes elementos:

- **Estructuras (`struct`)**  
  Traducción de estructuras de datos de C a registros de Haskell.

- **Listas enlazadas**  
  Reconocimiento de estructuras típicas de listas enlazadas y conversión a listas Haskell.

- **Arrays**  
  Traducción de arreglos de C a listas o estructuras de tipo `Array` en Haskell.

- **Manejo básico de archivos**  
  Traducción de operaciones como `fopen`, `fread`, `fwrite`, y `fclose` a sus equivalentes en Haskell (`openFile`, `hGetContents`, etc).

- **`printf` y salidas estándar**  
  Conversión de salidas formateadas (`printf`) a `putStrLn`, `print`, o funciones similares de Haskell.

- **Funciones en C**  
  Traducción de definiciones de funciones simples a funciones puras en Haskell.

- **Foreign.Ptr**  
  Reconocimiento y manejo inicial de punteros usando el módulo `Foreign.Ptr` para compatibilidad con código de bajo nivel.

![raw](https://github.com/user-attachments/assets/0871eb29-90a7-40dc-81c1-66676e8b7c84)
