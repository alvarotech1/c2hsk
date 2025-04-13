# C2Haskell: Traductor de C a Haskell

**C2Haskell** es un proyecto en desarrollo cuyo objetivo es traducir programas escritos en C a su equivalente en Haskell, respetando las diferencias semánticas y estructurales entre ambos lenguajes. Este traductor está diseñado con un scope acotado, priorizando inicialmente la traducción de estructuras y construcciones comunes y simples del lenguaje C.


> ⚠️ Este proyecto está en una etapa temprana de desarrollo. Las traducciones completas y complejas aún no están soportadas.

## ✨ Objetivo

Facilitar la comprensión y transición de programas escritos en C al paradigma funcional de Haskell, ofreciendo una herramienta automatizada que realice traducciones parciales y legibles.

## 🔧 Características soportadas (hasta ahora)

Actualmente, el traductor soporta la conversión de los siguientes elementos:

**Estructuras de control**  
- `if-else` 
- Bucles `while` 
- Secuenciación de comandos (`;`)  

**Manejo de funciones**  
- Declaración de funciones con tipos de retorno  
- Parámetros formales tipados  
- `return` 
- Llamadas a funciones simples  

**Tipos básicos**  
- `int`, `float`, `double`, `char`, `string`  
- Promoción automática numérica (ej: `int + float → float`)  

**Entrada/Salida**  
- `printf` básico con strings literales y expresiones  
- Traducción a `putStrLn` de Haskell  

**Funciones en C**  
  Traducción de definiciones de funciones simples a funciones puras en Haskell.

### 📜 Características en proceso

- Manejo de archivos
- Arrays
- Structs
- Punteros
- `for` loops
- `switch` case


![raw](https://github.com/user-attachments/assets/0871eb29-90a7-40dc-81c1-66676e8b7c84)
