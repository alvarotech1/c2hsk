C2Haskell: Traductor de C a Haskell
Descripción General

C2Haskell es un traductor automático cuyo objetivo es convertir código fuente en lenguaje C a su equivalente en Haskell. El proyecto busca respetar tanto la sintaxis como la semántica del código original, considerando especialmente las diferencias estructurales y conceptuales entre ambos lenguajes, como la inmutabilidad de las variables en Haskell.

Se apoya en técnicas tradicionales de construcción de compiladores, incluyendo un analizador sintáctico (Parser) y un generador de código (Evaluador), todo desarrollado en Haskell.
Objetivos

    Traducir programas escritos en C a programas equivalentes en Haskell.

    Mantener la validez sintáctica del código generado.

    Preservar la semántica original, incluida la salida por consola.

    Utilizar herramientas y estructuras propias del paradigma funcional, simulando mutabilidad cuando sea necesario.

Estructura del Proyecto

El código fuente se encuentra dividido en los siguientes módulos:

    AST.hs: Define la estructura del Árbol de Sintaxis Abstracta que representa un programa en C.

    Parser.hs: Implementa un parser utilizando Parsec, que convierte el código C en un AST.

    Evaluador.hs: Traduce el AST a código Haskell, línea por línea.

    Main.hs: Orquesta el proceso de lectura, parsing, evaluación y escritura del archivo de salida.

Funcionalidades Soportadas
Control de Flujo

    Condicionales if, if-else

    Bucles while, do-while, for

    Instrucción break

    Switch-case con case y default

Declaraciones y Tipos

    Declaraciones inicializadas y no inicializadas (int x;, float a = 3.0;)

    Tipos básicos: int, float, double, char, string, long, short, void

    Constantes (const int a = 5;)

    Arrays unidimensionales con y sin inicialización (int arr[5];, int arr[3] = {1,2,3};)

    Punteros y desreferenciación (int* p; *p = 10;)

Expresiones

    Aritméticas y booleanas: +, -, *, /, %, ==, !=, <, >, etc.

    Operadores unarios y postfijos: ++, --, -e, !e

    Acceso a arrays: arr[i]

    Toma de dirección y desreferencia: &x, *p

Entrada/Salida

    printf con formatos %d, %f, %s, %c, etc.

    scanf básico para variables simples y arr[i]

Funciones

    Definición de funciones (con o sin parámetros)

    Funciones void y con retorno (int suma(int a, int b))

    Llamadas a funciones

    Mecanismo de retorno (return)

Limitaciones Actuales

    No se soporta recursión mutua entre funciones.

    No se implementa manejo de memoria dinámica (malloc, free).

    No se traduce aún código C que hace uso de estructuras (struct) complejas anidadas.

    No hay soporte para punteros a funciones.

    No se generan tipos algebraicos equivalentes a struct.

Instrucciones de Uso

    Guardar el archivo en C con extensión 

    Ejecutar el archivo Main.hs. Esto puede hacerse desde GHCi:

    ghci Main.hs

    Luego dentro del entorno de ghci se llama a la funcion "run" mas el archivo c a traducir, por ejemplo run "asignacion.c".

    Esto ejecutara el traductor, devolverá tanto por terminal como un archivo .hs con el codigo haskell listo para compilar y ejecutar.

Requisitos

    GHC 9.x o superior

    Paquete parsec para análisis sintáctico

    Sistema operativo compatible con ejecución Haskell estándar

Estado del Proyecto

Este proyecto se encuentra en una etapa de desarrollo. Si bien no cubre todos los aspectos del lenguaje C, ha sido probado con múltiples programas de estructura media y ha demostrado generar código Haskell funcional y legible.
