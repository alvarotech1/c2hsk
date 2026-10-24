# C2Haskell

### Un traductor experimental de C a Haskell, escrito en Haskell

C2Haskell toma programas escritos en un **subconjunto de C**, analiza su estructura y genera código fuente Haskell que busca conservar su comportamiento. El proyecto explora un desafío central de la traducción entre paradigmas: representar variables mutables, punteros y estructuras de control imperativas dentro de un lenguaje funcional.

Está desarrollado con **Haskell y Parsec** y utiliza `IORef` e `IOArray` para modelar el estado mutable del programa original.

> [!IMPORTANT]
> Es un proyecto académico y experimental, **no un compilador completo de C**. La compatibilidad está limitada a las construcciones implementadas; un programa puede ser aceptado por el parser y, aun así, necesitar ajustes para generar Haskell válido o conservar exactamente la semántica de C.

## Tabla de contenidos

- [Cómo funciona](#cómo-funciona)
- [Inicio rápido](#inicio-rápido)
- [Ejemplo de traducción](#ejemplo-de-traducción)
- [Funcionalidades y alcance](#funcionalidades-y-alcance)
- [Estructura del repositorio](#estructura-del-repositorio)
- [Decisiones de implementación](#decisiones-de-implementación)
- [Limitaciones conocidas](#limitaciones-conocidas)
- [Pruebas y ejemplos](#pruebas-y-ejemplos)
- [Licencia](#licencia)

## Cómo funciona

El traductor sigue un flujo similar al de un compilador, pero su resultado es **otro archivo de código fuente**, no un ejecutable:

1. **Lectura:** `Main.run` recibe la ruta de un archivo con código C.
2. **Análisis sintáctico:** `Parser.hs`, construido con Parsec, transforma el texto en un árbol de sintaxis abstracta (AST).
3. **Representación intermedia:** `AST.hs` modela tipos, expresiones, condiciones, sentencias y funciones.
4. **Generación de código:** `Evaluador.hs` recorre el AST, administra identificadores, ámbitos y estado, y emite instrucciones Haskell.
5. **Salida:** `Main.run` muestra el código generado por consola y lo guarda en un archivo `.hs` junto al archivo de entrada.

La ejecución del programa resultante es un paso separado: se realiza con GHC, GHCi o `runghc`.

## Inicio rápido

### Requisitos

- **GHC/GHCi 9.x** (recomendado).
- Paquetes Haskell: `parsec`, `mtl`, `containers`, `array` y `filepath`, además de `base`.
- Una terminal desde la que ejecutar los comandos.

El repositorio no incluye actualmente un archivo `.cabal` ni una configuración de Stack. Es necesario disponer de GHC y de las bibliotecas mencionadas en el entorno local. Podés comprobarlo con `ghc --version` y `ghc-pkg list`.

### 1. Clonar el repositorio

```bash
git clone https://github.com/alvarotech1/c2hsk.git
cd c2hsk
```

### 2. Iniciar el traductor

```bash
ghci -XImportQualifiedPost Main.hs
```

El flag `-XImportQualifiedPost` habilita la sintaxis de importación utilizada por `Evaluador.hs` (`import Data.Array.IO qualified as A`).

Una vez dentro de GHCi:

```haskell
ghci> run "codigos/puntero/punterobasico.c"
ghci> :quit
```

Esto genera `codigos/puntero/punterobasico.hs` y también imprime su contenido en la terminal.

### 3. Ejecutar el Haskell generado

```bash
runghc codigos/puntero/punterobasico.hs
```

Para ese ejemplo, la salida esperada es:

```text
10
5
```

También podés invocar el traductor en una única línea, sin entrar a la sesión interactiva:

```bash
ghci -XImportQualifiedPost Main.hs -e 'run "codigos/puntero/punterobasico.c"'
```

> [!NOTE]
> `main` en `Main.hs` no implementa una interfaz por línea de comandos: actualmente es una acción vacía. Por eso se llama explícitamente a la función `run`. **El traductor sobrescribe** el archivo `.hs` de igual nombre si ya existe: conservá una copia de cualquier salida que quieras preservar.

## Ejemplo de traducción

El archivo [`codigos/puntero/punterobasico.c`](codigos/puntero/punterobasico.c) utiliza un puntero para leer una variable que cambia de valor:

```c
int main() {
    int x = 10;
    int* p = &x;
    printf("%d", *p);
    x = 5;
    printf("%d", *p);
}
```

En lugar de tratar `x` como un valor inmutable, el generador crea una **referencia mutable**. La expresión `&x` permite que `p` conserve acceso a esa referencia y `*p` lea su contenido actualizado.

De forma simplificada, el mecanismo producido en Haskell es:

```haskell
x_ref <- newIORef (10 :: Int32)
p_ref <- newIORef x_ref

ptr <- readIORef p_ref
valor <- readIORef ptr
print valor

writeIORef x_ref (5 :: Int32)
```

*Este fragmento es ilustrativo: el archivo generado real incluye nombres de variables temporales, controles de retorno e instrucciones adicionales.*

## Funcionalidades y alcance

La siguiente tabla describe las construcciones contempladas por el parser y el generador; **«parcial» no implica compatibilidad completa con C**.

| Área | Implementación actual | Alcance / observaciones |
| --- | --- | --- |
| Variables y asignaciones | Implementado para casos básicos | Declaraciones inicializadas y sin inicializar; asignación y operadores `+=`, `-=` y `*=`. |
| Tipos primitivos | Parcial | Se reconocen `int`, `float`, `double`, `char`, `long`, `short`, `void` y `string`; las conversiones y los valores por defecto no cubren todos los casos. `string` es una extensión del parser, no un tipo primitivo de C estándar. |
| Operaciones aritméticas | Implementación básica | `+`, `-`, `*`, `/`, `%`, negación, `++`, `--` y operaciones `sqrt`, `pow`, `log2`. |
| Expresiones lógicas | Implementación básica | Comparaciones `==`, `!=`, `<`, `<=`, `>` y `>=`; conectores `&&`, `\|\|` y `!`. |
| Condicionales | Implementación básica | `if`, `if/else` y combinaciones anidadas con las restricciones sintácticas del parser. |
| Bucles | Parcial | `while`, `do/while` y `for`, con control de estado para `break` y `return`. |
| Selección múltiple | Parcial | `switch`, `case`, `default` y tratamiento de caída entre casos (*fall-through*). |
| Funciones | Parcial | Definiciones, parámetros, llamadas, funciones `void` y retorno con valor; algunos caminos de retorno siguen siendo experimentales. |
| Arrays | Parcial | Arrays unidimensionales, inicialización, lectura/escritura por índice y buffers de caracteres. |
| Punteros | Parcial | Declaración, toma de dirección (`&`), desreferenciación (`*`) y paso a funciones en escenarios simples. |
| Estructuras | Parcial | Declaración de `struct` y acceso/asignación a campos directos (`persona.edad`). |
| Entrada/salida | Parcial | Traducción básica de `printf` y `scanf`, sin implementar por completo sus especificadores de formato. |
| Preprocesador | Limitado | Se ignoran directivas `#include`; no se procesan ni expanden encabezados o macros. |

## Estructura del repositorio

| Ruta | Responsabilidad |
| --- | --- |
| [`Main.hs`](Main.hs) | Entrada al traductor: lectura, parseo, generación e impresión/escritura del resultado mediante `run`. |
| [`Parser.hs`](Parser.hs) | Análisis léxico/sintáctico con Parsec; convierte código C en nodos del AST. |
| [`AST.hs`](AST.hs) | Definición de tipos (`Type`), expresiones (`Exp`, `BoolExp`) y sentencias (`Comm`, `Case`). |
| [`Evaluador.hs`](Evaluador.hs) | Recorrido del AST y generación de instrucciones Haskell; administración de referencias, scopes y control de flujo. |
| [`codigos/`](codigos/) | Programas C para ejercitar funcionalidades, organizados por casos de uso. |
| [`test/working/`](test/working/) | Casos de experimentación con extensión `.lis`. |
| [`test/testing/`](test/testing/) | Pruebas exploratorias y escenarios aún bajo investigación. |
| [`LICENSE`](LICENSE) | Licencia MIT. |

Algunos ejemplos de `codigos/` incluyen:

- [`puntero/`](codigos/puntero/): referencias, desreferenciación y arrays.
- [`switch/`](codigos/switch/): selección múltiple, `break`, funciones y anidamiento.
- [`return/`](codigos/return/): retornos dentro de bucles y `switch`.
- [`struct/`](codigos/struct/): estructuras simples; contiene también ejemplos `.hs` generados.
- [`juego/`](codigos/juego/): programas de mayor tamaño, como Blackjack y Wordle, útiles como casos de integración.

## Decisiones de implementación

### Estado mutable mediante `IORef`

En C, una variable puede cambiar su contenido a lo largo de la ejecución. El generador representa gran parte de esas variables como `IORef`, utilizando `newIORef`, `readIORef` y `writeIORef`. Esto permite reproducir asignaciones, incrementos y ciertas operaciones con punteros dentro de `IO`.

### Arrays mediante `IOArray`

Los arrays se representan con `Data.Array.IO`, lo que permite lectura y escritura indexadas. Para buffers `char[]`, el traductor incorpora lógica relacionada con el terminador nulo (`'\0'`) y una función auxiliar `readCString` cuando es necesaria.

### Ámbitos, nombres y generación

`Evaluador.hs` combina `StateT` con `Writer`: el estado mantiene la pila de ámbitos, los tipos, los identificadores temporales, las definiciones de estructuras y los indicadores de `break`/`return`; el escritor acumula las líneas de Haskell resultantes. Los nombres generados (`*_refN`, `tmpN`) ayudan a separar las variables de C de sus representaciones en Haskell.

### Flujo de control

Los bucles se modelan mediante acciones recursivas en `IO`. Referencias auxiliares registran cuándo se ejecutó un `break` o `return`, mientras que las funciones emplean un espacio de retorno (`IORef (Maybe ...)`). Es una aproximación que permite traducir varios patrones imperativos, pero todavía tiene casos límite.

## Limitaciones conocidas

- **No es compatible con todo C:** no procesa macros, compilación de múltiples unidades de traducción, bibliotecas externas ni el sistema completo de tipos de C.
- **`const` aún no funciona de extremo a extremo:** el parser construye `LetConst`, pero el generador no implementa ese constructor.
- **Punteros y memoria:** no se implementan `malloc`/`free` ni punteros a funciones. En particular, tomar la dirección de un elemento de array no conserva en todos los casos la misma semántica de aliasing de C.
- **`struct` es básico:** se modelan campos directos; no se traduce un tipo algebraico equivalente ni se admite de forma general el manejo de estructuras anidadas.
- **Salida con `printf`:** el generador usa `putStrLn`, por lo que agrega un salto de línea por llamada y elimina los `\n` del formato. No conserva fielmente todos los formatos, anchos o detalles de impresión de C.
- **Entrada con `scanf`:** la implementación usa `getLine` y `words`; no reproduce el parser de formatos, el consumo de caracteres ni todas las reglas de la función de C.
- **Control de flujo complejo:** combinaciones anidadas de `break` y `return`, incluidos los `return;` en funciones `void`, requieren validación adicional y pueden producir Haskell incorrecto.
- **Errores y validación:** un archivo puede fallar durante el parseo, la generación o la compilación posterior. No existe todavía un sistema de diagnóstico exhaustivo ni un pipeline automatizado de pruebas.

El proyecto prioriza la exploración de técnicas de traducción sobre la compatibilidad total con programas C existentes.

## Pruebas y ejemplos

Los directorios `codigos/` y `test/` contienen programas para explorar distintas características. Los archivos `.lis` son entradas de experimentación; **no hay actualmente un runner de tests automatizado** en el repositorio.

Para investigar un caso concreto:

1. Seleccioná o escribí un programa dentro del subconjunto soportado.
2. Ejecutá `run "ruta/al/archivo.c"` desde GHCi.
3. Revisá el `.hs` generado y compilalo/ejecutalo por separado.
4. Compará su comportamiento con el programa C original, especialmente si usa punteros, entrada/salida o control de flujo anidado.

Si encontrás una incompatibilidad, un reporte útil debería incluir el código C mínimo, el Haskell generado y el resultado esperado frente al obtenido.

## Licencia

Distribuido bajo la [licencia MIT](LICENSE). Copyright © 2025 Álvaro Acosta.
