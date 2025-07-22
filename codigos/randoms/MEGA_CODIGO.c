/* final_boss.c  –  ¡el compendio de todo! */


/*--------------------------------------------------*
 * 1. switch + break – función externa
 *--------------------------------------------------*/
void mostrarNumero(int op)
{
    switch (op) {
        case 1:  printf("uno\n");   break;
        case 2:  printf("dos\n");   break;
        case 3:  printf("tres\n");  break;
        default: printf("otro\n");  break;
    }
    printf("-- fin de mostrarNumero()\n");
}

/*--------------------------------------------------*
 * 2. switch + return;  (función void)
 *--------------------------------------------------*/
void procesar(int op)
{
    switch (op) {
        case 1:
            printf("proc uno\n");
            return;                          /* corta TODA la función */

        case 2:
            printf("proc dos\n");
            break;

        default:
            printf("proc otro\n");
            break;
    }
    printf("-- fin de procesar()\n");
}

/*--------------------------------------------------*
 * 3. switch con for / while / if / do-while / scanf
 *--------------------------------------------------*/
void combinacion(int op)
{
    int i, num;

    switch (op) {
        case 1:          /* for */
            for (i = 0; i < 3; i++){
                printf("for %d\n", i);
			}
            break;

        case 2:          /* while */
            i = 0;
            while (i < 3) {
                printf("while %d\n", i);
                i++;
            }
            break;

        case 3:          /* if / else */
            if (op == 3){
                printf("if-true\n");
			}
            else{
                printf("if-false\n");
			}
            break;

        case 4:          /* do-while */
            i = 0;
            do {
                printf("do-while %d\n", i);
                i++;
            } while (i < 3);
            break;

        case 5:          /* scanf */
            printf("Ingrese un entero (combinacion): ");
            scanf("%d", &num);
            printf("Recibido %d\n", num);
            break;

        default:
            printf("comb default\n");
    }
    printf("-- fin de combinacion()\n");
}

/*--------------------------------------------------*
 * 4. loop for con return;  (Test A)
 *--------------------------------------------------*/
void loopFor(int n)
{
    int i;
    switch (n) {
        case 0:
            for (i = 0; i < 5; i++) {
                printf("for-i=%d\n", i);
                if (i == 2) {
                    printf("return en i==2\n");
                    return;                  /* corta función */
                }
            }
            printf("nunca se imprime\n");
            break;

        default:
            printf("loopFor default\n");
            break;
    }
    printf("-- fin de loopFor()\n");
}

/*--------------------------------------------------*
 * 5. switch anidado con while + return;  (Test B)
 *--------------------------------------------------*/
void deepNested(int cat, int k)
{
    int j;      /* usado en el while */

    switch (cat) {
        case 1:
            printf("cat 1\n");

            switch (k) {
                case 5:
                    printf("  k=5, entro al while\n");
                    j = 0;
                    while (1 == 1) {
                        printf("    j=%d\n", j++);
                        return;              /* corta deepNested */
                    }
                    /* jamás llega */
                default:
                    printf("  sub default\n");
            }
            printf("fin cat 1\n");
            break;

        default:
            printf("cat default\n");
    }
    printf("-- fin de deepNested()\n");
}

/*--------------------------------------------------*
 * 6. switch interior con return; + for externo  (Test C)
 *--------------------------------------------------*/
void mix(int x)
{
    int p;

    switch (x) {
        case 2:
            printf("inner switch con return\n");
            return;                          /* corta función */

        default:
            printf("inner default\n");
    }

    for (p = 0; p < 3; p++){
        printf("for-p=%d\n", p);
	}

    printf("-- fin de mix()\n");
}

/*--------------------------------------------------*
 * 7. switch anidado en otra función (analizar)
 *--------------------------------------------------*/
void analizar(int categoria, int subCategoria)
{
    switch (categoria) {
        case 0:
            printf("Categoria 0\n");
            break;

        case 1:
            printf("Categoria 1\n");

            switch (subCategoria) {
                case 1:  printf("  Sub 1-1\n");          break;
                case 2:  printf("  Sub 1-2\n");          break;
                default: printf("  Sub 1-default\n");
            }
            printf("-- fin subcat 1 --\n");
            break;

        case 2:
            printf("Categoria 2\n");
            break;

        default:
            printf("Categoria default\n");
    }
    printf("-- fin de analizar()\n");
}

/*--------------------------------------------------*
 * MAIN – ejecuta TODO
 *--------------------------------------------------*/
int main(void)
{
    int i;
    int tests[5] = {1, 2, 3, 4, 5};

    /* 1. mostrarNumero con break */
    printf("=== mostrarNumero ===\n");
    mostrarNumero(1);
    mostrarNumero(2);
    mostrarNumero(5);

    /* 2. procesar con return; */
    printf("\n=== procesar ===\n");
    procesar(1);
    procesar(2);
    procesar(7);

    /* 3. combinacion con todos los casos (pide un número en op==5) */
    printf("\n=== combinacion ===\n");
    for (i = 0; i < 5; i++){
        combinacion(tests[i]);
	}

    /* 4. loopFor – return dentro de for */
    printf("\n=== loopFor ===\n");
    loopFor(0);
    loopFor(9);

    /* 5. deepNested – while + return; */
    printf("\n=== deepNested ===\n");
    deepNested(1, 5);     /* se corta en el while */
    deepNested(1, 9);     /* sigue completo */

    /* 6. mix – switch con return; y for externo */
    printf("\n=== mix ===\n");
    mix(2);               /* corta antes del for */
    mix(3);               /* for completo */

    /* 7. analizar – switch anidado en función externa */
    printf("\n=== analizar ===\n");
    analizar(1, 2);
    analizar(0, 0);
    analizar(1, 9);
    analizar(3, 0);

    /* scanf final para que veas que stdin aún funciona */
    printf("\n=== scanf final de control ===\n");
    printf("Ingrese otro entero (final): ");
    int z;
    scanf("%d", &z);
    printf("Fin. Leí %d\n", z);

    return 0;
}
