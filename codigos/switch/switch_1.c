/* switch_func_control.c */


void combinacion(int op)
{
    int i, num;                     /* variables auxiliares */

    switch (op) {

        case 1:                     /* --- for ------------------------ */
            for (i = 0; i < 3; i++) {
                printf("for %d\n", i);
            }
            break;

        case 2:                     /* --- while ---------------------- */
            i = 0;
            while (i < 3) {
                printf("while %d\n", i);
                i++;
            }
            break;

        case 3:                     /* --- if / else ------------------ */
            if (op == 3) {
                printf("if-true\n");
            } else {
                printf("if-false\n");
            }
            break;

        case 4:                     /* --- do-while ------------------- */
            i = 0;
            do {
                printf("do-while %d\n", i);
                i++;
            } while (i < 3);
            break;

        case 5:                     /* --- scanf ---------------------- */
            printf("Ingrese un entero: ");
            scanf("%d", &num);
            printf("Recibido %d\n", num);
            break;

        default:                    /* --- default -------------------- */
            printf("caso default\n");
            break;
    }

    /* Siempre se ejecuta, sin importar qué rama eligió el switch */
    printf("-- fin de combinacion()\n");
}

int main(void)
{
    /* Probamos cada variante.  Para el caso 5 te pedirá un número. */
    combinacion(1);
    combinacion(2);
    combinacion(3);
    combinacion(4);
    combinacion(5);

    return 0;
}
