// /* switch_break.c */

int main() {
    int selector = 2;
    int y        = -999;   // * valor centinela       */
    int z        = 0;      // /* para mostrar que main sigue */

    switch (selector) {
        case 1:
            y = 10;
            break;            // /* no debería entrar aquí */
            y = 1111;         // /* ¡NO debe ejecutarse! */

        case 2:
            y = 20;
            break;            // /* corta acá              */
            y = 2222;         // /* ¡NO debe ejecutarse! */

        default:
            y = -1;
    }

    z = 123;                 // /* línea después del switch */

    // /* Deberíamos ver: 20 123 */
    printf("%d %d\n", y, z);
    return 0;
}
