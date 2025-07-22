// * switch_booleana.c */

int main() {
    int a   = 5;
    int b   = 5;
    int res = -1;

    //  (a == b) se evalúa a 1 si es true, 0 si es false  */
    switch (a == b) {
        case 1:               /* true */
            res = 42;
            break;

        case 0:               /* false */
            res = 99;
            break;

        // * no haría falta, pero lo dejamos para cubrir el default */
        default:
            res = -100;
    }

    // * debería imprimir 42 */
    printf("%d\n", res);
    return 0;
}
