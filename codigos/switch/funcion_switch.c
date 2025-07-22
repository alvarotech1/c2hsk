// func_switch.c */

// /* ---------- función a testear ---------- */
int eval(int x) {
    int r = 0;

    switch (x) {
        case 0:
            r = -1;
            break;

        case 1:
            r = 10;
            break;

        case 2:
            r = 20;
            break;

        default:
            r = 99;
    }
    return r;
}

int main() {
    int a   = 2;          // /* parámetro que vamos a pasar */
    int res = eval(a);    // /* debería devolver 20 */

    printf("%d\n", res);  // /* salida esperada: 20 */
    return 0;
}
