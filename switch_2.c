/* switch_main_control_fix.c */

int main(void)
{
    int tests[5] = {1, 2, 3, 4, 5};   /* ← ahora el tamaño es [5]          */
    int t;                             /* índice del for                    */

    for (t = 0; t < 5; t++) {
        int op  = tests[t];
        int i, num;                    /* auxiliares para los casos */

        printf("=== op = %d ===\n", op);

        switch (op) {

            case 1:                    /* -------- for ----------- */
                for (i = 0; i < 3; i++){
                    printf("for %d\n", i);
				}
                break;

            case 2:                    /* -------- while ---------- */
                i = 0;
                while (i < 3) {
                    printf("while %d\n", i);
                    i++;
                }
                break;

            case 3:                    /* -------- if / else ------ */
                if (op == 3){
                    printf("if-true\n");
				}
                else{
                    printf("if-false\n");
				}
                break;

            case 4:                    /* -------- do-while ------- */
                i = 0;
                do {
                    printf("do-while %d\n", i);
                    i++;
                } while (i < 3);
                break;

            case 5:                    /* -------- scanf ---------- */
                printf("Ingrese un entero: ");
                scanf("%d", &num);
                printf("Recibido %d\n", num);
                break;

            default:
                printf("caso default\n");
                break;
        }

        printf("-- fin del bloque op=%d --\n\n", op);
    }

    return 0;
}