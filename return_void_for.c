/* testA_return_in_for.c */

void loopFor(int n)
{
    switch (n) {
        case 0:
            for (int i = 0; i < 5; i++) {
                printf("for-i=%d\n", i);
                if (i == 2) {
                    printf("return en i==2\n");
                    return;                 /* ← aquí salimos de la función */
                }
            }
            printf("nunca se imprime\n");
            break;

        default:
            printf("caso default\n");
            break;
    }
    printf("-- fin de loopFor()\n");        /* sólo si NO hubo return */
}

int main(void)
{
    loopFor(0);       /* debe cortar en i==2 */
    loopFor(9);       /* caso default, imprime fin */
    return 0;
}
