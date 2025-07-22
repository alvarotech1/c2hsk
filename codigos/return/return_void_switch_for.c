/* testC_return_inner_switch.c */

void mix(int x)
{
    switch (x) {
        case 2:
            printf("inner switch con return\n");
            return;                      /* ← corta función */
        default:
            printf("inner default\n");
    }

    /*  Bucle que nunca debe ejecutarse si x==2  */
    for (int p = 0; p < 3; p++){
        printf("for-p=%d\n", p);
    }
    printf("-- fin de mix()\n");
}

int main(void)
{
    mix(2);      /* debe cortar antes del for */
    mix(3);      /* ejecuta for completo + fin */
    return 0;
}
