/* switch_func_break.c */

void mostrarNumero(int op)            /* ← pasamos un parámetro            */
{
    switch (op) {                     /* ← aquí está el switch             */
        case 1:
            printf("uno\n");
            break;                    /* ← break evita el “fall-through”   */

        case 2:
            printf("dos\n");
            break;

        case 3:
            printf("tres\n");
            break;

        default:
            printf("otro\n");
            break;
    }

    /* Esto siempre se ejecuta, porque el break sólo corta el switch */
    printf("-- fin de mostrarNumero()\n");
}

int main(void)
{
    mostrarNumero(1);
    mostrarNumero(2);
    mostrarNumero(5);
    return 0;
}
