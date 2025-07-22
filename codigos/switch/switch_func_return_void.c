/* switch_func_return_void.c */

void procesar(int op)            /* función SIN valor de retorno */
{
    switch (op) {
        case 1:
            printf("uno\n");
            return;              /* ← ❶ sale de la función aquí */

        case 2:
            printf("dos\n");
            break;

        default:
            printf("otro\n");
            break;
    }

    /* Sólo se ve si NO hicimos return antes */
    printf("-- fin de procesar()\n");
}

int main(void)
{
    procesar(1);   /* imprime “uno” y vuelve al main            */
    procesar(2);   /* imprime “dos” y luego “-- fin de procesar()” */
    procesar(7);   /* imprime “otro” y luego “-- fin de procesar()” */
    return 0;
}
