
void analizar(int categoria, int subCategoria)
{
    switch (categoria) {

        case 0:
            printf("Categoría 0 (sin subcategorías)\n");
            break;

        case 1:                 
            printf("Categoría 1\n");

            switch (subCategoria) {

                case 1:
                    printf("  Sub 1-1\n");
                    break;

                case 2:
                    printf("  Sub 1-2\n");
                    break;

                default:
                    printf("  Sub 1-default\n");
                    break;
            }

            printf("-- fin subcategorías de 1 --\n");
            break;

        case 2:
            printf("Categoría 2 (sin subcategorías)\n");
            break;

        default:
            printf("Categoría default\n");
            break;
    }

    printf("-- fin de analizar() --\n\n");
}

int main(void)
{
    /* Cambiá estos pares de valores para probar todas las ramas */
    analizar(1, 2);   /* ← debería entrar en Sub 1-2 */
    analizar(0, 0);   /* ← categoría sin sub */
    analizar(1, 9);   /* ← sub-categoría default */
    analizar(3, 0);   /* ← categoría default    */
    return 0;
}
