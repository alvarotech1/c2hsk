/* switch_nested_in_main.c */


int main(void)
{
    int categoria   = 1;    /* cambiá estos valores para ver cada rama */
    int subCategoria = 2;

    switch (categoria) {

        case 0:
            printf("Categoría 0 (sin subcategorías)\n");
            break;

        case 1:             /* ---------- anidado ---------- */
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

    printf("-- fin de main() --\n");
    return 0;
}
