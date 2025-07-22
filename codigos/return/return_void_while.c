/* testB_return_in_while_nested.c */

void nestedSwitch(int cat, int k)
{
    switch (cat) {
        case 1:
            printf("cat 1\n");
            switch (k) {
                case 5:
                    printf("  k=5, entro al while\n");
                    int j = 0;
                    while (1 == 1) {
                        printf("    j=%d\n", j++);
                        return;             /* ← corta TODO nestedSwitch */
                    }
                    printf("  nunca\n");
                    break;
                default:
                    printf("  sub default\n");
            }
            printf("fin cat 1\n");
            break;

        default:
            printf("cat default\n");
    }
    printf("-- fin de nestedSwitch()\n");
}

int main(void)
{
    nestedSwitch(1, 5);   /* while con return; */
    nestedSwitch(1, 9);   /* sin return; */
    return 0;
}
