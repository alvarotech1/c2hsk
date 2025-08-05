int main() {
    int secreto = 7; // número fijo (no hay rand())
    int intento;
    int intentosRestantes = 5;

    while (intentosRestantes > 0) {
        printf("Adivina el número (1-20): ");
        scanf("%d", &intento);

        if (intento == secreto) {
            printf("¡Correcto!\n");
            return 0;
        } else {
            printf("Incorrecto\n");
            intentosRestantes = intentosRestantes - 1;
            printf("Intentos restantes: %d\n", intentosRestantes);
        }
    }

    printf("¡Perdiste! El número era %d\n", secreto);
}

