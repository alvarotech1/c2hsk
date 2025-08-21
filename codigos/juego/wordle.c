int main() {
    char secreto[5] = "gato"; // palabra secreta + \0
    char intento[5];
    int intentosRestantes = 6;
    int acierto = 0;

    while (intentosRestantes > 0 && acierto == 0) {
        printf("Intentá adivinar la palabra de 4 letras: ");
        scanf("%s", intento);

        int correctas = 0;

        // mostrar estado letra por letra
        for (int i = 0; i < 4; i++) {
            if (intento[i] == secreto[i]) {
                printf("✓ ");
                correctas = correctas + 1;
            } else {
                printf("_ ");
            }
        }

        printf("\n");

        if (correctas == 4) {
            acierto = 1;
            printf("¡Ganaste!\n");
        } else {
            intentosRestantes = intentosRestantes - 1;
            printf("Intentos restantes: %d\n", intentosRestantes);
        }
    }

    if (acierto == 0) {
        printf("¡Perdiste! La palabra era %s\n", secreto);
    }
}

