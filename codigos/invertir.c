int main() {
    char palabra[100];
    int len = 0;

    printf("Ingrese una palabra: ");
    scanf("%s", palabra);

    // calcular largo manualmente
    while (palabra[len] != '\0') {
        len = len + 1;
    }

    for (int i = 0; i < len / 2; i++) {
        char tmp = palabra[i];
        palabra[i] = palabra[len - i - 1];
        palabra[len - i - 1] = tmp;
    }

    printf("Palabra invertida: %s\n", palabra);
}

