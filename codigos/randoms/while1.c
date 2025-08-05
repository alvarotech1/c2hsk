int main() {
    int i = 0;
    while (i < 5) {
        i++;           // efecto válido
        printf("X\n"); // efecto válido
        break;         // corta el bucle
    }
    printf("%d\n", i); // debe imprimir 1
}

