void suma_y_resta(int x, int y, int *suma, int *dif) {
    *suma = x + y;
    *dif = x - y;
}

int main() {
    int a = 8, b = 5, s, d;
    suma_y_resta(a, b, &s, &d);
    printf("%d %d\n", s, d); // Debe imprimir 13 3
    return 0;
}
