void inc(int *x) {
    *x = *x + 1;
}

int main() {
    int a = 10;
    inc(&a);
    printf("%d\n", a); // Debe imprimir 11
    return 0;
}
