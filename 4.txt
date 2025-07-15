void ponerdos(int **pp) {
    **pp = 2;
}

int main() {
    int x = 9;
    int *p = &x;
    ponerdos(&p);
    printf("%d\n", x); // Debe imprimir 2
    return 0;
}
