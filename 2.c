#include <stdio.h>

void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int main() {
    int x = 3, y = 7;
    int *p1 = &x;
    int *p2 = &y;
    swap(p1, p2);
    printf("%d %d\n", x, y); // Debe imprimir 7 3
    return 0;
}
