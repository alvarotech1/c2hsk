int main() {
    int x = 3;
    int *p1 = &x;
    int *p2 = &x;

    *p1 = 100;
    printf("%d\n", *p2); // 100
}
