int main() {
    int x = 10;
    int *p = &x;
    *p = 77;
    printf("%d\n", x);
}
