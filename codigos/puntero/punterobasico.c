int main() {
    int x = 10;
    int* p = &x;
    printf("%d", *p);
    x = 5;
    printf("%d", *p);
}
