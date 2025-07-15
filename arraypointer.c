int main() {
    int arr[3] = {10, 20, 30};
    int *p = &arr[1];  // p apunta al segundo elemento
    *p = 99;           // modificamos arr[1] a través de p
    printf("%d\n", arr[1]);
}
