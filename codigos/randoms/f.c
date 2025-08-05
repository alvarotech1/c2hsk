#include <stdio.h>



int doble(int x){
    while(x < 5){
    printf("CACA");
    x++;
    }
    return 25;
}

int main(){
    int x = 0;

    int resultado = doble(x);

    printf("\nTERRIBLE RESULTADO PA: %d\n", resultado);
}
