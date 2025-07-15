#include <stdio.h>


void modificar_doble(int *p){

    *p = *p * 2;

}


int main() {

    int a = 2;

    int *p = &a;

    printf("%d\n", *p);  // 2

    modificar_doble(p);

    printf("%d\n", *p); 

}

