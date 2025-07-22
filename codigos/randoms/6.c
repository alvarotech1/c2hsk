int traducir(int codigo) {

    switch (codigo) {
        case 1:{
            return 100;
	}
        case 2: {
            return 200;
	}
        default:{
            return -1;
	}
    }


    printf("nunca\n");
    return 0;
}

int main() {
    int valor = 2;
    int resultado = traducir(valor);

    printf("%d\n", resultado);  // debe imprimir: 200
    return 0;
}