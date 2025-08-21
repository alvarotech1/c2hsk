#include <stdio.h>

int sumarCartas(int carta1, int carta2) {
    int total = carta1 + carta2;
    return total;
}

int main() {
    int cartaJugador1 = 0;
    int cartaJugador2 = 0;
    int totalJugador = 0;
    int cartaCrupier1 = 7;
    int cartaCrupier2 = 8;
    int totalCrupier = 0;
    int eleccion = 0;

    printf("Ingrese su primera carta: ");
    scanf("%d", &cartaJugador1);

    printf("Ingrese su segunda carta: ");
    scanf("%d", &cartaJugador2);

    totalJugador = sumarCartas(cartaJugador1, cartaJugador2);
    printf("Total jugador: %d\n", totalJugador);

    printf("Desea otra carta? (1=si, 0=no): ");
    scanf("%d", &eleccion);

    if (eleccion == 1) {
        int nuevaCarta = 0;
        printf("Ingrese nueva carta: ");
        scanf("%d", &nuevaCarta);
        totalJugador = totalJugador + nuevaCarta;
        printf("Nuevo total: %d\n", totalJugador);
    }

    if (totalJugador > 21) {
        printf("Te pasaste de 21. Perdiste.\n");
    } else {
        totalCrupier = sumarCartas(cartaCrupier1, cartaCrupier2);
        printf("Total crupier: %d\n", totalCrupier);

        if (totalJugador > totalCrupier) {
            printf("Ganaste!\n");
        } else if (totalJugador < totalCrupier) {
            printf("Perdiste!\n");
        } else {
            printf("Empate!\n");
        }
    }

    return 0;
}
