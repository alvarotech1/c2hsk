struct Jugador {
    int salud;
    int energia;
};

int main() {
    struct Jugador j;
    j.salud = 100;
    j.energia = 50;

    printf("Salud inicial: %d\n", j.salud);
    printf("Energia inicial: %d\n", j.energia);

    // El jugador recibe daño
    j.salud = j.salud - 30;

    // El jugador usa energía para correr
    j.energia = j.energia - 10;

    printf("Salud restante: %d\n", j.salud);
    printf("Energia restante: %d\n", j.energia);
}
