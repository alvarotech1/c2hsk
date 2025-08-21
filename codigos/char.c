int main (){
    char perro[5];

    perro[0] = 'a';
    perro[2] = 'b';

    for(int i = 0; i < 5; i++){
        printf("ARRAY POSS %d: %d", i, perro[i]);
    }
    
    perro[0] = 't';
    for(int i = 0; i < 5; i++){
        printf("ARRAY POSS %d: %d", i, perro[i]);
    }

}
