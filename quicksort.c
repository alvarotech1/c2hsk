
void myQsort (int min, int max, int arr[]){

    if(min < max){
        int contMenores = min;

        int aux = 0;
        int pivote = arr[max];
        for (int i = min; i< max; i++){
            if(pivote > arr[i]){
                aux = arr[contMenores];
                arr[contMenores] = arr[i];
                arr[i] = aux;
                contMenores++;

            }
        }
        aux = arr[contMenores];
        arr[contMenores] = arr[max];
        arr[max] = aux;

        myQsort(contMenores+1,max,arr);
        myQsort(min,contMenores-1,arr);

    }
}
int main (){
    int arr[10] = {12,4,7,3,1,8,9,15,6,2};
    for(int i = 0; i<10; i++){
        printf("%d ", arr[i]);
    }

    printf("\n------------\n");
    myQsort(0,9,arr);
    for(int i = 0; i<10; i++){
        printf("%d ", arr[i]);
    }
    return 0;
}


