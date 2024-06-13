int fib(int n) {
    int i = 0;
    int j = 1;
    int c = 0;
    if (n >= 100) {
        c = 100;
        i = j * -1;
    } else {
        while (c < n) {
            int aux = j;
            j = j + i;
            i = aux;
            c = c + 1;
        }
    }
    return i;
}
