int fib(int n) {
    int a = 1, b = 0;
    while (n != 0) {
        int t = a + b;
        b = a;
        a = t;
        n--;
    }
    return a;
}
