
extern int test();

int main() {
    int n = 10;
    int cont = 0;
    for (int i=0; i<n; i++) {
        test();
        cont += i;
    }
}
