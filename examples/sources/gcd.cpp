int euclides_gcd(int a, int b) {
    if (b == 0)
        return a;
    return euclides_gcd(b, a % b);
}
