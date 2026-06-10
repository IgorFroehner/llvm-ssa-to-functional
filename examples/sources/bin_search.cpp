int bin_search(int n) {
    int l = 0, r = n - 1;
    while (l < r) {
        int m = (l + r) / 2;
        if (m * m < n) {
            l = m + 1;
        } else {
            r = m;
        }
    }
    return l;
}

