#include <stdbool.h>

bool is_prime(int num) {
    if (num <= 1) return false;
    if (num < 4 || num % 2 == 0) return num < 4;
    for (int i = 2; (i + 1) * (i + 1) <= num; i++) {
        if (num % (i + 1) == 0) return false;
    }
    return true;
}
