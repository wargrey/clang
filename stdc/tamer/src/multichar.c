#include <stdio.h>

int main(int argc, char *argv[]) {
    char u8c = u'x';
    long long sc_int = 'abcd';
    long long bc_int = 'ABCD';
    long long large_int = L'ABCDEFGH';

    fprintf(stdout, "%c\n", u8c);
    fprintf(stdout, "%llx\n", sc_int);
    fprintf(stdout, "%llx\n", bc_int);
    fprintf(stdout, "%llx\n", large_int);

    return 0;
}

