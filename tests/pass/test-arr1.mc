int main() {
    int i;
    int *p;
    p = (int *)malloc(4*10); /* Hardcode 4 for sizeof(int). */
    for (i = 0; i < 10; i = i + 1) {
        *(p + i) = i;
        print(*(p + i));
    }
    free((void *)p);
    return 0;
}
