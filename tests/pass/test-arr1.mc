int main() {
    int i;
    int *p;
    p = (int *)malloc(sizeof(int) * 10);
    for (i = 0; i < 10; i = i + 1) {
        *(p + i) = i;
        print(*(p + i));
    }
    free((void *)p);
    return 0;
}
