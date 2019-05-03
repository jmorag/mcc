int main() {
    int i;
    int *p;
    i = 10;
    p = &i;
    *p = 1;
    printf("%d\n", i);
    return 0;
}
