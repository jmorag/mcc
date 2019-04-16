int main() {
    int i;
    int *p;
    p = alloc_ints(10);
    for (i = 0; i < 10; i = i + 1) {
        *(p + i) = i;
        print(*(p + i));
    }
    return 0;
}
