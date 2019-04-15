int main() {
    int i;
    int *p;
    i = 10;
    p = &i;
    *p = 1; /* i = 1 */
    print(i);
    return 0;
}
