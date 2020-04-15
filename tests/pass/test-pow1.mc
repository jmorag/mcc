int main() {
    printf("%g\n", 2.0 ** 4.0);
    printf("%g\n", 2.0 ** 4);
    printf("%g\n", ((float)2) ** 4.0); // bug in combinator. Casting isn't high enough precedence
    return 0;
}
