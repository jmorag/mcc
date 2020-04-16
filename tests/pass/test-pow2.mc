int main() {
    int e;
    int b;
    for (b = 0; b < 10; b = b + 1)
      for (e = 0; e < 10; e = e + 1)
            printf("%d ^ %d = % 10d\n", b, e, b ** e);
    return 0;
}
