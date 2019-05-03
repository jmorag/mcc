int main()
{
  int *p;
  int *q;
  p = (int *)malloc(sizeof(int) * 5);
  for (q = p; q - p < 5; q = q + 1) {
    printf("%d\n", q - p);
  }
  free((void *)p);
  return 0;
}
