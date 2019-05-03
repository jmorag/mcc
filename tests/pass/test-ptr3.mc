int main()
{
  int i;
  int *n; int *m;
  float *f; float *g;
  bool *b; bool *c;
  n = (int *)malloc(10 * sizeof(int));
  f = (float *)malloc(10 * sizeof(float));
  b = (bool *)malloc(10 * sizeof(bool));

  m = n + 9;
  g = f + 9;
  c = b + 9;

  printf("%d\n", (m - n == g - f) && (g - f == c - b));
  free((void *)n);
  free((void *)f);
  free((void *)b);

  return 0;
}
