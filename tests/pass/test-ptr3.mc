int main()
{
  int i;
  int *n; int *m;
  float *f; float *g;
  bool *b; bool *c;
  n = (int *)malloc(10 * 4);
  f = (float *)malloc(10 * 8);
  b = (bool *)malloc(10);

  m = n + 9;
  g = f + 9;
  c = b + 9;

  printb((m - n == g - f) && (g - f == c - b));
  free((void *)n);
  free((void *)f);
  free((void *)b);

  return 0;
}
