int main()
{
  int *p;
  int *q;
  p = (int *)malloc(4 * 5); /* Hardcode 4 for sizeof(int) */
  for (q = p; q - p < 5; q = q + 1) {
    print(q - p);
  }
  free((void *)p);
  return 0;
}
