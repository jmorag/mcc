int main()
{
  int *p;
  int *q;
  p = alloc_ints(5);
  for (q = p; q - p < 5; q = q + 1) {
    print(q - p);
  }
  return 0;
}
