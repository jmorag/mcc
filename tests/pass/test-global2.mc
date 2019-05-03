bool i;

int main()
{
  int i; /* Should hide the global i */

  i = 42;
  printf("%d\n", i + i);
  return 0;
}
