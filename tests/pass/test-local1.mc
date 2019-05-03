void foo(bool i)
{
  int i; /* Should hide the formal i */

  i = 42;
  printf("%d\n", i + i);
}

int main()
{
  foo(true);
  return 0;
}
