void foo() {}

int bar(int a, bool b, int c) { return a + c; }

int main()
{
  printf("%d\n", bar(17, false, 25));
  return 0;
}
