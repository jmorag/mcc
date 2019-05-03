int fib(int x)
{
  if (x < 2) return 1;
  return fib(x-1) + fib(x-2); 
}

int main()
{
  printf("%d\n", fib(0));
  printf("%d\n", fib(1));
  printf("%d\n", fib(2));
  printf("%d\n", fib(3));
  printf("%d\n", fib(4));
  printf("%d\n", fib(5));
  return 0;
}
