int cond(bool b)
{
  int x;
  x = 10;
  if (b)
    if (x == 10)
      x = 42;
  else
    x = 17;
  return x;
}

int main()
{
 printf("%d\n", cond(true));
 printf("%d\n", cond(false));
 return 0;
}
