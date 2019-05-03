int cond(bool b)
{
  int x;
  if (b)
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
