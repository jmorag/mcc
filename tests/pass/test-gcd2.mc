int gcd(int a, int b) {
  while (a != b)
    if (a > b) a = a - b;
    else b = b - a;
  return a;
}

int main()
{
  printf("%d\n", gcd(14,21));
  printf("%d\n", gcd(8,36));
  printf("%d\n", gcd(99,121));
  return 0;
}
