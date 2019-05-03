int gcd(int a, int b) {
  while (a != b) {
    if (a > b) a = a - b;
    else b = b - a;
  }
  return a;
}

int main()
{
  printf("%d\n", gcd(2,14));
  printf("%d\n", gcd(3,15));
  printf("%d\n", gcd(99,121));
  return 0;
}
