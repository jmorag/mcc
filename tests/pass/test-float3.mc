void testfloat(float a, float b)
{
  printf("%g\n", a + b);
  printf("%g\n", a - b);
  printf("%g\n", a * b);
  printf("%g\n", a / b);
  printf("%d\n", a == b);
  printf("%d\n", a == a);
  printf("%d\n", a != b);
  printf("%d\n", a != a);
  printf("%d\n", a > b);
  printf("%d\n", a >= b);
  printf("%d\n", a < b);
  printf("%d\n", a <= b);
}

int main()
{
  float c;
  float d;

  c = 42.0;
  d = 3.14159;

  testfloat(c, d);

  testfloat(d, d);

  return 0;
}
