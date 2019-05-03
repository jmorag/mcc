struct point
{
  int x;
  int y;
};

int main()
{
  struct point p;
  p.x = 10;
  p.y = 11;
  printf("%d\n", p.x);
  printf("%d\n", p.y);
  return 0;
}
