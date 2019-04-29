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
  print(p.x);
  print(p.y);
  return 0;
}
