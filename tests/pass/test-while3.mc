int main()
{
  while (true) {
    printf("Once\n");
    return 0;
  }
  // Although it's obvious that control never reaches here
  // our analysis isn't smart enough to detect that.
  return 0;
}
