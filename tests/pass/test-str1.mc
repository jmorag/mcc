int strlen (char *string) {
  char *c;
  int total;
  total = 0;
  for (c = string; *c != '\0'; c = c + 1) {
    total = total + 1;
  }
  return total;
}

int main()
{
  char *s;
  int i;
  s = "Hello world!";
  prints(s);
  print(strlen(s));

  return 0;
}
