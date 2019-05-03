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
  char *string;
  int i;
  string = "Hello world!";
  printf("%s\n", string);
  printf("%d\n", strlen(string));

  return 0;
}
