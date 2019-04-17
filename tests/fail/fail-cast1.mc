int main()
{
  int *p;
  /* Shouldn't be able to assign void * to int * without casting.*/
  p = malloc(24);
  return 0;
}
