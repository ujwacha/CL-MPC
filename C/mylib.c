#include <stdio.h>
#include <stdlib.h>
#include "mylib.h"


struct MyStruct* my_struct_generator(int a, int b, int c, int d, float* e)
{
  struct MyStruct* pointer = (struct MyStruct*) malloc(sizeof(struct MyStruct));
  pointer->a = 0;
  pointer->b = 0;
  pointer->c = 0;
  pointer->d = 0;
  pointer->e = (float *) malloc(sizeof(float));

  return pointer;
}



void pint_struct(struct MyStruct* point)
{
  printf("{%i, %i, %i, %i, %f}", point->a, point->b, point->c, point->d, point->e);
} 
