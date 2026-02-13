struct MyStruct {
  int a;
  int b;
  int c;
  char d;
  float* e;
};


struct MyStruct* my_struct_generator(int a, int b, int c, int d, float* e);

void pint_struct(struct MyStruct* point); 
