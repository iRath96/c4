struct S {
  int x;
  union {
    int a;
    int b;
    struct {
	  char o, p;
      int c, d;
      char r;
	};
  };
  int y;
  //int;
} s;

int main(void) {
	char *i = &s.r;
}
