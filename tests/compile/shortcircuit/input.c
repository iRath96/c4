int printf(char *, ...);

int num(void) {
  return 42;
}

int main(void) {
	int a = num(), b = num();
	if (a >= b && a <= b) {
		printf("%d\n", a - b);
	}
}
