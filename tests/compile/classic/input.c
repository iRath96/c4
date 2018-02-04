int printf(char *, ...);

int dn;
int dontknow() {
  return dn--;
}

int read() {
  return 0;
}

int main(void) {
  dn = 5;

	int x = 1;
	int z = read();
	int y = z;

	while (dontknow()) {
		if (y != z)
			x = 2;
		x = 2 - x;
		if (x != 1)
			y = 2;
	}

	printf("%d == 1\n", x);
}
