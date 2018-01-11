int printf(char *, ...);
void *malloc(int);

int main(void) {
	int n = 4;
	int *a = malloc(sizeof(int) * n);
	int *b = a + n;
	int *i = a;

	a[0] = 1;
	a[1] = 2;
	a[2] = 3;
	a[3] = 4;

	while (i < b) {
		printf("%d: %d\n", i - a, *i);
		++i;
	}

	printf("last: %x = %d\n", (b - 1) - a, *(b - 1));
}
