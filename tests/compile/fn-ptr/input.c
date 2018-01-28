int printf(char *, ...);

int a() { return 1; }
int b() { return 2; }

int (*fn)(void);

int main(void) {
	fn = &a;
	printf("%d\n", fn());
	fn = b;
	printf("%d\n", (*fn)());
}
