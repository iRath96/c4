/*
 * This tests recursion.
 */

int printf(char *, ...);

int fib(int i) {
	if (i <= 2) return i;
	return fib(i-1) + fib(i-2);
}

int main() {
	printf("fib(10): %d\n", fib(10));
}
