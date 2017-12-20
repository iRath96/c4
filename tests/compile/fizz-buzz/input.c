/*
 * This tests control flow.
 */

int printf(char *, ...);

int main(void) {
	int i;

	while (i <= 20) {
		if (i % 3 == 0) printf("Fizz");
		if (i % 5 == 0) printf("Buzz");
		if ((i % 3 != 0) && (i % 5 != 0)) printf("number=%d", i);
		else printf("!");
		printf("\n");

		++i;
	}

	return 0;
}