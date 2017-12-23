int printf(char *, ...);
void *malloc(int);

int main(void) {
	int nPrimes = 20;
	int nFound = 0;

	int *primes = malloc(sizeof(int) * nPrimes);

	int test = 2;
	while (nFound < nPrimes) {
		int j = 0;
		while (j < nFound) if (test % primes[j++] == 0) goto noPrime;

		primes[nFound++] = test;
		printf("prime: %d\n", test);

	noPrime:
		++test;
	}
}
