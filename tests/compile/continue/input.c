int printf(char *, ...);

int main(void) {
	int i = 0;
	while (1) {
		++i;
		if (i >= 20) break;
		printf("< 20: %d\n", i);

		if (i >= 10) continue;
		printf("< 10: %d\n", i);
	}

	printf("= 20: %d\n", i);
}
