int printf(char *, ...);
int num() { return 42; }

int main(void) {
	int x = num(), y = num();

	if (x > y) x = y;
	else if (x < y) x = y;
	else printf("easy: %d\n", x - y); // will always be zero

	//printf("hard: %d\n", x - y);
}
