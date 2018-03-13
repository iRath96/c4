int printf(char *, ...);

int num() { return 42; }

int main(void) {
	printf("%d\n",
		num() > 100 ? (num() ? 10 : 20) : num() && num()
	);
}
