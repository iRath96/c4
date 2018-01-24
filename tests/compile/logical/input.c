int printf(char *, ...);

int hello(void) {
	printf("hello!\n");
	return 0;
}

int bye(void) {
	printf("good-bye!\n");
	return 100;
}

int main(int argc, char **argv) {
	int a = hello() && bye();
	int b = hello() || bye();
	int c = !hello();
	int d = !bye();
	int e = !(1 || hello()) && bye();
	int f = !(0 || hello()) && bye();
	int g = ((0 || hello()) + 1) && bye();

	printf("%d %d %d %d %d %d %d\n", a, b, c, d, e, f, g);

	if (!(0 || hello()) && bye()) {
		printf("it works\n");
	} else if (bye()) {
		printf("it does not work\n");
	}

	if ((0 || hello()) + 1)
		printf("nice\n");
}
