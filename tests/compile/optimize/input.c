int printf(char *, ...);

int main(void) {
	int x = 4, y = 8;
	int z = x + y;

	if (x < y) x = y;
	else if (x > y) x = y;

	if (x < y) {
		printf("fail\n");
		y = 100;
	}

	printf("%d %d %d\n", x, y, z);
}
