int printf(char *, ...);

int main(void) {
	printf("test 1\n");
	goto hi;
	bye:
	printf("test 3\n");
	return 0;
	printf("fail\n");
	hi:
	printf("test 2\n");
	goto bye;
}
