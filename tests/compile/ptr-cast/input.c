int printf(char *, ...);

int a;

int main(void) {
	((char *)&a)[0] = 1;
	((char *)&a)[1] = 2;
	((char *)&a)[2] = 2;
	((char *)&a)[3] = 1;
	printf("%d\n", a);
}
