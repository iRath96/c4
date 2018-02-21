int printf(char *, ...);

int read() { return 42; }

int write(int num) {
	printf("%d\n", num);
	return num;
}

int main(void) {
	write(read() ? write(0) : write(1));
	write(1 - (1 && 0 || 1 ? 1 : 0));
}
