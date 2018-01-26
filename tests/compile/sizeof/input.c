int printf(char *, ...);

int main(void) {
	printf("sizeof(int):    %d\n", sizeof(int));
	printf("sizeof(char):   %d\n", sizeof(char));
	printf("sizeof(int *):  %d\n", sizeof(int *));
	printf("sizeof(int **): %d\n", sizeof(int **));
	printf("sizeof(char *): %d\n", sizeof(char *));
	printf("\n");
	printf("sizeof(0):   %d\n", sizeof(0));
	printf("sizeof(1):   %d\n", sizeof(1));
	printf("sizeof(1+1): %d\n", sizeof(1+1));
	printf("sizeof('a'): %d\n", sizeof('a'));
	printf("sizeof(\"a\"): %d\n", sizeof("a"));
	printf("\n");
	printf("sizeof((char)0):     %d\n", sizeof((char)0));
	printf("sizeof(((char)0)+0): %d\n", sizeof(((char)0)+0));
	printf("sizeof('a'+'b'):     %d\n", sizeof(((char)0)+0));
	printf("sizeof('a'*'b'):     %d\n", sizeof('a'*'b'));
	printf("\n");

	char charV;
	int intV;
	printf("sizeof(charV): %d\n", sizeof(charV));
	printf("sizeof(intV):  %d\n", sizeof(intV));
	printf("\n");

	struct A { int a, b; } a;
	struct B { struct A a; struct A *b; } b;

	printf("sizeof(struct A): %d\n", sizeof(struct A));
	printf("sizeof(a):        %d\n", sizeof(a));
	printf("sizeof(struct B): %d\n", sizeof(struct B));
	printf("sizeof(b):        %d\n", sizeof(b));
}
