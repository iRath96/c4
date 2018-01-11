int printf(char *, ...);
void *malloc(int);

void conflict() {
	printf("this function will never be called.\n");
}

void dump(int i) {
	printf("i = %d\n", i);
}

int main(void) {
	int i = 0;
	while (i < 10) {
		if (i == 0) {
			dump(i);
			if (i == 1) conflict();
			if (i > 0) conflict();
		} else if (i == 1) {
			if (i < 4) dump(1);
		} else if (i > 0 && i < 4) {
			if (i == 1) conflict();
			if (i == 2) dump(2);
			if (i == 3) dump(3);
		}

		if (i == 4 || i == 5 || i == 6) {
			if (i == 4) dump(4);
			if (i == 5) dump(5);
		} else if (i == 4) conflict();

		if (i >= 6 && i <= 6) dump(6);
		else if (i == 6) conflict();

		if (i > 6) {
			if (i > 7) {
				if (i > 8) {
					if (i < 7) conflict();
					if (i > 9) conflict();
					else dump(9);
					if (i == 0) conflict();
				} else dump(8);
			} else dump(7);
		}

		++i;
	}
}
