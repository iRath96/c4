void *memcpy(void *dst, void *src, int n);
void *malloc(int n);
int printf(char *, ...);
int strcmp(char *, char *);

struct sort_args {
	void *array;
	void *tmp;
	int element_size;
	int count;
	int (*callback)(struct sort_args *, int, int); // lower than
};

int partition(struct sort_args *, int, int);
void quicksort3(struct sort_args *, int, int);
void quicksort(struct sort_args *args, int (*cb)(struct sort_args *, int, int)) {
	args->callback = cb;
	quicksort3(args, 0, args->count - 1);
}

void quicksort3(struct sort_args *args, int lo, int hi) {
	if (lo < hi) {
		int p = partition(args, lo, hi);
		quicksort3(args, lo, p - 1);
		quicksort3(args, p + 1, hi);
	}
}

void swap(struct sort_args *args, int a, int b) {
	if (a == b) return;

	int r = args->element_size;
	memcpy(args->tmp, args->array + a*r, args->element_size);
	memcpy(args->array + a*r, args->array + b*r, args->element_size);
	memcpy(args->array + b*r, args->tmp, args->element_size);
}

int partition(struct sort_args *args, int lo, int hi) {
	int pivot = hi;
	int i = lo;
	int j = lo;

	while (j < hi) {
		if (args->callback(args, j, pivot))
			swap(args, i++, j);
		++j;
	}

	if (args->callback(args, hi, i))
		swap(args, hi, i);

	return i;
}

void alloc_sort_args(struct sort_args *args, int element_size, int n) {
	args->array = malloc(element_size * n);
	args->tmp = malloc(element_size);
	args->element_size = element_size;
	args->count = n;
	args->callback = 0;
}

struct kitten {
	char *name;
	int age;
};

void create_kitten(struct sort_args *args, int i, char *name, int age) {
	struct kitten *k = &((struct kitten *)args->array)[i];
	k->name = name;
	k->age = age;
}

int cmp_kitten_age(struct sort_args *args, int a, int b) {
	struct kitten *k = args->array;
	return k[a].age < k[b].age;
}

int cmp_kitten_name(struct sort_args *args, int a, int b) {
	struct kitten *k = args->array;
	return strcmp(k[a].name, k[b].name) < 0;
}

void inspect_kittens(struct sort_args *args) {
	int i = 0;
	while (i < args->count) {
		struct kitten *k = &((struct kitten *)args->array)[i++];
		printf("#%2d: %s (%d)\n", i, k->name, k->age);
	}
}

void swap(struct sort_args *, int, int);
void reverse(struct sort_args *args) {
	int a = 0;
	int b = args->count - 1;
	while (a < b) {
		swap(args, a++, b--);
	}
}

int main(void) {
	struct sort_args s;
	alloc_sort_args(&s, sizeof(struct kitten), 10);

	create_kitten(&s, 0, "Alex", 21);
	create_kitten(&s, 1, "Anika", 17);
	create_kitten(&s, 2, "Elvis", 83);
	create_kitten(&s, 3, "Christian", 27);
	create_kitten(&s, 4, "Anastasia", 21);
	create_kitten(&s, 5, "Steve", 63);
	create_kitten(&s, 6, "Mario", 24);
	create_kitten(&s, 7, "Speck", 9);
	create_kitten(&s, 8, "Olaf", 1);
	create_kitten(&s, 9, "Sensei", 100);

	printf("original:\n");
	swap(&s, 0, 1);
	inspect_kittens(&s);

	quicksort(&s, cmp_kitten_age);

	printf("\nby age:\n");
	inspect_kittens(&s);

	quicksort(&s, &cmp_kitten_name);

	printf("\nby name:\n");
	inspect_kittens(&s);

	reverse(&s);

	printf("\nreversed:\n");
	inspect_kittens(&s);
}
