/*
 * This tests data structures.
 */

int printf(char *, ...);
void *malloc(int);
void free(void *);

struct Node {
  int value;
  struct Node *next;
};

struct List {
  struct Node *head;
  struct Node **tail;
};

struct List *create_list() {
  struct List *result = malloc(sizeof(struct List));
  result->head = 0;
  result->tail = &result->head;
  return result;
}

void insert(struct List *list, int number) {
  *(list->tail) = malloc(sizeof(struct Node));
  (*list->tail)->value = number;
  (*list->tail)->next = 0;

  list->tail = &(*list->tail)->next;
}

int sum(struct List *list) {
  int sum = 0;
  struct Node *node = list->head;

  while (node) {
    sum = sum + node->value;
    node = node->next;
  }

  return sum;
}

int main() {
  struct List *list = create_list();

  insert(list, 1);
  insert(list, 2);
  insert(list, 3);

  printf("sum: %d\n", sum(list));
}
