unsigned int x = 4, **y;

struct example { int x; };
struct example var;

int (*functionPtr)(int, int);
int (*getFunc(void))(int, int) {}
int (*test(int x, int y))(int, int) {
}

int (*(*wtf)(int))(int) = (void *)&x;

struct hello {
    struct { int inception; } rules;
} kitty;

int b = sizeof(int);
int c = sizeof(int) { 0 };

int fn(int)(void);
int say(char *);

char *kitten(char *hi)
int a;
int b;
int c, d, e;
int *(*array)(int, int, int);
int do_something, hammertime;
struct { int right(int); } *are_nice;
int **assignments;
{
    a ? b : c ? d : e; // a ? b : (c ? d : e)
    
    /*
        the following statement is valid,
        but wtf does it mean?
    */
    (int (*)(int *(*), int x)) 0;

    int cc = 1 << 8;
    4 + 2 * 3;
    4 + 4 << 8;
    (*(array[1](1,2,3)) << 8 || 1 + ++a);
    fn(&c + 1)();
    
    sizeof(int) { [2] = 2 };
    
    label: {
        42;
    }
    
    while (1) {
        goto label;
        break;
        continue;
    }
    
    ;;;
    
    if (1) if (1) do_something; else if ("hi") {
        stop: hammertime;
    }
    
    while (nobody_is_watching) {
        say("I'm Groot");
        *assignments[1] = are_nice->right(1);
    }
    
    return ((("nope")));
}
