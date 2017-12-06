struct x *kitten;

struct x {
    struct x *y;
};

int main(void) {}

struct zz { int o; } *woof(void) {
    { struct z { int o; }; }
    struct z { int o; };
    struct zz w;
}

struct z hi;
