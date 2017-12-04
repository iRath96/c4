int main(void) {
    int x(void);
    
    {
        int *x;
        *x = 2;
    }
    
    x();
}
