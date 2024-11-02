int main() {
    return f(9);
}

int f(int x) {
    return x + g(10);
}

int g(int y) {
    return y + 11 + h();
}

int h(void) {
    return 12;
}