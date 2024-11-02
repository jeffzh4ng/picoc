int main() {
    // func foo = f
    // c does not have function types (values)
    return f(9);
}

int f(int x) {
    return g(x) + 10;
}

int g(int y) {
    return y + 11;
}