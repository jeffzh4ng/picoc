int main() {
    return f(9);
}

int f(int x) {
    return g(x) + 10;
}

int g(int y) {
    return y + 11;
}