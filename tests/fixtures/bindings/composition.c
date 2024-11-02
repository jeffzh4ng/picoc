int main() {
    return f();
}

int f() {
    return 9 + g();
}

int g() {
    return 10 + h();
}

int h() {
    return 11;
}