int main(int a, int b) {
  int X = 50;
  while ((X < 100) || X > 1 && true) {
    if (X > 85) {
      X = X - 25;
    } else {
      X = X + 14;
    }
  }
  return 0;
}

bool test() {
  int a = 10;
  int b = 20;
  int c = 30;
  return a + b + c > c + b + a;
}
