#include <stdio.h>
long a = 679, b = 771, i, n;
long A() { do { a = a * 16807 % 2147483647; } while (a&3); return a; }
long B() { do { b = b * 48271 % 2147483647; } while (b&7); return b; }
int main() { for (; i < 5000000; i++) n += !(short)(A() ^ B()); printf("%ld\n", n); }
