extern printd(x);

def binary : 1 (x y) 0;  # Low-precedence operator that ignores operands.

printd(123) : printd(456) : printd(789);