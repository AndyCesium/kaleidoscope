extern putchard(char);

def printstar(n)
  for i = 1, i < n, 1.0 in
    putchard(42);  # ascii 42 = '*'

# print 5 '*' characters
printstar(5);