def expression(rbp=0):
    global token
    t = token
    token = next_c()
    left = t.nud()
    while rbp < token.lbp:
        t = token
        token = next_c()
        left = t.led(left)

    return left

class literal_token(object):
    lbp = 0
    def __init__(self, value):
        self.value = int(value)
    def nud(self):
        return self.value

class operator_add_token(object):
    lbp = 10
    def led(self, left):
        right = expression(10)
        return left + right

class operator_mul_token(object):
    lbp = 20
    def led(self, left):
        return left * expression(20)

class end_token(object):
    lbp = 0

import re
token_pat = re.compile("\s*(?:(\d+)|(.))")

def tokenize(program):
    print(token_pat.findall(program))
    for number, operator in token_pat.findall(program):
        if number:
            yield literal_token(number)
        elif operator == "+":
            yield operator_add_token()
        elif operator == "*":
            yield operator_mul_token()
        else:
            raise SyntaxError('unknown operator: %s', operator)
    yield end_token()

def parse(program):
    global token, next_c
    tt = tokenize(program)
    next_c = lambda: next(tt)
    token = next_c()
    return expression()

print(parse('3 + 1 * 2 * 4 + 5'))