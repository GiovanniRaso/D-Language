# dsharp_parser.py
import ply.yacc as yacc
from dsharp_lexer import tokens

# Precedence and associativity of operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),  # Unary minus operator
)

# Grammar rules
def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if p[2] == '+': p[0] = f"(+ {p[1]} {p[3]})"
    elif p[2] == '-': p[0] = f"(- {p[1]} {p[3]})"
    elif p[2] == '*': p[0] = f"(* {p[1]} {p[3]})"
    elif p[2] == '/': p[0] = f"(/ {p[1]} {p[3]})"

def p_expression_group(p):
    "expression : LPREN expression RPREN"
    p[0] = f"(group {p[2]})"

def p_expression_uminus(p):
    "expression : MINUS expression %prec UMINUS"
    p[0] = f"(- {p[2]})"

def p_expression_number(p):
    '''expression : INTEGER
                  | FLOAT'''
    p[0] = str(p[1])

def p_error(p):
    if p:
        print(f"[line {p.lineno}] Error at {p.value}: Expect ')' after expression.")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()
