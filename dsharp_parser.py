import ply.yacc as yacc
from dsharp_lexer import tokens

# Symbol table to store variable values
symbol_table = {}

# Precedence rules for arithmetic operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),  # Unary minus operation
)

def p_program(p):
    '''program : statement
               | program statement'''
    if len(p) == 2:  # Single statement
        p[0] = [p[1]]
    else:  # Multiple statements, append statement to program list
        p[0] = p[1] + [p[2]]

def p_statement(p):
    '''statement : var_declaration
                 | expression'''
    p[0] = p[1]

# Corrected Variable declaration and assignment rule name
def p_var_declaration(p):
    'var_declaration : VAR IDENTIFIER ASSIGN expression SEMICOLON'
    symbol_table[p[2]] = p[4]
    p[0] = p[4]  # This could return the variable's value, or just return None if you don't need to output it here

# Handling expressions including arithmetic and variables
def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if p[2] == '+': p[0] = p[1] + p[3]
    elif p[2] == '-': p[0] = p[1] - p[3]
    elif p[2] == '*': p[0] = p[1] * p[3]
    elif p[2] == '/': p[0] = p[1] / p[3]

# Handling negative numbers and subtraction
def p_expression_uminus(p):
    "expression : MINUS expression %prec UMINUS"
    p[0] = -p[2]

# Handling grouped expressions (parentheses)
def p_expression_group(p):
    "expression : LPREN expression RPREN"
    p[0] = p[2]

# Handling numbers (integers and floats)
def p_expression_number(p):
    '''expression : INTEGER
                  | FLOAT'''
    p[0] = p[1]

# Handling identifiers (variable names) in expressions
def p_expression_identifier(p):
    "expression : IDENTIFIER"
    if p[1] in symbol_table:
        p[0] = symbol_table[p[1]]
    else:
        print(f"Undefined variable '{p[1]}'.")
        p[0] = 0  # Or consider raising an error or stopping execution

# Error rule for syntax errors
def p_error(p):
    if p:
        print(f"[line {p.lineno}] Error at '{p.value}': Syntax error.")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()
