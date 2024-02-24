import ply.lex as lex
from dsharp_tokens import tokens

tokens = (
    'IDENTIFIER',
    'INTEGER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
)

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore  = ' \t'

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)
    
lexer = lex.lex()

def tokenize_input(input_text):
    lexer.input(input_text)
    for tok in lexer:
        print(tok)

if __name__ == "__main__":
    while True:
        try:
            s = input('D# > ')
        except EOFError:
            break  # Exit the loop if EOF signal is received (Ctrl+D/Ctrl+Z)
        if not s: 
            continue  # Continue looping if empty input is received
        tokenize_input(s)