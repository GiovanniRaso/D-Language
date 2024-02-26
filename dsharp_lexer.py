import ply.lex as lex
from dsharp_tokens import tokens



t_ASSIGN = r'='
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MODULUS = r'%'
t_EQUAL = r'=='
t_NOT_EQUAL = r'!='
t_GREATER_THAN = r'>'
t_LESS_THAN = r'<'
t_GREATER_THAN_OR_EQUAL = r'>='
t_LESS_THAN_OR_EQUAL = r'<='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'
t_LPREN = r'\('
t_RPREN = r'\)'
t_LEFT_CURLY_BRACE = r'\{'
t_RIGHT_CURLY_BRACE = r'\}'
t_LEFT_SQUARE_BRACKET = r'\['
t_RIGHT_SQUARE_BRACKET = r'\]'
t_COMMA = r','
t_SEMICOLON = r';'
t_COLON = r':'
t_DOT = r'\.'
t_RIGHT_ARROW = r'->'
t_LEFT_ARROW = r'<-'

reserved = {
    'func': 'FUNCTION',
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'while': 'WHILE',
    'return': 'RETURN',
    'var': 'VAR',
    'const': 'CONST',
    'true': 'TRUE',
    'false': 'FALSE',
}


def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_STRING(t):
    r'\".*?\"|\'\.*?\''
    t.value = t.value[1:-1]  # remove quotes
    return t

def t_BOOLEAN(t):
    r'\btrue\b|\bfalse\b'
    t.value = True if t.value == 'true' else False
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore  = ' \t'

def t_RESERVED(t):
    r'\b(?:func|if|else|for|while|return|var|const|true|false)\b'
    t.type = reserved.get(t.value,'IDENTIFIER')    # Check for reserved words
    return t

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