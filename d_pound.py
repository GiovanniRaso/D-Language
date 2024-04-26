import ply.lex as lex
import ply.yacc as yacc

tokens = (
    'INTEGER', 'FLOAT', 'STRING', 'BOOLEAN',
    'IDENTIFIER',
    'FUNCTION', 'IF', 'ELSE', 'FOR', 'WHILE', 'RETURN', 'VAR', 'CONST', 'TRUE', 'FALSE',
    'ASSIGN', 'PRINT',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULUS',
    'EQUAL', 'NOT_EQUAL', 'GREATER_THAN', 'LESS_THAN', 'GREATER_THAN_OR_EQUAL', 'LESS_THAN_OR_EQUAL',
    'AND', 'OR', 'NOT',
    'LPAREN', 'RPAREN',
    'LEFT_CURLY_BRACE', 'RIGHT_CURLY_BRACE',
    'LEFT_SQUARE_BRACKET', 'RIGHT_SQUARE_BRACKET',
    'COMMA', 'SEMICOLON', 'COLON', 'DOT',
    'RIGHT_ARROW', 'LEFT_ARROW'
)


t_PLUS    = r'add'
t_MINUS   = r'sub'
t_TIMES   = r'mult'
t_DIVIDE  = r'div'
t_MODULUS = r'mod'
t_ASSIGN  = r'is'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LEFT_CURLY_BRACE = r'\{'
t_RIGHT_CURLY_BRACE = r'\}'
t_LEFT_SQUARE_BRACKET = r'\['
t_RIGHT_SQUARE_BRACKET = r'\]'
t_COMMA  = r','
t_SEMICOLON = r';'
t_COLON = r':'
t_DOT = r'\.'
t_EQUAL = r'equal'
t_NOT_EQUAL = r'nequal'
t_GREATER_THAN = r'gt'
t_LESS_THAN = r'lt'
t_GREATER_THAN_OR_EQUAL = r'gte'
t_LESS_THAN_OR_EQUAL = r'lte'
t_AND = r'and'
t_OR = r'or'
t_NOT = r'not'


def t_FlOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1] 
    return t

def t_BOOLEAN(t):
    r'True|False'
    t.value = True if t.value == 'True' else False
    return t

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')
    return t

reserved = {
    "if" : "IF",
    "else" : "ELSE",
    "for" : "FOR",
    "while" : "WHILE",
    "function" : "FUNCTION",
    "return" : "RETURN",
    "var" : "VAR",
    "const" : "CONST",
    "print" : "PRINT",
    "true" : "TRUE",
    "false" : "FALSE"
}

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)
    
lexer = lex.lex()


# Parser

def p_program(p):
    'program : statement_list'
    p[0] = ('program', p[1])

def p_statement_list(p):
    '''
    statement_list : statement_list statement
                   | statement
    '''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_statement(p):
    '''
    statement : expression_statement
              | compound_statement
              | selection_statement
              | iteration_statement
              | assignment_statement
              | declaration_statement
    '''
    p[0] = p[1]

def p_expression_statement(p):
    'expression_statement : expression SEMICOLON'
    p[0] = ('expr', p[1])

def p_compound_statement(p):
    'compound_statement : LEFT_CURLY_BRACE statement_list RIGHT_CURLY_BRACE'
    p[0] = ('compound', p[2])

def p_selection_statement(p):
    '''
    selection_statement : IF LPAREN expression RPAREN statement
                        | IF LPAREN expression RPAREN statement ELSE statement
    '''
    if len(p) == 6:
        p[0] = ('if', p[3], p[5])
    else:
        p[0] = ('if-else', p[3], p[5], p[7])

def p_iteration_statement(p):
    '''
    iteration_statement : WHILE LPAREN expression RPAREN statement
                        | FOR LPAREN expression_statement expression_statement expression RPAREN statement
    '''
    if len(p) == 6:
        p[0] = ('while', p[3], p[5])
    else:
        p[0] = ('for', p[3], p[4], p[5], p[7])

def p_assignment_statement(p):
    'assignment_statement : IDENTIFIER ASSIGN expression SEMICOLON'
    p[0] = ('assign', p[1], p[3])

def p_declaration_statement(p):
    '''
    declaration_statement : VAR IDENTIFIER ASSIGN expression SEMICOLON
                          | CONST IDENTIFIER ASSIGN expression SEMICOLON
    '''
    p[0] = ('declare', p[1], p[2], p[4])

def p_expression(p):
    '''
    expression : expression PLUS expression
               | expression MINUS expression
               | expression TIMES expression
               | expression DIVIDE expression
               | INTEGER
               | FLOAT
               | STRING
               | BOOLEAN
               | IDENTIFIER
    '''
    if len(p) == 4:
        p[0] = ('binop', p[1], p[2], p[3])
    else:
        p[0] = ('value', p[1])

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

if __name__ == "__main__":
    while True:
        try:
            s = input('D# > ')
        except EOFError:
            break
        if not s: continue
        result = parser.parse(s)
        print(result)
