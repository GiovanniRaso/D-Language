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
t_MINUS   = r'subtract'
t_TIMES   = r'multiply'
t_DIVIDE  = r'divide'
t_MODULUS = r'modulus'
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
t_NOT_EQUAL = r'!='
t_GREATER_THAN = r'>'
t_LESS_THAN = r'<'
t_GREATER_THAN_OR_EQUAL = r'>='
t_LESS_THAN_OR_EQUAL = r'<='
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
    "false" : "FALSE",
    "var" : "VAR"
}

t_ignore = ' \t'

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
    iteration_statement : WHILE LPAREN expression RPAREN compound_statement 
    '''
    p[0] = ('while', p[3], p[5])

def p_assignment_statement(p):
    'assignment_statement : IDENTIFIER ASSIGN expression SEMICOLON'
    p[0] = ('assign', p[1], p[3])

def p_declaration_statement(p):
    '''
    declaration_statement : VAR IDENTIFIER ASSIGN expression SEMICOLON
    '''
    p[0] = ('declare', 'var', p[2], p[4])

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

#Interpreter

memory = {}

def interpret(ast):
    """ Interpret the AST (Abstract Syntax Tree). """
    if ast is None:
        return None
    if type(ast) == tuple:
        op = ast[0]
        if op == 'program':
            return interpret_program(ast[1])
        elif op == 'compound':
            return interpret_compound(ast[1])
        elif op == 'assign':
            return interpret_assign(ast[1], ast[2])
        elif op == 'expr':
            return interpret_expression(ast[1])
        elif op == 'binop':
            return interpret_binop(ast[1], ast[2], ast[3])
        elif op == 'declare':
            return interpret_declare(ast[1], ast[2], ast[3])
        elif op == 'if':
            return interpret_if(ast[1], ast[2])
        elif op == 'if-else':
            return interpret_if_else(ast[1], ast[2], ast[3])
        elif op == 'while':
            return interpret_while(ast[1], ast[2])
        elif op == 'value':
            if isinstance(ast[1], str) and ast[1] in memory:
                return memory[ast[1]]
            return ast[1]
    else:
        return ast

def interpret_program(statements):
    for statement in statements:
        result = interpret(statement)
    return result

def interpret_compound(statements):
    for statement in statements:
        interpret(statement)

def interpret_assign(var, expr):
    value = interpret(expr)
    memory[var] = value
    return value

def interpret_expression(expr):
    return interpret(expr)

def interpret_binop(left, op, right):
    left_val = interpret(left)
    right_val = interpret(right)
    if op == '+':
        return left_val + right_val
    elif op == '-':
        return left_val - right_val
    elif op == '*':
        return left_val * right_val
    elif op == '/':
        return left_val / right_val
    # Add other operators here

def interpret_declare(kind, var, expr):
    value = interpret(expr)
    memory[var] = value
    return value

def interpret_if(condition, statement):
    if interpret(condition):
        return interpret(statement)
    
def interpret_if_else(condition, if_statement, else_statement):
    if interpret(condition):
        return interpret(if_statement)
    else:
        return interpret(else_statement)
    
def interpret_while(condition, statement):
    while interpret(condition):
        interpret(statement)


if __name__ == "__main__":
    while True:
        try:
            s = input('D# > ')
        except EOFError:
            break
        if not s:
            continue
        result = parser.parse(s, lexer=lexer)
        if result is not None:
            print(interpret(result))
        else:
            print("Error in parsing.")
