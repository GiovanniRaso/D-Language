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
    'RIGHT_ARROW', 'LEFT_ARROW',
    'INCREMENT', 'DECREMENT', 'EQUAL_TO', 'NOT_EQUAL_TO'
)


t_ASSIGN    = r'='
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_MODULUS   = r'%'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LEFT_CURLY_BRACE = r'\{'
t_RIGHT_CURLY_BRACE = r'\}'
t_LEFT_SQUARE_BRACKET = r'\['
t_RIGHT_SQUARE_BRACKET = r'\]'
t_COMMA     = r','
t_SEMICOLON = r';'
t_COLON     = r':'
t_DOT       = r'\.'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_EQUAL_TO  = r'=='
t_NOT_EQUAL_TO = r'!='
t_EQUAL     = r'=='
t_NOT_EQUAL = r'!='
t_GREATER_THAN = r'>'
t_LESS_THAN = r'<'
t_GREATER_THAN_OR_EQUAL = r'>='
t_LESS_THAN_OR_EQUAL = r'<='
t_AND       = r'&&'
t_OR        = r'\|\|'
t_NOT       = r'!'

reserved = {
    "if" : "IF",
    'then' : 'THEN', 
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
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value.lower(), 'IDENTIFIER')  # Ensure case insensitivity
    return t

tokens += tuple(reserved.values())

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
        
def p_print_statement(p):
    'print_statement : PRINT expression SEMICOLON'
    p[0] = ('print', p[2])

def p_statement(p):
    '''
    statement : print_statement
              | expression_statement
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
    selection_statement : IF expression THEN compound_statement
                        | IF expression THEN compound_statement ELSE compound_statement
    '''
    if len(p) == 5:
        p[0] = ('if', p[2], p[4], None)
    else:
        p[0] = ('if-else', p[2], p[4], p[6])

def p_iteration_statement(p):
    '''
    iteration_statement : WHILE LPAREN expression RPAREN compound_statement
                        | FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN compound_statement
    '''
    if len(p) == 6:
        p[0] = ('while', p[3], p[5])
    else:
        p[0] = ('for', p[3], p[5], p[7], p[9])

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
               | expression EQUAL expression
               | expression NOT_EQUAL expression
               | expression GREATER_THAN expression
               | expression LESS_THAN expression
               | expression GREATER_THAN_OR_EQUAL expression
               | expression LESS_THAN_OR_EQUAL expression
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
        elif op == 'for':
            return interpret_for(ast[1], ast[2], ast[3], ast[4])
        elif op == 'print':  # Handle 'print' operation
            value_to_print = interpret(ast[1])  # Evaluate the expression to be printed
            print(value_to_print)  # Print the result of the expression
            return
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
    if expr[0] == 'value':
        return expr[1]  # Direct value
    elif expr[0] == 'binop':
        return interpret_binop(expr[1], expr[2], expr[3])

def interpret_print(value):
    print(interpret(value))

def interpret_binop(left, op, right):
    left_val = interpret(left)
    right_val = interpret(right)
    if isinstance(left_val, str) and left_val.isdigit():
        left_val = int(left_val)
    if isinstance(right_val, str) and right_val.isdigit():
        right_val = int(right_val)
    if op == '+':
        return left_val + right_val
    elif op == '-':
        return left_val - right_val
    elif op == '*':
        return left_val * right_val
    elif op == '/':
        if right_val == 0:
            print("Error: Division by zero")  # Print an error message or handle it in another way
            return None  # You can choose to return None or raise an exception
        else:
            return left_val / right_val
    elif op == '>':
        return left_val > right_val
    elif op == '<':
        return left_val < right_val
    elif op == '>=':
        return left_val >= right_val
    elif op == '<=':
        return left_val <= right_val
    elif op == '==':
        return left_val == right_val
    elif op == '!=':
        return left_val != right_val
    elif op == '&&':
        return left_val and right_val
    elif op == '||':
        return left_val or right_val
    else:
        raise ValueError(f"Unsupported operator: {op}")

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
        
def interpret_for(assign, condition, update, statement):
    interpret(assign)
    while interpret(condition):
        interpret(statement)
        interpret(update)


if __name__ == "__main__":
    while True:
        try:
            s = input('D# > ')
        except EOFError:
            break
        if not s:
            continue
        result = parser.parse(s, lexer=lexer)
        if result is not None:  # Interpret and check if there's something to print
            output = interpret(result)
            if output is not None:
                print(output)