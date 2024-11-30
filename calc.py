import ply.lex as lex
import ply.yacc as yacc
import math
import sys

# Reserved words
reserved = {
    'e': 'E',
    'pi': 'PI'
}

# Define tokens
tokens = [

    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
    'EXPONENT',
    'RT',
    'ID',
    'LPAREN',
    'RPAREN'
]  + list(reserved.values())

# Define the meaning of each token
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EXPONENT = r'\^'
t_RT = r'\&'
t_EQUALS = r'\='
t_LPAREN = r'\('
t_RPAREN = r'\)'

# Handle reserved types
def t_RESERVED(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')  
    return t

# Ignoring spaces (3+4 is the same as 3 + 4)
t_ignore = r' '

# Definine a floating point input as integer . integer
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Define integer
def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Define what a variable name can be
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

# Notify user of illegal input and skip to prevent lexer crash
def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)

lexer = lex.lex()

# Giving precedence definitions
precedence = (
    ('right', 'EXPONENT', 'RT'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')

)

# Define a claculation as a variable assignment, an expression, or empty
def p_calc(p):
    '''
    calc : expression
         | var_assign
         | empty
    '''
    print(run(p[1]))

#Define what a variable assignment is
def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression
    '''
    p[0] = ('=', p[1], p[3])

# Define legal expressions
def p_expression(p):
    '''
    expression : expression EXPONENT expression
               | expression RT expression
               | expression MULTIPLY expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    # Build a parse tree for the expression
    p[0] = (p[2], p[1], p[3])

# Define how parentheses work
def p_expression_group(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]  # Evaluate the expression inside the parentheses

def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]

def p_expression_var(p):
    '''
    expression : NAME
    '''
    p[0] = ('var', p[1])

def p_expression_pi(p):
    'expression : PI'
    p[0] = math.pi

def p_expression_e(p):
    'expression : E'
    p[0] = math.e

# Error message
def p_error(p):
    print("Syntax error; please try again")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

parser = yacc.yacc()
# Create the environment upon which we will store and retreive variables from.
env = {}

#Execute the parse tree
def run(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '^':
            return pow(run(p[1]), run(p[2]))
        elif p[0] == '&':
            return pow(run(p[1]), (1/run(p[2])))
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '=':
            env[p[1]] = run(p[2])
            return ''
        elif p[0] == 'var':
            if p[1] not in env:
                return 'Warning: Undeclared variable.'
            else:
                return env[p[1]]
    else:
        return p

# Run the calculator as a loop
print('Simple calculator for COS 360, please see below for usage directions: \n - a root b: a&b \n - a to the power of b: a^b \n - π: pi\n - number e: e')
while True:
    try:
        s = input('>>> ')
    except EOFError:
        break
    parser.parse(s)
