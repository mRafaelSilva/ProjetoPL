import ply.lex as lex

"""
Pascal Lexical Analyzer - Improved Version
"""

tokens = (
    # bloco principal
    'PROGRAM', 'BEGIN', 'END',

    # tipos e variáveis
    'VAR', 'ARRAY', 'OF', 'INTEGER', 'REAL', 'BOOLEAN', 'CHAR', 'STRINGTYPE',

    # controlo de fluxo
    'IF', 'THEN', 'ELSE',
    'WHILE', 'DO',
    'FOR', 'TO', 'DOWNTO',
    'REPEAT', 'UNTIL',

    # funções e procedimentos
    'FUNCTION', 'PROCEDURE',

    # I/O operations
    'WRITELN', 'WRITE', 'READLN', 'READ',

    # funções built-in
    'LENGTH',

    # operadores
    'ASSIGN', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EQUAL', 'NEQUAL', 'LT', 'LE', 'GT', 'GE',
    'MOD', 'DIV',
    'AND', 'OR', 'NOT',

    # símbolos
    'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET',
    'COLON', 'SEMICOLON', 'DOT', 'COMMA',

    # identificadores e literais
    'ID', 'INT', 'REALNUM', 'STRING', 'CHARLIT',

    # valores boolean
    'TRUE', 'FALSE',

    # comentários
    'COMMENT'
)

states = (
    ('comment', 'exclusive'),
)

# ignorar espaços e tabulações em modo normal
t_ignore = ' \t'
t_comment_ignore = ''

keywords = {
    'program': 'PROGRAM', 'begin': 'BEGIN', 'end': 'END',
    'var': 'VAR', 'array': 'ARRAY', 'of': 'OF',
    'integer': 'INTEGER', 'real': 'REAL', 'boolean': 'BOOLEAN',
    'char': 'CHAR', 'string': 'STRINGTYPE', 
    'if': 'IF', 'then': 'THEN', 'else': 'ELSE',
    'while': 'WHILE', 'do': 'DO',
    'for': 'FOR', 'to': 'TO', 'downto': 'DOWNTO',
    'repeat': 'REPEAT', 'until': 'UNTIL',
    'mod': 'MOD', 'div': 'DIV',
    'and': 'AND', 'or': 'OR', 'not': 'NOT',
    'function': 'FUNCTION', 'procedure': 'PROCEDURE',
    'writeln': 'WRITELN', 'write': 'WRITE',
    'readln': 'READLN', 'read': 'READ',
    'length': 'LENGTH',
    'true': 'TRUE', 'false': 'FALSE'
}

# operadores e símbolos (ordem importa - mais específicos primeiro)
t_ASSIGN    = r':='
t_LE        = r'<='
t_GE        = r'>='
t_NEQUAL    = r'<>'
t_EQUAL     = r'='
t_LT        = r'<'
t_GT        = r'>'
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LBRACKET  = r'\['
t_RBRACKET  = r'\]'
t_COLON     = r':'
t_SEMICOLON = r';'
t_DOT       = r'\.'
t_COMMA     = r','

# comentários entre { }
def t_COMMENT(t):
    r'\{[^}]*\}'
    # Count newlines for proper line tracking
    t.lexer.lineno += t.value.count('\n')
    pass

# comentários entre (* *) com aninhamento
def t_LPAREN_STAR(t):
    r'\(\*'
    t.lexer.comment_level = 1
    t.lexer.push_state('comment')

def t_comment_LPAREN_STAR(t):
    r'\(\*'
    t.lexer.comment_level += 1

def t_comment_STAR_RPAREN(t):
    r'\*\)'
    t.lexer.comment_level -= 1
    if t.lexer.comment_level == 0:
        t.lexer.pop_state()

def t_comment_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comment_content(t):
    r'[^(*\*)\n]+'
    pass

def t_comment_error(t):
    t.lexer.skip(1)

# números (real numbers first to avoid conflicts)
def t_REALNUM(t):
    r'\d+\.\d+([eE][+-]?\d+)?'
    try:
        t.value = float(t.value)
    except ValueError:
        print(f"Invalid real number: {t.value}")
        t.value = 0.0
    return t

def t_INT(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print(f"Invalid integer: {t.value}")
        t.value = 0
    return t

# character literals
def t_CHARLIT(t):
    r"'(?:''|[^']){1}'"
    # Handle escaped quotes
    if t.value == "''''":  # Two single quotes inside quotes
        t.value = "'"
    else:
        t.value = t.value[1:-1]  # Remove surrounding quotes
    return t

# strings (must come after character literals)
def t_STRING(t):
    r"'(?:''|[^'])*'"
    # Handle multi-character strings and escaped quotes
    if len(t.value) > 3 or (len(t.value) == 3 and t.value != "''"):
        t.value = t.value[1:-1].replace("''", "'")
        return t
    else:
        # Single character - reclassify as CHARLIT
        if t.value == "''''":
            t.value = "'"
        else:
            t.value = t.value[1:-1]
        t.type = 'CHARLIT'
        return t

# identificadores e palavras-chave
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = keywords.get(t.value.lower(), 'ID')
    return t

# nova linha
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# erro
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}")
    t.lexer.skip(1)

# criar lexer
lexer = lex.lex()

def analyze_file(filename):
    """Analyze a Pascal file"""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
        
        lexer.input(data)
        tokens_found = []
        
        while True:
            try:
                tok = lexer.token()
                if not tok:
                    break
                tokens_found.append(tok)
                print(f"Line {tok.lineno}: {tok.type} = {repr(tok.value)}")
            except lex.LexError as e:
                print(f"Lexical error: {e}")
                break
                
        return tokens_found
        
    except FileNotFoundError:
        print(f"File '{filename}' not found")
        return []
    except Exception as e:
        print(f"Error reading file: {e}")
        return []

def get_input():
    """Interactive mode"""
    print("Pascal Lexer - Interactive Mode")
    print("Type 'quit' to exit, 'file <filename>' to analyze a file")
    
    while True:
        try:
            data = input('>>> ')
        except EOFError:
            break
            
        if data.lower() == 'quit':
            break
        elif data.lower().startswith('file '):
            filename = data[5:].strip()
            analyze_file(filename)
            continue
        elif data == '':
            continue
            
        lexer.input(data)
        while True:
            try:
                tok = lexer.token()
                if not tok:
                    break
                print(f"{tok.type} = {repr(tok.value)}")
            except lex.LexError as e:
                print(f"Lexical error: {e}")
                break

if __name__ == "__main__":
    get_input()