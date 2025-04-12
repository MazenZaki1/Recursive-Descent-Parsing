#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Token types
typedef enum
{
    // Keywords
    TOK_PROGRAM,
    TOK_INT,
    TOK_FLOAT,
    TOK_IF,
    TOK_ELSE,
    TOK_WHILE,
    // Operators and punctuation
    TOK_PLUS,
    TOK_MINUS,
    TOK_MUL,
    TOK_DIV,
    TOK_ASSIGN,
    TOK_LT,
    TOK_GT,
    TOK_LTE,
    TOK_GTE,
    TOK_EQ,
    TOK_NEQ,
    TOK_SEMI,
    TOK_COMMA,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    // Identifiers and literals
    TOK_ID,
    TOK_NUM,
    // Special
    TOK_EOF,
    TOK_ERROR
} TokenType;

typedef struct
{
    TokenType type;
    char lexeme[256];
    int line;
    int start_pos; // Start column of the token
    int end_pos;   // End column of the token
} Token;

// Global state
const char *input;
int current_pos = 0;
int line = 1;
int column = 1;
Token lookahead;

void match(TokenType expected);
void expression();
void report_error(const char *message, Token *t);
void declaration_list();
void assignment_stmt();

// ================== SCANNER ==================
void init_scanner(const char *src)
{
    input = src;
    current_pos = 0;
    line = 1;
    column = 1;
}

void consume_char()
{
    if (input[current_pos] == '\n')
    {
        line++;
        column = 1; // Reset column for new line
    }
    else if (input[current_pos] == '\r')
    {
        // Handle Windows-style '\r\n'
        if (input[current_pos + 1] == '\n')
        {
            current_pos++; // Skip '\r'
            line++;
            column = 1;
        }
        else
        {
            column++;
        }
    }
    else
    {
        column++;
    }
    current_pos++;
}

char peek_char()
{
    return input[current_pos];
}

Token make_token(TokenType type, const char *lexeme)
{
    Token t;
    t.type = type;
    strncpy(t.lexeme, lexeme, 255);
    t.lexeme[255] = '\0';
    t.line = line;
    t.start_pos = column - strlen(lexeme); // Calculate start position
    t.end_pos = column - 1;                // Calculate end position
    return t;
}

Token get_next_token()
{
    while (1)
    {
        char c = peek_char();
        if (c == '\0')
            return make_token(TOK_EOF, "");

        // Skip whitespace
        if (isspace(c))
        {
            consume_char();
            continue;
        }

        // Handle comments
        if (c == '/' && input[current_pos + 1] == '*')
        {
            consume_char(); // '/'
            consume_char(); // '*'
            while (1)
            {
                if (peek_char() == '\0')
                {
                    return make_token(TOK_ERROR, "Unclosed comment");
                }
                if (peek_char() == '*' && input[current_pos + 1] == '/')
                {
                    consume_char(); // '*'
                    consume_char(); // '/'
                    break;
                }
                consume_char();
            }
            continue;
        }

        // Check keywords and identifiers
        if (isalpha(c))
        {
            char lexeme[256] = {0};
            int i = 0;
            while (isalnum(peek_char()))
            {
                lexeme[i++] = peek_char();
                consume_char();
            }
            lexeme[i] = '\0';

            if (strcmp(lexeme, "Program") == 0)
                return make_token(TOK_PROGRAM, lexeme);
            if (strcmp(lexeme, "int") == 0)
                return make_token(TOK_INT, lexeme);
            if (strcmp(lexeme, "float") == 0)
                return make_token(TOK_FLOAT, lexeme);
            if (strcmp(lexeme, "if") == 0)
                return make_token(TOK_IF, lexeme);
            if (strcmp(lexeme, "else") == 0)
                return make_token(TOK_ELSE, lexeme);
            if (strcmp(lexeme, "while") == 0)
                return make_token(TOK_WHILE, lexeme);

            return make_token(TOK_ID, lexeme);
        }

        // Check numbers
        if (isdigit(c))
        {
            char lexeme[256] = {0};
            int i = 0;
            while (isdigit(peek_char()))
            {
                lexeme[i++] = peek_char();
                consume_char();
            }

            if (peek_char() == '.')
            {
                lexeme[i++] = peek_char();
                consume_char();
                while (isdigit(peek_char()))
                {
                    lexeme[i++] = peek_char();
                    consume_char();
                }
            }

            if (tolower(peek_char()) == 'e')
            {
                lexeme[i++] = peek_char();
                consume_char();
                if (peek_char() == '+' || peek_char() == '-')
                {
                    lexeme[i++] = peek_char();
                    consume_char();
                }
                while (isdigit(peek_char()))
                {
                    lexeme[i++] = peek_char();
                    consume_char();
                }
            }

            return make_token(TOK_NUM, lexeme);
        }

        // Handle operators and punctuation
        switch (c)
        {
        case '+':
            consume_char();
            return make_token(TOK_PLUS, "+");
        case '-':
            consume_char();
            return make_token(TOK_MINUS, "-");
        case '*':
            consume_char();
            return make_token(TOK_MUL, "*");
        case '/':
            consume_char();
            return make_token(TOK_DIV, "/");
        case '=':
            consume_char();
            if (peek_char() == '=')
            {
                consume_char();
                return make_token(TOK_EQ, "==");
            }
            return make_token(TOK_ASSIGN, "=");
        case '<':
            consume_char();
            if (peek_char() == '=')
            {
                consume_char();
                return make_token(TOK_LTE, "<=");
            }
            return make_token(TOK_LT, "<");
        case '>':
            consume_char();
            if (peek_char() == '=')
            {
                consume_char();
                return make_token(TOK_GTE, ">=");
            }
            return make_token(TOK_GT, ">");
        case '!':
            consume_char();
            if (peek_char() == '=')
            {
                consume_char();
                return make_token(TOK_NEQ, "!=");
            }
            return make_token(TOK_ERROR, "!");
        case ';':
            consume_char();
            return make_token(TOK_SEMI, ";");
        case '{':
            consume_char();
            return make_token(TOK_LBRACE, "{");
        case '}':
            consume_char();
            return make_token(TOK_RBRACE, "}");
        case '(':
            consume_char();
            return make_token(TOK_LPAREN, "(");
        case ')':
            consume_char();
            return make_token(TOK_RPAREN, ")");
        default:
            char err[2] = {c, '\0'};
            consume_char();
            return make_token(TOK_ERROR, err);
        }
    }
}

// ================== PARSER ==================
// ================== EXPRESSION PARSER ==================
void factor()
{
    switch (lookahead.type)
    {
    case TOK_ID:
        match(TOK_ID);
        break;
    case TOK_NUM:
        match(TOK_NUM);
        break;
    case TOK_LPAREN:
        match(TOK_LPAREN);
        expression();
        match(TOK_RPAREN);
        break;
    default:
        report_error("Unexpected token in factor", &lookahead);
    }
}

void term()
{
    factor();
    while (lookahead.type == TOK_MUL || lookahead.type == TOK_DIV)
    {
        match(lookahead.type);
        factor();
    }
}

void additive_expression()
{
    term();
    while (lookahead.type == TOK_PLUS || lookahead.type == TOK_MINUS)
    {
        match(lookahead.type);
        term();
    }
}

void expression()
{
    printf("DEBUG: Parsing expression\n");
    additive_expression();
    // Handle relational operators
    if (lookahead.type == TOK_LT || lookahead.type == TOK_GT ||
        lookahead.type == TOK_LTE || lookahead.type == TOK_GTE ||
        lookahead.type == TOK_EQ || lookahead.type == TOK_NEQ)
    {
        printf("DEBUG: Found relational operator: %s\n", token_to_str(lookahead.type));
        match(lookahead.type);
        additive_expression();
    }
}

const char *token_to_str(TokenType type)
{
    switch (type)
    {
    // Keywords
    case TOK_PROGRAM:
        return "Program";
    case TOK_INT:
        return "int";
    case TOK_FLOAT:
        return "float";
    case TOK_IF:
        return "if";
    case TOK_ELSE:
        return "else";
    case TOK_WHILE:
        return "while";

    // Operators
    case TOK_PLUS:
        return "+";
    case TOK_MINUS:
        return "-";
    case TOK_MUL:
        return "*";
    case TOK_DIV:
        return "/";
    case TOK_ASSIGN:
        return "=";
    case TOK_LT:
        return "<";
    case TOK_GT:
        return ">";
    case TOK_LTE:
        return "<=";
    case TOK_GTE:
        return ">=";
    case TOK_EQ:
        return "==";
    case TOK_NEQ:
        return "!=";

    // Punctuation
    case TOK_SEMI:
        return ";";
    case TOK_COMMA:
        return ",";
    case TOK_LPAREN:
        return "(";
    case TOK_RPAREN:
        return ")";
    case TOK_LBRACE:
        return "{";
    case TOK_RBRACE:
        return "}";

    // Identifiers and literals
    case TOK_ID:
        return "identifier";
    case TOK_NUM:
        return "number";

    // Special tokens
    case TOK_EOF:
        return "end-of-file";
    case TOK_ERROR:
        return "error";

    // Default case (should never be reached)
    default:
        return "unknown-token";
    }
}

void report_error(const char *message, Token *t)
{
    printf("ERROR at line %d, position %d: %s (found '%s')\n",
           t->line, t->end_pos, // Use end_pos for precise error location
           message, t->lexeme);
    exit(1);
}

void match(TokenType expected)
{
    if (lookahead.type == expected)
    {
        lookahead = get_next_token();
    }
    else
    {
        printf("ERROR at line %d, position %d: Expected '%s', found '%s'\n",
               lookahead.line,
               lookahead.start_pos, // Use start_pos of lookahead token
               token_to_str(expected),
               token_to_str(lookahead.type));
        exit(1);
    }
}

void expression();
void additive_expression();

// Grammar rules
void program()
{
    match(TOK_PROGRAM);
    match(TOK_ID);
    match(TOK_LBRACE);
    declaration_list();
    statement_list();
    match(TOK_RBRACE);
}

void declaration_list()
{
    while (lookahead.type == TOK_INT || lookahead.type == TOK_FLOAT)
    {
        // Match type (int/float)
        Token type_token = lookahead;
        match(lookahead.type);

        // Match identifier and save its position
        Token id_token = lookahead;
        match(TOK_ID);

        // Check for semicolon immediately after the identifier
        if (lookahead.type != TOK_SEMI)
        {
            printf("ERROR at line %d, position %d: Expected ';' after declaration of '%s'\n",
                   id_token.line, id_token.end_pos + 1, id_token.lexeme);
            exit(1);
        }
        match(TOK_SEMI);
    }
}

void statement_list()
{
    printf("DEBUG: Entering statement_list()\n");
    while (lookahead.type != TOK_RBRACE)
    {
        printf("DEBUG: Current token: %s (line %d)\n",
               token_to_str(lookahead.type), lookahead.line);
        statement();
    }
    printf("DEBUG: Exiting statement_list()\n");
}

void assignment_stmt()
{
    match(TOK_ID);     // Match variable (e.g., 'x')
    match(TOK_ASSIGN); // Match '='
    expression();      // Parse the expression (e.g., '5', 'x + 1')
    match(TOK_SEMI);   // Match ';'
}

// Stubs for other rules (implement similarly)
void selection_stmt() { /* ... */ }
void iteration_stmt() { /* ... */ }
void compound_stmt() { /* ... */ }

// ================== MAIN ==================
int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <input-file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *fp = fopen(argv[1], "r");
    if (!fp)
    {
        perror("Error opening file");
        return 1;
    }
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    rewind(fp);
    char *buffer = malloc(size + 1);
    fread(buffer, 1, size, fp);
    buffer[size] = '\0';
    fclose(fp);

    // Initialize scanner and parser
    init_scanner(buffer);
    lookahead = get_next_token();

    // Start parsing
    program();

    if (lookahead.type == TOK_EOF)
    {
        printf("Parsing completed successfully.\n");
    }
    else
    {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}