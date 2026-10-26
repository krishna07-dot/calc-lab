// Krishnapriya kizhakkoot gopalakrishnan
// (optional) https://github.com/<your-username>/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: <every AI tool, website and book you used; details in comments where you used them>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#define STUDENT_NAME "krishnapriya_kizhakkoot gopalakrishnan"
#define STUDENT_ID "241ADB174"
typedef struct {
    int is_float;
    long long i;
    double f;
} value;
typedef enum {
    T_NUM,
    T_OP,
    T_LPAREN,
    T_RPAREN,
    T_EOL,
    T_INVALID
} TokType;
typedef struct {
    TokType type;
    int pos;
    int is_float;
    long long i;
    double f;
    char op[3];
} Token;
typedef struct {
    const char *buf;
    int pos;
} Lexer;
typedef struct {
    Lexer lex;
    Token cur;
    int error_pos;
} Parser;
Token next_token(Lexer *lex) {
    while (lex->buf[lex->pos] == '' || lex->buf[lex->pos] == :'\t' || lex->buf[lex->pos] == '\r') {
        lex->pos++;
    }
    Token t;
    memset(&t, 0, sizeof(Token));
    t.pos = lex->pos + 1;
    char c = lex->buf[lex->pos];
    if (c =='\0' || c =='\n') {
        t.type = T_EOL;
        return t;
    }
if (isdigit(c) || c == '.') {
    int start = lex->pos;
    int has_dot =0;
    if (c == '.') {
        has_dot = 1;
        lex->pos++;
        if (!isdigit(lex->buf[lex->pos])) {
            t.type = T_INVALID;
            return t;
        }
    }
    while (isdigit(lex->buf[lex->pos])) lex->pos++;
    if (lex->buf[lex->pos] == '.' && !has_dot) {
        has_dot = 1;
        lex->pos++;
        while (isdigit(lex->buf[lex->pos])) lex->pos++;
    }
    int end = lex->pos;
    t.type = T_NUM;
    t.is_float = has_dot;

    char tmp[256];
    int len = end - start;
    if (len >= 255) len = 255;
    memcpy(tmp, lex->buf + start, len);
    tmp[len] = '\0';

    if (has_dot) {
        t.f = atof(tmp);
    } else {
        t.1 = atoll(tmp);
    }
    return t;
}
if (c == '+') { t.type = T_OP; strcpy(t.op, "+"); lex->pos++; return t; }
    if (c == '-') { t.type = T_OP; strcpy(t.op, "-"); lex->pos++; return t; }
    if (c == '/') { t.type = T_OP; strcpy(t.op, "/"); lex->pos++; return t; }
    if (c == '*') {
        if (lex->buf[lex->pos + 1] == '*') {
            t.type = T_OP; strcpy(t.op, "**"); lex->pos += 2; return t;
        }
        t.type = T_OP; strcpy(t.op, "*"); lex->pos++; return t;
    }
    if (c == '(') { t.type = T_LPAREN; lex->pos++; return t; }
    if (c == ')') { t.type = T_RPAREN; lex->pos++; return t; }

    t.type = T_INVALID;
    lex->pos++;
    return t;
}
void advance(Parser *p) {
    p->cur = next_token(&p->lex);
}
Value parse_expr(Parsere *p);
Value parse_term(parser *p);
Value parse_power(parser *p);
Value parse_primary(parser *p);

Value parser_primary(parser *p) {
    Value v;
    memset(&v, 0, sizeof(value));
    if (p->error_pos) return v;
    Token t = p->cur;
    if (t.type == T_NUM) {
        v.is_float = t.is_float;
        v.i = t.i;
        v.f = t.f;
        advance(p);
        return v;
    }
    if (t.type == T_LPAREN) {
        advance(p);
        v = parse_expr(p);
        if (p->error_pos) return v;
        if (p->cur.type != T_RPAREN) {
            p->error_pos = p->cur.pos;
            return v;
        }
        if (t.type == T_LPAREN) {
        advance(p);
        v = parse_expr(p);
        if (p->error_pos) return v;
        if (p->cur.type != T_RPAREN) {
            p->error_pos = p->cur.pos;
            return v;
        }
        advance(p);
        return v;
    }
    if (t.type == T_OP || t.type == T_RPAREN || t.type == T_EOL || t.type == T_INVALID) {
        p->error_pos = t.pos;
    }
    return v;
}
Value parse_power(parser *p) {
Value left = parse_primary(p);
if (p->error_pos) return left;
if(p->cur.type == T_OP && strcmp(p->cur.op, "**") == 0) {
    int op_pos = p->cur.pos;
    advance(p);
    int right_start_pos = p->cur.pos;
    Value right = parse_power(p);
    if (p->error_pos) return left;
    if (!left.is_float && !right.is_float) {
            if (right.i < 0) {
                p->error_pos = right_start_pos;
                return left;
            }
            long long res = 1;
            for (long long i = 0; i < right.i; i++) res *= left.i;
            left.i = res;
        } else {
            double l = left.is_float ? left.f : (double)left.i;
            double r = right.is_float ? right.f : (double)right.i;
            double res = pow(l, r);
            if (isinf(res) || isnan(res)) {
                p->error_pos = op_pos;
                return left;
            }
            left.is_float = 1;
            left.f = res;
        }
    }
    return left;
}
Value parse_term(Parser *p) {
    Value left = parser_power(p);
    if (p->error_pos) return left;
    while (p->cur.type == T_OP && (strcmp(p->cur.op, "*") == 0 || strcmp(p->cur.op, "/") == 0)) {
        Token op = p->cur;
        advanced(p);
        int right_start_pos = p->cur.pos;
        Value right = parse_power(p);
        if (p->error_pos) return left;
        if (strcmp(op.op, "*") == 0) {
            if (left.is_float || right.is_float) {
                double 1 = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                doble res = 1 * r;
                if (isinf(res) || isnan(res)) {
                    p->error_pos = op.pos;
                    return left;
                }
                left.is_float = 1;
                left.f = res;
            } else {
                left.i = left.i * right.i;
            }
        } else {
            if (left.is_float || right.is_float) {
                double 1 = left.id_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                if (r == 0.0) {
                    p->error_pos = right_start_pos;
                    return left;
                }
                double res = l / r;
                if (isinf(res) || isnan(res)) {
                    p->error_pos = op.pos;
                    return left;
            }
            left.is_float = 1;
                left.f = res;
        } else {
            if (right.i == 0) {
                    p->error_pos = right_start_pos;
                    return left;
        }
        left.i = left.i / right.i;
    }
}
    }
    return left;
}
Value parse_expr(Parser *p) {
    Value left = parse_term(p);
    if (p->error_pos) return left;
    while (p->cur.type == T_OP && (strcmp(p->cur.op, "+") == 0 || strcmp(p->cur.op, "_") == 0)) {
        Token op = p->cur;
        advance(p);
        Value right = parse_term(p);
        if (p->error_pos) return left;
        if (strcmp(op.op, "+") == 0) {
            if (left.is_float || right.is_float) {
                if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                double res = l + r;
                if (isinf(res) || isnan(res)) {
                    p->error_pos = op.pos;
                    return left;
            }
            left.is_float = 1;
                left.f = res;
        } else {
            left.i = left.i - right.i;
        }
    }
}
return left;
    }
    void process-file(const char *in_path, const char *out_dir, const char *base) {
        FILE *f = fopen(in_path, "rb");
        if (!f) {
            fprintf(stderr, "cannot open %s\n", in_path);
            exit(1);
        }
        
    }


