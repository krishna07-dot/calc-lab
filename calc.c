// Krishnapriya kizhakkoot gopalakrishnan
// (optional) https://github.com/<your-username>/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: <every AI tool, website and book you used; details in comments where you used them>

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#define STUDENT_NAME "krishnapriya_kizhakkoot gopalakrishnan"
#define STUDENT_ID "241ADB174"

typedef struct {
    int is_float;
    long long i;
    double f;
    size_t start;
} Value;

typedef enum {
    T_NUM,
    T_OP,
    T_LPAREN,
    T_RPAREN,
    T_EOF,
    T_INVALID
} TokType;

typedef struct {
    TokType type;
    size_t pos;
    char op[3];
    int is_float;
    long long i;
    double f;
} Token;

typedef struct {
    const char *buf;
    size_t len;
    size_t pos;
    size_t base_offset;
    Token cur;
    int error_pos;
} Parser;

static void advance(Parser *p) {
    size_t start = p->pos;
    while (start < p->len && (p->buf[start] == ' ' || p->buf[start] == '\t' || p->buf[start] == '\r')) {
        start++;
    }

    if (start >= p->len) {
        p->cur.type = T_EOF;
        p->cur.pos = p->base_offset + p->len;
        p->pos = p->len;
        return;
    }

    char c = p->buf[start];
    Token t;
    memset(&t, 0, sizeof(t));
    
    // why this tokenizer tracking: we add p->base_offset to the local 'start' index

    // since we process file line-by-line, this converts the line-local position into

    // the exact 1-based position within the entire file as required by the lab rules

    t.pos = p->base_offset + start;

    if (isdigit((unsigned char)c)) {
        size_t end = start + 1;
        while (end < p->len && isdigit((unsigned char)p->buf[end])) {
            end++;
        }
        if (end < p->len && p->buf[end] == '.') {
            size_t dot = end;
            end++;
            if (end >= p->len || !isdigit((unsigned char)p->buf[end])) {
                t.type = T_INVALID;
                t.pos = p->base_offset + dot;
                p->cur = t;
                p->pos = dot + 1;
                return;
            }
            while (end < p->len && isdigit((unsigned char)p->buf[end])) {
                end++;
            }
        }

        size_t length = end - start;
        if (length >= 255) {
            length = 255;
        }

        char tmp[256];
        memcpy(tmp, p->buf + start, length);
        tmp[length] = '\0';

        t.type = T_NUM;
        t.is_float = (strchr(tmp, '.') != NULL);
        if (t.is_float) {
            t.f = strtod(tmp, NULL);
        } else {
            t.i = strtoll(tmp, NULL, 10);
        }
        p->cur = t;
        p->pos = end;
        return;
    }

    if (c == '+') {
        t.type = T_OP;
        strcpy(t.op, "+");
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == '-') {
        t.type = T_OP;
        strcpy(t.op, "-");
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == '*') {
        t.type = T_OP;
        if (start + 1 < p->len && p->buf[start + 1] == '*') {
            strcpy(t.op, "**");
            p->cur = t;
            p->pos = start + 2;
            return;
        }
        strcpy(t.op, "*");
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == '/') {
        t.type = T_OP;
        strcpy(t.op, "/");
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == '(') {
        t.type = T_LPAREN;
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == ')') {
        t.type = T_RPAREN;
        p->cur = t;
        p->pos = start + 1;
        return;
    }
    if (c == '#' || c == '.' || c == '\n' || c == '\0') {
        t.type = T_INVALID;
        t.pos = p->base_offset + start;
        p->cur = t;
        p->pos = start + 1;
        return;
    }

    t.type = T_INVALID;
    t.pos = p->base_offset + start;
    p->cur = t;
    p->pos = start + 1;
}

static Value parse_primary(Parser *p);
static Value parse_power(Parser *p);
static Value parse_term(Parser *p);
static Value parse_expr(Parser *p);

static Value make_value_int(long long v, size_t start) {
    Value value;
    memset(&value, 0, sizeof(value));
    value.is_float = 0;
    value.i = v;
    value.start = start;
    return value;
}

static Value make_value_float(double v, size_t start) {
    Value value;
    memset(&value, 0, sizeof(value));
    value.is_float = 1;
    value.f = v;
    value.start = start;
    return value;
}

static Value parse_primary(Parser *p) {
    Value v;
    memset(&v, 0, sizeof(v));
    if (p->error_pos) {
        return v;
    }

    if (p->cur.type == T_NUM) {
        if (p->cur.is_float) {
            v = make_value_float(p->cur.f, p->cur.pos);
        } else {
            v = make_value_int(p->cur.i, p->cur.pos);
        }
        advance(p);
        return v;
    }

    if (p->cur.type == T_LPAREN) {
        size_t lparen_pos = p->cur.pos;
        advance(p);
        v = parse_expr(p);
        if (p->error_pos) {
            return v;
        }
        if (p->cur.type != T_RPAREN) {

            // why this error position: If a ')' is missing, the parser fails when it expects

            // the closing parenthesis but encounters the end of the line or an operator.

            // we report the position of this unexpected token.

            if (p->cur.type == T_EOF) {
                p->error_pos = (int)(p->cur.pos + 1);
            } else {
                p->error_pos = (int)(p->cur.pos + 1);
            }
            return v;
        }
        advance(p);
        v.start = lparen_pos;
        return v;
    }

    // why this error position: If we expected a number or '(' but got something else.

    // we report thr exact position of this unexpected token.


    if (p->cur.type == T_EOF) {
        p->error_pos = (int)(p->cur.pos + 1);
        return v;
    }

    if (p->cur.type == T_INVALID) {
        p->error_pos = (int)(p->cur.pos + 1);
        return v;
    }

    p->error_pos = (int)(p->cur.pos + 1);
    return v;
}
static Value parse_power(Parser *p) { 

    Value left = parse_primary(p);
    if (p->error_pos) {
        return left;
    }

    // why grammer gives right precedence and right-associative '**';
    
    // we call parse_power() recursively for the right side. This ensure that in an expression

    // like 2 ** 3 ** 2, the parser groups it as 2 **(3 **2) = 512, rather than (2 ** 3) ** 2 = 64. 

    if (p->cur.type == T_OP && strcmp(p->cur.op, "**") == 0) {
        size_t op_pos = p->cur.pos;
        advance(p);
        size_t right_start = p->cur.pos;
        Value right = parse_power(p);
        if (p->error_pos) {
            return left;
        }
        // why this error position : For a negative exponential, the eoor is reported
        
        // at the start of the exponent (the right operand).


        if (right.is_float ? right.f < 0.0 : right.i < 0) {
            p->error_pos = (int)(right_start + 1);
            return left;
        }
        // why decide between integer and float arithmetic:

        // If both operands are integers, we use a fast loop and exact long long to aviod precision loss.

        // If either is a float, we cast to double and use pow().

        if (!left.is_float && !right.is_float) {
            long long base = left.i;
            long long exp = right.i;
            if (exp < 0) {
                p->error_pos = (int)(right_start + 1);
                return left;
            }
            long long result = 1;
            for (long long i = 0; i < exp; i++) {
                if (base != 0 && result > LLONG_MAX / llabs(base)) {
                    p->error_pos = (int)(op_pos + 1);
                    return left;
                }
                result *= base;
            }
            left = make_value_int(result, left.start);
        } else {
            double l = left.is_float ? left.f : (double)left.i;
            double r = right.is_float ? right.f : (double)right.i;
            double result = pow(l, r);

            // why this error position: The lab rules specify that infinite or NaN float

            // results report the error at the operator itself.

            if (!isfinite(result) || isnan(result)) {
                p->error_pos = (int)(op_pos + 1);
                return left;
            }
            left = make_value_float(result, left.start);
        }
    }

    return left;
}

static Value parse_term(Parser *p) {
    Value left = parse_power(p);
    if (p->error_pos) {
        return left;
    }
    //why grammar gives right precedence: 'term' handles * and /, which are lower precedence
    // than ** but higher than + and -. The loop ensures left-associativity (e.g., 100/7/2 is (100/7)2).

    while (p->cur.type == T_OP && (strcmp(p->cur.op, "*") == 0 || strcmp(p->cur.op, "/") == 0)) {
        Token op = p->cur;
        size_t op_pos = op.pos;
        advance(p);
        //why this error position : For division by zero, the error is reported
        //at the start of the right operand (the denominator).
        size_t right_start = p->cur.pos;
        Value right = parse_power(p);
        if (p->error_pos) {
            return left;
        }

        if (strcmp(op.op, "*") == 0) {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                double result = l * r;
                if (!isfinite(result) || isnan(result)) {
                    p->error_pos = (int)(op_pos + 1);
                    return left;
                }
                left = make_value_float(result, left.start);
            } else {
                left = make_value_int(left.i * right.i, left.start);
            }
        } else {
            if ((left.is_float || right.is_float) ? (right.is_float ? right.f == 0.0 : right.i == 0) : right.i == 0) {
                p->error_pos = (int)(right_start + 1);
                return left;
            }

            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                double result = l / r;
                if (!isfinite(result) || isnan(result)) {
                    p->error_pos = (int)(op_pos + 1);
                    return left;
                }
                left = make_value_float(result, left.start);
            } else {
                left = make_value_int(left.i / right.i, left.start);
            }
        }
    }

    return left;
}

static Value parse_expr(Parser *p) {
    Value left = parse_term(p);
    if (p->error_pos) {
        return left;
    }
    // 'expr' handles + and -, which have the lowest precedence. The loop ensures left-associativity.

    while (p->cur.type == T_OP && (strcmp(p->cur.op, "+") == 0 || strcmp(p->cur.op, "-") == 0)) {
        Token op = p->cur;
        size_t op_pos = op.pos;
        advance(p);
        Value right = parse_term(p);
        if (p->error_pos) {
            return left;
        }

        if (strcmp(op.op, "+") == 0) {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                double result = l + r;
                if (!isfinite(result) || isnan(result)) {
                    p->error_pos = (int)(op_pos + 1);
                    return left;
                }
                left = make_value_float(result, left.start);
            } else {
                left = make_value_int(left.i + right.i, left.start);
            }
        } else {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f : (double)left.i;
                double r = right.is_float ? right.f : (double)right.i;
                double result = l - r;
                if (!isfinite(result) || isnan(result)) {
                    p->error_pos = (int)(op_pos + 1);
                    return left;
                }
                left = make_value_float(result, left.start);
            } else {
                left = make_value_int(left.i - right.i, left.start);
            }
        }
    }

    return left;
}

static const char *trim_start(const char *s) {
    while (*s == ' ' || *s == '\t' || *s == '\r') {
        s++;
    }
    return s;
}

static void strip_line_endings(char *s) {
    size_t len = strlen(s);
    while (len > 0 && (s[len - 1] == '\n' || s[len - 1] == '\r')) {
        s[len - 1] = '\0';
        len--;
    }
}

static void extract_base(const char *path, char *base, size_t base_size) {
    const char *slash = strrchr(path, '/');
    const char *start = slash ? slash + 1 : path;
    const char *dot = strrchr(start, '.');
    size_t len = strlen(start);
    if (dot != NULL && dot > start) {
        len = (size_t)(dot - start);
    }
    if (len >= base_size) {
        len = base_size - 1;
    }
    memcpy(base, start, len);
    base[len] = '\0';
}

static void ensure_dir(const char *dir_path) {
    if (mkdir(dir_path, 0777) != 0 && errno != EEXIST) {
        fprintf(stderr, "cannot create directory %s\n", dir_path);
        exit(1);
    }
}

static void process_file(const char *in_path, const char *out_dir, const char *base) {
    FILE *f = fopen(in_path, "rb");
    if (!f) {
        fprintf(stderr, "cannot open %s\n", in_path);
        exit(1);
    }

    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "cannot read %s\n", in_path);
        fclose(f);
        exit(1);
    }
    long file_size = ftell(f);
    if (file_size < 0) {
        fprintf(stderr, "cannot read %s\n", in_path);
        fclose(f);
        exit(1);
    }
    rewind(f);

    // Why this handles large files efficiently: We allocate the buffer once based on the file size

    // and read the entire file in a single fread call. This prevents slow O(n^2) memory copying

    // or string concatenation during parsing, which would cause slowdowns on multi-megabyte files

    char *buf = malloc((size_t)file_size + 1);
    if (!buf) {
        fprintf(stderr, "out of memory\n");
        fclose(f);
        exit(1);
    }
    size_t actually_read = fread(buf, 1, (size_t)file_size, f);
    fclose(f);
    if (actually_read != (size_t)file_size) {
        fprintf(stderr, "cannot read %s\n", in_path);
        free(buf);
        exit(1);
    }
    buf[file_size] = '\0';

    char out_path[1024];
    snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s.txt", out_dir, base, STUDENT_NAME, STUDENT_ID);
    FILE *out = fopen(out_path, "w");
    if (!out) {
        fprintf(stderr, "cannot write %s\n", out_path);
        free(buf);
        exit(1);
    }

    char *line_start = buf;
    char *end = buf + file_size;
    char *result_buf = NULL;
    size_t result_len = 0;
    size_t result_cap = 0;
    bool had_error = false;

    while (line_start < end) {
        char *line_end = line_start;
        while (line_end < end && *line_end != '\n' && *line_end != '\r') {
            line_end++;
        }

        size_t line_len = (size_t)(line_end - line_start);
        char line[4096];
        if (line_len >= sizeof(line)) {
            line_len = sizeof(line) - 1;
        }
        memcpy(line, line_start, line_len);
        line[line_len] = '\0';
        strip_line_endings(line);

        const char *trimmed = trim_start(line);
        if (*trimmed == '\0' || *trimmed == '#') {
            if (line_end < end) {
                while (line_end < end && (*line_end == '\n' || *line_end == '\r')) {
                    line_end++;
                }
            }
            line_start = line_end;
            continue;
        }

        Parser p;
        memset(&p, 0, sizeof(p));
        p.buf = line;
        p.len = strlen(line);
        p.pos = 0;
        p.base_offset = (size_t)(line_start - buf);
        advance(&p);
        Value result = parse_expr(&p);

        if (p.error_pos) {
            fprintf(out, "ERROR:%d\n", p.error_pos);
            had_error = true;
            break;
        }

        if (p.cur.type != T_EOF) {

         // Why this error position: If the parser finishes successfully but there are leftover

            // tokens (e.g., extra ')'), we report the position of the first unexpected token.  

            fprintf(out, "ERROR:%zu\n", p.cur.pos + 1);
            had_error = true;
            break;
        }

        char res[256];
        if (result.is_float) {

        // Use %.15g to strictly follow the lab's float printing requirements 

            snprintf(res, sizeof(res), "%.15g", result.f);
        } else {
            snprintf(res, sizeof(res), "%lld", result.i);
        }

        size_t res_len = strlen(res);
        if (result_len + res_len + 2 > result_cap) {
            result_cap = result_cap ? result_cap * 2 : 256;
            while (result_len + res_len + 2 > result_cap) {
                result_cap *= 2;
            }
            char *tmp = realloc(result_buf, result_cap);
            if (!tmp) {
                fprintf(stderr, "out of memory\n");
                fclose(out);
                free(result_buf);
                free(buf);
                exit(1);
            }
            result_buf = tmp;
        }
        memcpy(result_buf + result_len, res, res_len);
        result_len += res_len;
        result_buf[result_len++] = '\n';

        if (line_end < end) {
            while (line_end < end && (*line_end == '\n' || *line_end == '\r')) {
                line_end++;
            }
        }
        line_start = line_end;
    }

    if (!had_error) {
        if (result_buf != NULL) {
            fwrite(result_buf, 1, result_len, out);
        }
    }

    free(result_buf);
    fclose(out);
    free(buf);
}

static void process_directory(const char *dir_path, const char *out_dir) {
    DIR *dir = opendir(dir_path);
    if (!dir) {
        fprintf(stderr, "cannot open directory %s\n", dir_path);
        exit(1);
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        if (strstr(entry->d_name, ".txt") == NULL) {
            continue;
        }

        char full_path[1024];
        snprintf(full_path, sizeof(full_path), "%s/%s", dir_path, entry->d_name);
        struct stat st;
        if (stat(full_path, &st) != 0 || !S_ISREG(st.st_mode)) {
            continue;
        }

        char base[256];
        extract_base(entry->d_name, base, sizeof(base));
        process_file(full_path, out_dir, base);
    }

    closedir(dir);
}

int main(int argc, char **argv) {
    const char *dir_path = NULL;
    const char *input_path = NULL;
    const char *output_dir = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "missing argument for %s\n", argv[i]);
                return 1;
            }
            dir_path = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "missing argument for %s\n", argv[i]);
                return 1;
            }
            output_dir = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "unknown option %s\n", argv[i]);
            return 1;
        } else if (input_path == NULL && dir_path == NULL) {
            input_path = argv[i];
        } else {
            fprintf(stderr, "usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
            return 1;
        }
    }

    if (!dir_path && !input_path) {
        fprintf(stderr, "usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }

    if (dir_path && input_path) {
        fprintf(stderr, "cannot specify both -d and input file\n");
        return 1;
    }

    char dir_base[256];
    char default_dir[512];
    if (output_dir == NULL) {
        if (input_path) {
            extract_base(input_path, dir_base, sizeof(dir_base));
        } else {
            extract_base(dir_path, dir_base, sizeof(dir_base));
        }
        snprintf(default_dir, sizeof(default_dir), "%s_%s_%s", dir_base, STUDENT_NAME, STUDENT_ID);
        output_dir = default_dir;
    }

    ensure_dir(output_dir);

    if (input_path) {
        char base[256];
        extract_base(input_path, base, sizeof(base));
        process_file(input_path, output_dir, base);
    } else {
        process_directory(dir_path, output_dir);
    }

    return 0;
}
