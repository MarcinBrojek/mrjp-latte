#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct string {
    char* c;
    int len;
};
typedef struct string string;

void function_printInt (int x) {
    printf ("%d\n", x);
    return;
}

void function_printString (string* str) {
    printf ("%s\n", str->c);
    return;
}

void function_error () {
    printf ("runtime error\n");
    exit (1);
}

int function_readInt () {
    int x = 0;
    scanf ("%d[^]", &x);
    int c;
    do {
        c = getc (stdin);
    } while (c == ' ');
    if (c != EOF && c != '\n')
        ungetc (c, stdin);
    return x;
}

char* __readString () {
    char* tmp = (char*) malloc (100);
    char* res = fgets (tmp, 100, stdin);
    int len = strlen (tmp);
    if (res == NULL || tmp [len - 1] == '\n') {
        if (len > 0 && tmp [len - 1] == '\n') {
            len -= 1;
            tmp [len] = '\00';
        }
        tmp = (char*) realloc (tmp, len + 1);
    } else {
        res = __readString();
        len = strlen(res);
        tmp = (char*) realloc (tmp, 100 + len);
        tmp = strcat (tmp, res);
    }
    return tmp;
}

string* function_readString () {
    char* c = __readString();
    string* res = (string*) malloc (sizeof (string));
    res->c = c;
    res->len = strlen(c);
    return res;
}

string* function_concat (string* S1, string* S2) {
    char* s1 = S1->c;
    char* s2 = S2->c;
    int l1 = strlen(s1);
    int l2 = strlen(s2);
    char* concat = (char*) calloc (l1 + l2 + 1, 1);
    concat = strcat (concat, s1);
    concat = strcat (concat, s2);

    string* res = (string*) malloc (sizeof(string));
    res->c = concat;
    res->len = S1->len + S2->len;
    return res;
}

// int main() {
//     int a = function_readInt();
//     string* b = function_readString();
//     string* c = function_readString();

//     function_printInt(a - 5);
//     function_printString(function_concat(b, c));
//     return 0;
// }