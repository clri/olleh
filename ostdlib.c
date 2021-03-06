/*
 * !OLLEH builtin functions, written in C.
 * Contributors: Caroline Roig-Irwin clr2176
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

//STRUCTS: object definitions
typedef struct charmap {
        char key;
        int value;
        struct charmap *next;
} cmap_t;

typedef struct stringmap {
        char *key;
        int value;
        struct stringmap *next;
} smap_t;

//concat two strings
char *SConcat(char *a, char *b) {
        char *ans;
        int n = strlen(a) + strlen(b) + 1;

        ans = malloc(n);
        strcpy(ans, a);
        strcpy(ans + strlen(a), b);
        ans[n-1] = 0;
        return ans;
}

//helper function for converting int to string
int GetElemSpace(int e) {
        int ans = 9;
        int powers[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};

        while (e / powers[ans] == 0)
                ans--;
        return ans;
}

//convert int to string
char *intToString(int e) {
        char *ans;
        int s = GetElemSpace(e) + 3;
        ans = malloc(s);
        sprintf(ans, "%d", e);
        ans[s - 1] = 0;
        return ans;
}

//helper function for PrintCharLis and PrintListList
void PrintCharLis1(char *lis) {
        int i;
        printf("[");
        int n = strlen(lis);
        if (n == 0) {
                printf("]");
                return;
        }
        for (i = 0; i < n - 1; i++)
                printf("'%c', ", lis[i]);
        printf("'%c']", lis[i]);
}

//print charlist, formatted
void PrintCharLis(char *lis) {
        PrintCharLis1(lis);
        printf("\n");
}

//print listlist
void PrintListList(char **lis) {
        int i = 0;

        printf("[");
        while (lis[i+1] != NULL) {
                PrintCharLis1(lis[i]);
                printf(",\n");
        }
        PrintCharLis1(lis[i]);
        printf("]\n");
}

//print a stringmap
void PrintStringmap(smap_t *map) {
        smap_t *tmp = map;

        printf("{");
        while (tmp != NULL && tmp->next != NULL) {
                printf("\"%s\" -> %d, ", tmp->key, tmp->value);
                tmp = tmp->next;
        }
        if (tmp != NULL)
                printf("\"%s\" -> %d", tmp->key, tmp->value);
        printf("}\n");
}

//print a charmap
void PrintCharmap(cmap_t *map) {
        cmap_t *tmp = map;

        printf("{");
        while (tmp != NULL && tmp->next != NULL) {
                printf("'%c' -> %d, ", tmp->key, tmp->value);
                tmp = tmp->next;
        }
        if (tmp != NULL)
                printf("'%c' -> %d", tmp->key, tmp->value);
        printf("}\n");
}


//called once at the beginning of each program
//to initialize the RNG
void InitializeRandom(void) {
        srand(time(NULL));
}
//random() function maps to this
int OllehRandom(int max) {
        return rand() % max;
}

//returns a line without newline char
char* readInput(void) {
        char *ans = NULL;
        size_t len = 0;
        ssize_t err = getline(&ans, &len, stdin);
        if (!err) return "";
        if (len == 0) return "";
        ans[strlen(ans) - 1] = 0; //remove newline
        return ans;
}

//sets m[k] = v. returns the map itself. if the map
//is empty, allocates space for it.
smap_t *Stringmapset(smap_t *m, char *k, int v) {
        smap_t *tmp = m;
        smap_t *tmp2 = tmp;

        if (m == NULL) {
                m = malloc(sizeof(smap_t));
                m->key = k;
                m->value = v;
                m->next = NULL;
                return m;
        }
        while (tmp != NULL) {
                if (strcmp(tmp->key, k) == 0)
                        break;
                tmp2 = tmp;
                tmp = tmp->next;
        }
        if (tmp == NULL) {
                tmp = malloc(sizeof(smap_t));
                tmp->key = k;
                tmp->value = v;
                tmp->next = NULL;
                tmp2->next = tmp;
        } else
                tmp->value = v;

        return m;
}

//readDict: returns the dictionary. NULL if error reading in or empty file.
//user sees it as a void function.
smap_t *readDict(char* filename) {
        smap_t *ans, *tmp;
        FILE *fp;
        ssize_t err;
        char *buf = NULL;
        size_t len = 0;

        fp = fopen(filename, "r");
        if (fp == NULL) {
                fprintf(stderr, "Error reading file %s\n", filename);
                return NULL;
        }
        //read line, parse, allocate
        err = getline(&buf, &len, fp);
        if (!err) {
                fclose(fp);
                free(buf);
                fprintf(stderr, "Error reading file %s\n", filename);
                return NULL;
        }

        if (strlen(buf) == 0 || (strlen(buf) == 1 && buf[0] == '\n')) {
                 fprintf(stderr, "Error: dictionary formatted incorrectly\n");
                 return NULL;
        }
        ans = malloc(sizeof(smap_t));
        buf[strlen(buf) - 1] = 0; //remove newline
        ans->key = buf;
        ans->value = 1;
        ans->next = NULL;
        tmp = ans;
        buf = NULL;
        len = 0;
        err = getline(&buf, &len, fp);
        while (err && buf != NULL && strlen(buf)) {
                buf[strlen(buf) - 1] = 0; //remove newline
                tmp = Stringmapset(ans, buf, 1);
                buf = NULL;
                len = 0;
                err = getline(&buf, &len, fp);
        }
        free(buf); //after getline

        fclose(fp);
        return ans;
}

//converts int to ascii. will return zero if int is
//greater than unsigned 8bit representation. ASCII maps to this
char ToAscii(int i) {
        if (i > 127 || i < 0)
            return 0;
        return (char)i;
}

//LIST FUNCTIONS
//length: we can just use strlen for Charlist.
//here we use a null pointer as a sentinel for listlists
int ListlistgetLength(char **lis) {
        int ans = 0;

        while (lis[ans] != NULL)
                ans++;
        return ans;
}

//Listlist.get(int): return NULL on out of bounds
char* Listlistget(char **lis, int i) {
        if (i >= ListlistgetLength(lis))
                return NULL;
        return lis[i];
}

//Charlist.get(int): return 0 on out of bounds
char Charlistget(char *lis, int i) {
        if (i >= strlen(lis))
                return '\0';
        return lis[i];
}

//list setters: will do nothing if index out of bounds
void Listlistset(char **lis, int i, char *c) {
        if (i >= ListlistgetLength(lis))
                return;
        lis[i] = c; //don't free what's there; we don't know if it's still in use
}

void Charlistset(char *lis, int i, char c) {
        if (i >= strlen(lis))
                return;
        lis[i] = c;
}

//fills a list with unprintable ASCII 1
void FillList(char *lis, int e) {
        int i;

        for (i = 0; i < e; i++)
                lis[i] = 1;
}

//fill a 2d list with 0-terminated strings
void FillListlist(char **lis, int r, int c) {
        int i;

        for (i = 0; i < r; i++) {
                lis[i] = malloc(c * sizeof(char) + 1);
                lis[i][c] = 0;
                FillList(lis[i], c);
        }
}

//length of a charmap
int CharmapgetLength(cmap_t *m) {
        int ans = 0;
        cmap_t *tmp = m;

        while (tmp != NULL) {
                tmp = tmp->next;
                ans++;
        }
        return ans;
}

//length of a stringmap
int StringmapgetLength(smap_t *m) {
        int ans = 0;
        smap_t *tmp = m;

        while (tmp != NULL) {
                tmp = tmp->next;
                ans++;
        }
        return ans;
}

//when c is not in the map it will return
//zero as a "default"
//so you'd better do a contains() check first!
//if the map is empty, returns 0
int Charmapget(cmap_t *m, char c) {
        cmap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL && tmp->key != c)
                tmp = tmp->next;
        return (tmp == NULL ? 0 : tmp->value);
}
int Stringmapget(smap_t *m, char *c) {
        smap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL && strcmp(tmp->key,c) != 0)
                tmp = tmp->next;
        return (tmp == NULL ? 0 : tmp->value);
}


//geti: used by foreach loops to get each element one by one.
char Charmapgeti(cmap_t *m, int i) {
        int ind;
        cmap_t *tmp = m;

        for (ind = 0; ind < i; ind++) {
                if (tmp == NULL)
                        return 0;
                tmp = tmp->next;
        }
        return tmp->key;
}
char *Stringmapgeti(smap_t *m, int i) {
        int ind;
        smap_t *tmp = m;

        for (ind = 0; ind < i; ind++) {
                if (tmp == NULL)
                        return "";
                tmp = tmp->next;
        }
        return tmp->key;
}

//destroys element with given key
//does nothing if key is not in the map
//returns the parameter map in case the pointer
//must be changed
cmap_t *Charmapdestroy(cmap_t *m, char key) {
        cmap_t *tmp = m;
        cmap_t *tmp2 = tmp;

        if (m == NULL)
                return m;

        if (m->key == key) {
                //special case
                tmp2 = m->next;
                free(tmp);
                return tmp2;
        }

        while (tmp != NULL && tmp->key != key) {
                tmp2 = tmp;
                tmp = tmp->next;
        }
        if (tmp != NULL && tmp->key == key) {
                tmp2->next = tmp->next;
                free(tmp);
        }
        return m;
}

smap_t *Stringmapdestroy(smap_t *m, char *key) {
        smap_t *tmp = m;
        smap_t *tmp2 = tmp;

        if (m == NULL)
                return m;

        if (!strcmp(m->key, key)) {
                //special case
                tmp2 = m->next;
                free(tmp);
                return tmp2;
        }

        while (tmp != NULL && !strcmp(tmp->key, key)) {
                tmp2 = tmp;
                tmp = tmp->next;
        }
        if (tmp != NULL && !strcmp(tmp->key, key)) {
                tmp2->next = tmp->next;
                free(tmp);
        }
        return m;
}

//analogous to Stringmapset
cmap_t *Charmapset(cmap_t *m, char k, int v) {
        cmap_t *tmp = m;
        cmap_t *tmp2 = tmp;

        if (m == NULL) {
                m = malloc(sizeof(cmap_t));
                m->key = k;
                m->value = v;
                m->next = NULL;
                return m;
        }
        while (tmp != NULL) {
                if (tmp->key == k)
                        break;
                tmp2 = tmp;
                tmp = tmp->next;
        }
        if (tmp == NULL) {
                tmp = malloc(sizeof(cmap_t));
                tmp->key = k;
                tmp->value = v;
                tmp->next = NULL;
                tmp2->next = tmp;
        } else
                tmp->value = v;

        return m;
}

//returns 0 if k not in m, 1 if k in m
unsigned char Charmapcontains(cmap_t *m, char k) {
        cmap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL) {
                if (tmp->key == k)
                        return 1;
                tmp = tmp->next;
        }
        return 0;
}
unsigned char Stringmapcontains(smap_t *m, char *k) {
        smap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL) {
                if (strcmp(tmp->key, k) == 0)
                        return 1;
                tmp = tmp->next;
        }
        return 0;
}


//helper function
int IsAnagram(char *s1, char *s2) {
        int i;
        int a1[256] = { 0 };
        int a2[256] = { 0 };

        if (s1 == NULL || s2 == NULL)
                return 0;
        if (strlen(s1) != strlen(s2))
                return 0;
        if (strcmp(s1, s2) == 0)
                return 0; //not anagram of self
        for (i = 0; i < strlen(s1); i++) {
                a1[(unsigned int)s1[i]] += 1;
                a2[(unsigned int)s2[i]] += 1;
        }
        for (i = 0; i < 256; i++) {
                if (a1[i] != a2[i])
                        return 0;
        }
        return 1;
}

//finds first valid anagram in the dictionary, or null if none such
//finds via iterating, so "first" is first to be entered in the
//dictionary
char *anagram(smap_t *dictionary, char *s) {
        smap_t *tmp = dictionary;

        while (tmp != NULL) {
                if (IsAnagram(s, tmp->key))
                        return tmp->key;
                tmp = tmp->next;
        }
        return NULL;
}
