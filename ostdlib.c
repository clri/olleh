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

typedef struct player {
        int score;
        unsigned char turn; //should be good for alignment
        char *letters;
        smap_t *guessedWords;
} player_t;


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

//int to string
char *IntToS(int e) {
        char *ans;
        int s = GetElemSpace(e) + 2;
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
        for (i = 0; i < n - 1; i++)
                printf("'%c', ", lis[i]);
        printf("'%c']", lis[i]);
}

//print charlist
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

//@TODO: print bool, charmap, stringmap


//called once at the beginning of each program
//to initialize the RNG
void InitializeRandom(void) {
        srand(time(NULL));
}

//random() function maps to this
int OllehRandom(int max) {
        return rand() % max;
}

//scramble(): scrambles a string as specified in LRM
char* scramble(char* w) {
        int r;
        int i = 0;
        int len = strlen(w);
        char *ans = malloc(len + 1);

        memset(ans, 0, len+1);

        while (i < len) {
                r = OllehRandom(len);
                if (ans[r] == 0)
                        ans[r] = w[i++];
        }
        return ans;
}

//returns the reverse of a string
char* reverse(char* w) {
        int i;
        int len = strlen(w);
        char *ans = malloc(len + 1);

        for (i = 0; i < len; i++) {
                ans[i] = w[len - i - 1];
        }
        ans[len] = 0;
        return ans;
};

//returns a line without newline char, also used as helper function for readDict
char* ReadInput(void) {
        char *ans = NULL;
        size_t len = 0;
        ssize_t err = getline(&ans, &len, stdin);
        if (!err) return "";
        ans[strlen(ans) - 1] = 0; //remove newline
        return ans;
}

//converts int to ascii. will return zero if int is
//greater than unsigned 8bit representation. ASCII maps to this
char ToAscii(int i) {
        if (i > 256 || i < 0)
            return 0;
        return (char)i;
}

//@TODO: IMPLEMENT BELOW
char* anagram(char* w); //may take additional param for dictionary
int readDict(char* filename); //bool return val, may take additional param for dictionary
//void map.destroy(map<type> k) how to implement?
//int Mapcontains(map<type> k) bool return val, how to implement? (Stringmapcontains, Charmapcontains...)
//int map.getLength(void) how to implement?
//map<String> subStrings(String w) how to implement?

//LIST FUNCTIONS
//length: we can just use strlen for Charlist.
//here we use a null pointer as a sentinel for listlists
int Listlistlen(char **lis) {
        int ans = 0;

        while (lis[ans] != NULL)
                ans++;
        return ans;
}

//Listlist.get(int): return NULL on out of bounds
char* ListlistGet(char **lis, int i) {
        if (i > Listlistlen(lis))
                return NULL;
        return lis[i];
}

//Charlist.get(int): return 0 on out of bounds
char CharlistGet(char *lis, int i) {
        if (i > strlen(lis))
                return '\0';
        return lis[i];
}

//list to string: doesn't really do much, but
//olleh pretends there is a difference
char *ListToString(char *lis) {
        return lis;
}

//neither does stringToList, but since
//we don't want users to manipulate strings
//as strings they'll have to do it as a list
char *StringToList(char *lis) {
        return lis;
}


//MAP FUNCTIONS
int CharmapgetLength(cmap_t *m) {
        int ans = 0;
        cmap_t *tmp = m;

        while (tmp != NULL) {
                tmp = tmp->next;
                ans++;
        }
        return ans;
}

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
//the value of some element in the map.
//so you'd better do a contains() check first!
//if the map is empty, returns 0
int CharmapGet(cmap_t *m, char c) {
        cmap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL && tmp->key != c)
                tmp = tmp->next;
        return (tmp == NULL ? m->value : tmp->value);
}

int StringmapGet(smap_t *m, char *c) {
        smap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL && strcmp(tmp->key,c) != 0)
                tmp = tmp->next;
        return (tmp == NULL ? m->value : tmp->value);
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
                tmp = tmp->next;
                tmp2 = tmp;
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
                tmp = tmp->next;
                tmp2 = tmp;
        }
        if (tmp != NULL && !strcmp(tmp->key, key)) {
                tmp2->next = tmp->next;
                free(tmp);
        }
        return m;
}

//setters: add a node to the map if none exists, or
//change the existing value
cmap_t *CharmapSet(cmap_t *m, char k, int v) {
        cmap_t *tmp = m;
        cmap_t *tmp2 = tmp;

        if (m == NULL) {
                m = malloc(sizeof(cmap_t));
                m->key = k;
                m->value = v;
                m->next = NULL;
                return m;
        }
        while (tmp != NULL && tmp->key != k) {
                tmp = tmp->next;
                tmp2 = tmp;
        }
        if (tmp == NULL) {
                tmp = malloc(sizeof(cmap_t));
                tmp2->next = tmp;
                tmp->key = k;
                tmp->value = v;
                tmp->next = NULL;
        } else
                tmp->value = v;

        return m;
}

smap_t *StringmapSet(smap_t *m, char *k, int v) {
        smap_t *tmp = m;
        smap_t *tmp2 = tmp;

        if (m == NULL) {
                m = malloc(sizeof(smap_t));
                m->key = k;
                m->value = v;
                m->next = NULL;
                return m;
        }
        while (tmp != NULL && !strcmp(tmp->key, k)) {
                tmp = tmp->next;
                tmp2 = tmp;
        }
        if (tmp == NULL) {
                tmp = malloc(sizeof(smap_t));
                tmp2->next = tmp;
                tmp->key = k;
                tmp->value = v;
                tmp->next = NULL;
        } else
                tmp->value = v;

        return m;
}

//for contains, how to return a bool? use unsigned char
unsigned char Charmapcontains(cmap_t *m, char k) {
        cmap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL) {
                tmp = tmp->next;
                if (tmp->key == k)
                        return 1;
        }
        return 0;
}

unsigned char Stringmapcontains(smap_t *m, char *k) {
        smap_t *tmp = m;

        if (m == NULL)
                return 0;
        while (tmp != NULL) {
                tmp = tmp->next;
                if (strcmp(tmp->key, k))
                        return 1;
        }
        return 0;
}

smap_t *subStrings(char *s);



//
