#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

//dictionary, letterScores, and seeds--must they exist here as constants as
//well or shall we pass them in fron codegen.ml?


/*
 *this typedef and the next three functions are for garbage collection.
 * They may not be invoked by a valid olleh program, and will fail on the Scan
 * pass if the user attempts to invoke them (since valid olleh identifiers don't
 * begin with capital letters). They are for internal use ONLY. The compiler
 * will insert the InitializeLocalGarbage() code at the beginning of each
 * function, and will insert the CollectLocalGarbage() code at the end of each
 * function. For more on how the garbage collection mechanism works, please read
 * the LRM.
 */
typedef struct GarbageLL {
        char *payload;
        struct GarbageLL *next;
} RecycleBin;
RecycleBin *trash;

typedef struct TrashStack {
        struct GarbageLL *payload;
        struct TrashStack *next;
} TStack;
TStack *tstack;

void InitializeLocalGarbage(void) {
        TStack *t = malloc(sizeof(TStack));

        trash = malloc(sizeof(struct GarbageLL));
        trash->payload = NULL;
        trash->next = NULL;

        //push new trash onto the stack
        t->payload = trash;
        t->next = tstack;
        tstack = t;
}

void AddToGarbage(char *pl) {
        RecycleBin *tmp = trash;
        while (tmp->next != NULL) tmp = tmp->next;
        tmp->next = malloc(sizeof(struct GarbageLL));
        tmp->payload = pl;
        tmp->next->payload = NULL;
        tmp->next->next = NULL;
}

void CollectLocalGarbage(void) {
        RecycleBin *tmp;
        TStack *t;

        while (trash->next != NULL) {
                tmp = trash->next;
                free(trash->payload);
                free(trash);
                trash = tmp;
        }
        free(trash);
        t = tstack;
        tstack = tstack->next; //pop off the stack
        free(t);
}

void CollectLocalGarbageWithReturn(char *returnVal) {
        RecycleBin *tmp;
        TStack *t = tstack;

        if (returnVal == NULL) {
                CollectLocalGarbage();
                return;
        }

        while (trash->next != NULL) {
                tmp = trash->next;
                if (trash->payload != returnVal) {
                        free(trash->payload);
                }
                free(trash);
                trash = tmp;
        }
        free(trash);
        tstack = tstack->next; //pop off the stack
        AddToGarbage(returnVal);
        free(t);
}

char * SConcat(char *a, char*b) {
        char *ans;
        int n = strlen(a) + strlen(b) +1;

        ans = malloc(n);
        AddToGarbage(ans);
        strcpy(ans, a);
        strcat(ans, b);
        return ans;
}

//helper functions

int GetElemSpace(int e) {
        int ans = 9;
        int powers[] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};

        while (e / powers[ans] == 0)
                ans--;
        return ans;
}
char *IntToS(int e) {
        char *ans;
        int s = GetElemSpace(e) + 1;
        ans = malloc(s);
        AddToGarbage(ans);
        sprintf(ans, "%d", e);
        ans[s - 1] = 0;
        return ans;
}

/*
int GetListSpace(int * lis, int n) {
        int i;
        int ans = 0;

        for (i = 0; i < n; i++)
                ans += getElemSpace(lis[i]);
        return ans;
}*/

//print a list of ints
void ListOfIntsToString(int *lis, int n) {
        int i;
        printf("[");
        for (i = 0; i < n - 1; i++)
                printf("%d, ", lis[i]);
        printf("%d]\n", lis[i]);
        /*int i = 0;
        int j;
        char *ans = malloc (2 * n + 1 + getListSpace(lis, n));

        ans[i++] = '[';
        for (j = 0; j < n - 1; j++) {
                itoa(lis[j], ans[i], 10);
                i += getElemSpace(lis[j]);
                ans[i++] = ',';
                ans[i++] = ' ';
        }
        itoa(lis[j], ans[i], 10);
        ans[i++] = ']';
        ans[i] = '\0';

        printf("%s\n", ans);
        free(ans);*/
}

//@TODO: print a list of chars


//called once at the beginning of each program
//to initialize the RNG
void InitializeRandom(void) {
        srand(time(NULL));
}

int OllehRandom(int max) {
        return rand() % max;
}

char* scramble(char* w) {
        int r;
        int i = 0;
        int len = strlen(w);
        char *ans = malloc(len + 1);

        memset(ans, 0, len+1);
        AddToGarbage(ans);

        while (i < len) {
                r = OllehRandom(len);
                if (ans[r] == 0)
                        ans[r] = w[i++];
        }
        return ans;
}

char* reverse(char* w) {
        int i;
        int len = strlen(w);
        char *ans = malloc(len + 1);

        AddToGarbage(ans);
        for (i = 0; i < len; i++) {
                ans[i] = w[len - i - 1];
        }
        ans[len] = 0;
        return ans;
};

//returns a line without newline char, helper function for readDict
char* ReadInput(void) {
        char *ans = NULL;
        size_t len = 0;
        ssize_t err = getline(&ans, &len, stdin);
        if (!err) return "";
        AddToGarbage(ans);
        ans[strlen(ans) - 1] = 0; //remove newline
        return ans;
}


//@TODO: IMPLEMENT BELOW
char* anagram(char* w); //may take additional param for dictionary
int readDict(char* filename); //bool return val, may take additional param for dictionary
//void map.destroy(map<type> k) how to implement?
//int map.contains(map<type> k) bool return val, how to implement?
//int map.getLength(void) how to implement?
//char* listToString(list lis[]); //should we implement list as a linked list so we can track length dynamically?
//map<String> subStrings(String w) how to implement?




/*TEST ONLY: REMOVE FOR FINAL DELIVERABLE
int main(void) {
        InitializeLocalGarbage();
        char *v = readInput();
        v = "ASDF";
        fprintf(stderr,"%s\n",v);
        v = reverse(v);
        fprintf(stderr,"%s\n",v);
        CollectLocalGarbage();
        return 1;
}*/
