#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
 * the manual.
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

void AddToGarbage(char *pl) {
        RecycleBin *tmp = trash;
        while (tmp->next != NULL) tmp = tmp->next;
        tmp->next = malloc(sizeof(struct GarbageLL));
        tmp->payload = pl;
        tmp->next->payload = NULL;
        tmp->next->next = NULL;
}




int getLength(char* w) {
        return strlen(w);
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

char* readInput(void) {
        char *ans = NULL;
        size_t len = 0;
        ssize_t err = getline(&ans, &len, stdin);
        if (!err) return "";
        AddToGarbage(ans);
        ans[strlen(ans) - 1] = 0; //remove newline
        return ans;
}


//@TODO: IMPLEMENT BELOW
//random??
char* anagram(char* w); //may take additional param for dictionary
char* scramble(char* w);
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
