#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

//dictionary, letterScores, and seeds--must they exist here as constants as
//well or shall we pass them in fron codegen.ml?


char *SConcat(char *a, char *b) {
        char *ans;
        int n = strlen(a) + strlen(b) + 1;

        ans = malloc(n);
        strcpy(ans, a);
        strcpy(ans + strlen(a), b);
        ans[n-1] = 0;
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
        int s = GetElemSpace(e) + 2;
        ans = malloc(s);
        sprintf(ans, "%d", e);
        ans[s - 1] = 0;
        return ans;
}


int MAXINT = 1073741823;

int GetLengthOfIList(int lis[]) {
        int i = 0;
        while (lis[i] != MAXINT) i++;
        return i;
}

//print a list of ints
void ListOfIntsToString(int lis[]) {
        int i;
        printf("[");
        int n = GetLengthOfIList(lis);
        for (i = 0; i < n - 1; i++)
                printf("%d, ", lis[i]);
        printf("%d]\n", lis[i]);

}

void PrintCharLis(char *lis) {
        int i;
        printf("[");
        int n = strlen(lis);
        for (i = 0; i < n - 1; i++)
                printf("%c, ", lis[i]);
        printf("%c]\n", lis[i]);
}

char *listToString(char *lis) {
        return lis;
}


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
        ans[strlen(ans) - 1] = 0; //remove newline
        return ans;
}


//@TODO: IMPLEMENT BELOW
char* anagram(char* w); //may take additional param for dictionary
int readDict(char* filename); //bool return val, may take additional param for dictionary
//void map.destroy(map<type> k) how to implement?
//int map.contains(map<type> k) bool return val, how to implement?
//int map.getLength(void) how to implement?
//map<String> subStrings(String w) how to implement?




/*TEST ONLY: REMOVE FOR FINAL DELIVERABLE
int main(void) {
        char *v = readInput();
        v = "ASDF";
        fprintf(stderr,"%s\n",v);
        v = reverse(v);
        fprintf(stderr,"%s\n",v);
        return 1;
}*/
