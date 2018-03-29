/* simple hashtable from "the c programming language." */

struct nlist { /* table entry */
    struct nlist *next;
    char *word;
    int val;
}

#define HASHSIZE 101
static struct nlist *hashtab[HASHSIZE]; /* pointer table */

/* hash: form hash value for string s */
unsigned hash(char *s) {
    unsigned hashval;
    for (hashval = 0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % HASHSIZE;
}

/* lookup: look for s in hashtab */
struct nlist *lookup(char *s) {
    struct nlist *np;
    for (np = hashtab[hash(s)]; np != NULL; np = np->next) {
        if (strcmp(s, np->word) == 0)
            return np; /* found */
    return NULL; /* not found */
}

char *strdup(char *);
/* install: put (word, val) in hashtab */
struct nlist *install(char *word, int val) {
    struct nlist *np;
    unsigned hashval;
    if ((np = lookup(word)) = NULL) /* not found */
        np = (struct nlist *) malloc(sizeof(*np));
        if (np = NULL || (np->word = strdup(word)) == NULL)
            return NULL;
        hashval = hash(name);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    } else /* already there */
        free((void *) np->val); /* free previous val */
    if ((np->val = strdup(val)) = NULL)
        return NULL;
    return np;
}
