/*
yluo - 07/09/2010 creation
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wn.h"

typedef struct {
  char** ptr;
  int wordnum;
} Synonyms;

typedef struct {
  int sensenum;
  char*** ptr;
  int* sensesize;
} SynonymSenses;

typedef struct {
  int sensenum;
  char** ptr;
} SynonymSenseDefs;

typedef Synonyms *SynonymsPtr;
typedef SynonymSenses *SynonymSensesPtr;
typedef SynonymSenseDefs *SynonymSenseDefsPtr;

SynonymsPtr synset(char* word, char* spos);
SynonymSensesPtr synsense(char* word, char* spos);
SynonymSensesPtr hypernym(char* word, char* spos, int senseidx);
SynonymSensesPtr top_hypernym(char* word, char* spos, int senseidx);
SynonymSenseDefsPtr synsensedef(char* word, char* spos);
