/*
  yluo - 07/09/2010 creation
*/
#include "wordnet.h"

SynonymsPtr synset(char* word, char* spos) {
  SynonymsPtr res;
  char* base;
  SynsetPtr synlist = NULL, cursyn = NULL;
  int wordnum = 0, wordidx = 0, i;
  int pos;
  if (wninit()) {		/* open database */
    display_message("wn: Fatal error - cannot open WordNet database\n");
    exit (-1);
  }

  res = (SynonymsPtr) malloc(sizeof(Synonyms));
  memset(res, 0, sizeof(Synonyms));
  pos = getpos(spos);
  // obtain base form of word in pos
  base = morphword(word, pos);
  if(base == NULL) 
    base = word;

  // obtain synset list
  synlist = findtheinfo_ds(base, pos, SYNS, ALLSENSES);

  // count the total number words in all synsets retreived
  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
    wordnum += cursyn->wcount;
  }
  res->wordnum = wordnum;
  // printf("Total wordnum is: %d\n", wordnum);
  
  // allocate first dimension of res;
  res->ptr = (char**) malloc( wordnum * sizeof(char*));
  memset(res->ptr, 0, wordnum * sizeof(char*));
  // for each sense, obtain the synset, remember to free the Synset linked list
  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
    for(i = 0; i < cursyn->wcount; i++) {
      res->ptr[wordidx] = (char*) malloc( (strlen(cursyn->words[i])+1) *  
										  sizeof(char));
      memcpy(res->ptr[wordidx], cursyn->words[i], strlen(cursyn->words[i])+1);
      wordidx++;
    }
    // printf("%s\n", FmtSynset(cursyn, 0));
  }
  free_syns(synlist);
  // return the Synonyms structure, haven't removed duplicates
  return res;
}

SynonymSensesPtr synsense(char* word, char* spos) {
  SynonymSensesPtr res;
  char* base;
  SynsetPtr synlist = NULL, cursyn = NULL;
  int sensenum = 0, i;
  int pos, senseidx = 0;
  if (wninit()) {		/* open database */
    display_message("wn: Fatal error - cannot open WordNet database\n");
    exit (-1);
  }

  res = (SynonymSensesPtr) malloc(sizeof(SynonymSenses));
  memset(res, 0, sizeof(SynonymSenses));
  pos = getpos(spos);
  // obtain base form of word in pos
  base = morphword(word, pos);
  if(base == NULL) 
    base = word;

  // obtain synset list
  synlist = findtheinfo_ds(base, pos, SYNS, ALLSENSES);

  // count the total number words in all synsets retreived
  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
    sensenum++;
  }
  res->sensenum = sensenum;
  res->sensesize = (int*) malloc(sensenum * sizeof(int*));
  // allocate first dimension of res;
  res->ptr = (char***) malloc(sensenum * sizeof(char**));
  memset(res->ptr, 0, sensenum * sizeof(char**));

  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
    res->sensesize[senseidx] = cursyn->wcount;
    // allocate second dimension of res
    res->ptr[senseidx] = (char**) malloc( cursyn->wcount * sizeof(char*));
    memset(res->ptr[senseidx], 0, cursyn->wcount * sizeof(char*));
    printf("%s :\n", FmtSynset(cursyn, 0));

    for(i = 0; i < cursyn->wcount; i++) {
      res->ptr[senseidx][i] = (char*) malloc( (strlen(cursyn->words[i])+1) *  
											  sizeof(char) );
      memcpy(res->ptr[senseidx][i], cursyn->words[i], 
			 strlen(cursyn->words[i])+1);
      //printf("%s ", res->ptr[senseidx][i]);
    }
    //printf("\n");
    senseidx++;
  }  
  free_syns(synlist);
  //printf("res: &sensenum: %p, sensesize: %p, ptr: %p\n", 
  // &(res->sensenum), res->sensesize, res->ptr);
  return res;
}

SynonymSensesPtr hypernym(char* word, char* spos, int senseidx) {
  // find the direct hypernyms of the synsense or a word having part-of-speech 
  // spos (indexed by sense_num)
  SynonymSensesPtr res;
  char* base;
  SynsetPtr synsense = NULL, hyperlist = NULL, curhyper = NULL;
  int hypernum = 0, i;
  int pos, hyperidx = 0;
  if (wninit()) {		/* open database */
    display_message("wn: Fatal error - cannot open WordNet database\n");
    exit (-1);
  }

  res = (SynonymSensesPtr) malloc(sizeof(SynonymSenses));
  memset(res, 0, sizeof(SynonymSenses));
  pos = getpos(spos);
  // obtain base form of word in pos
  base = morphword(word, pos);
  if(base == NULL) 
    base = word;

  // obtain the synsense indexed by senseidx, and then the hypernym list
  synsense = findtheinfo_ds(base, pos, HYPERPTR, senseidx);
  hyperlist = synsense->ptrlist;

  // count the total number words in all synsets retreived
  for(curhyper = hyperlist; curhyper != NULL; curhyper = curhyper->nextss) {
    hypernum++;
  }

  res->sensenum = hypernum;
  res->sensesize = (int*) malloc(res->sensenum * sizeof(int*));
  // allocate & init first dimension of res;
  res->ptr = (char***) malloc(res->sensenum * sizeof(char**));
  memset(res->ptr, 0, res->sensenum * sizeof(char**));

  for(curhyper = hyperlist; curhyper != NULL; curhyper = curhyper->nextss) {
    res->sensesize[hyperidx] = curhyper->wcount;
    // allocate & init second dimension of res
    res->ptr[hyperidx] = (char**) malloc( curhyper->wcount * sizeof(char*));
    memset(res->ptr[hyperidx], 0, curhyper->wcount * sizeof(char*));
    // printf("%s :\n", FmtSynset(curhyper, 0));

    for(i = 0; i < curhyper->wcount; i++) {
	  // allocate & set third dimension of res
      res->ptr[hyperidx][i] = (char*) malloc( (strlen(curhyper->words[i])+1) *  
											  sizeof(char) );
      memcpy(res->ptr[hyperidx][i], curhyper->words[i], 
			 strlen(curhyper->words[i])+1);
      //printf("%s ", res->ptr[senseidx][i]);
    }
    //printf("\n");
    hyperidx++;
  }  
  free_syns(synsense);
  //printf("res: &sensenum: %p, sensesize: %p, ptr: %p\n", 
  // &(res->sensenum), res->sensesize, res->ptr);
  return res;
}

SynonymSensesPtr top_hypernym(char* word, char* spos, int senseidx) {
  // find the top hypernyms of the synsense or a word having part-of-speech 
  // spos (indexed by sense_num), this traces the hypernym chain to the top
  SynonymSensesPtr res;
  char* base;
  SynsetPtr synsense = NULL, hyperlist = NULL, curhyper = NULL, tophyper = NULL;
  int i;
  int pos;
  if (wninit()) {		/* open database */
    display_message("wn: Fatal error - cannot open WordNet database\n");
    exit (-1);
  }

  res = (SynonymSensesPtr) malloc(sizeof(SynonymSenses));
  memset(res, 0, sizeof(SynonymSenses));
  pos = getpos(spos);
  // obtain base form of word in pos
  base = morphword(word, pos);
  if(base == NULL) 
    base = word;

  // obtain the synsense indexed by senseidx, and then the hypernym list
  synsense = findtheinfo_ds(base, pos, SYNS, senseidx);
  hyperlist = traceptrs_ds(synsense, HYPERPTR, pos, 1);

  /* traverse up the hypernym hierarchy to find the top, assumed the following:
	 1. linked list by nextss are always ordered from most frequent to least
	 2. in the case of multiple inheritance, frequency can be a reasonable 
	 selection criteria to pick a unique hypernym
   */ 
  for(curhyper = hyperlist; curhyper != NULL; curhyper = curhyper->ptrlist) {
    tophyper = curhyper;
  }
  // if the current synsense does not have a hypernym
  if(tophyper == NULL) 
	tophyper = synsense;
  res->sensenum = 1;
  res->sensesize = (int*) malloc(res->sensenum * sizeof(int*));
  // allocate & init first dimension of res;
  res->ptr = (char***) malloc(res->sensenum * sizeof(char**));
  memset(res->ptr, 0, res->sensenum * sizeof(char**));


  res->sensesize[0] = tophyper->wcount;
  // allocate & init second dimension of res
  res->ptr[0] = (char**) malloc( tophyper->wcount * sizeof(char*));
  memset(res->ptr[0], 0, tophyper->wcount * sizeof(char*));
  // printf("%s :\n", FmtSynset(tophyper, 0));

  for(i = 0; i < tophyper->wcount; i++) {
	// allocate & set third dimension of res
	res->ptr[0][i] = (char*) malloc( (strlen(tophyper->words[i])+1) *  
											sizeof(char) );
	memcpy(res->ptr[0][i], tophyper->words[i], 
		   strlen(tophyper->words[i])+1);
	//printf("%s ", res->ptr[senseidx][i]);
  }
  //printf("\n");
  free_syns(hyperlist);
  free_syns(synsense);

  //printf("res: &sensenum: %p, sensesize: %p, ptr: %p\n", 
  // &(res->sensenum), res->sensesize, res->ptr);
  return res;
}

SynonymSenseDefsPtr synsensedef(char* word, char* spos) {
  SynonymSenseDefsPtr res;
  char* base;
  SynsetPtr synlist = NULL, cursyn = NULL;
  int sensenum = 0;
  int pos, senseidx = 0;
  if (wninit()) {		/* open database */
    display_message("wn: Fatal error - cannot open WordNet database\n");
    exit (-1);
  }

  res = (SynonymSenseDefsPtr) malloc(sizeof(SynonymSenseDefs));
  memset(res, 0, sizeof(SynonymSenseDefs));
  pos = getpos(spos);
  // obtain base form of word in pos
  base = morphword(word, pos);
  if(base == NULL) 
    base = word;

  // obtain synset list
  synlist = findtheinfo_ds(base, pos, SYNS, ALLSENSES);

  // count the total number words in all synsets retreived
  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
    sensenum++;
  }
  res->sensenum = sensenum;
  // allocate first dimension of res;
  res->ptr = (char**) malloc(sensenum * sizeof(char*));
  memset(res->ptr, 0, sensenum * sizeof(char*));

  for(cursyn = synlist; cursyn != NULL; cursyn = cursyn->nextss) {
	// allocate second dimension of res
    res->ptr[senseidx] = (char*) malloc((1+strlen(cursyn->defn))*sizeof(char));
    // printf("%s (%s):\n", FmtSynset(cursyn, 0), cursyn->defn);

    memcpy(res->ptr[senseidx], cursyn->defn, strlen(cursyn->defn)+1);
    senseidx++;
  }  
  free_syns(synlist);
  //printf("res: &sensenum: %p, sensesize: %p, ptr: %p\n", 
  // &(res->sensenum), res->sensesize, res->ptr);
  return res;
}
