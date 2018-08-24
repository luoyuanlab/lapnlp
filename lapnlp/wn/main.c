/*
yluo - 07/09/2010 creation
*/
#include "wordnet.h"

int main (int argc, char *argv[]) {
  SynonymsPtr synonyms;
  SynonymSenseDefsPtr synSenseDefs;
  SynonymSensesPtr hypersenses;
  int i = 0, j=0, senseidx = 0, sensenum = 0;
  if(argc < 2) {
    printf("Please specify a word and a pos to search");
  }else {

    printf("Word is %s\n", argv[1]);
    printf("sense is %s\n", argv[2]);
    // returns non-null if morphword is different from input word
    // printf("Lemma is %s\n", morphword(argv[1], getpos(argv[2])));
    synonyms = synset(argv[1], argv[2]);
    for(i = 0; i < synonyms->wordnum - 1; i++) {
      printf("%s, ", synonyms->ptr[i]);
      free(synonyms->ptr[i]);
    }
    printf("%s\n", synonyms->ptr[i]);
    free(synonyms->ptr[i]);
    free(synonyms->ptr);
    free(synonyms);

    synSenseDefs = synsensedef(argv[1], argv[2]);
	sensenum = synSenseDefs->sensenum;
    printf("sense num is %d\n", synSenseDefs->sensenum);
    for(i = 0; i < synSenseDefs->sensenum; i++) {
      printf("%s\n", synSenseDefs->ptr[i]);
      free(synSenseDefs->ptr[i]);
    }
    free(synSenseDefs);
	printf("======================================\n");
	printf("direct hypernyms\n======================================\n");
	for(senseidx = 1; senseidx <= sensenum; senseidx++) {
	  hypersenses = hypernym(argv[1], argv[2], senseidx);
	  printf("hyper num is %d\n", hypersenses->sensenum);
	  for(i = 0; i < hypersenses->sensenum; i++) {
		for(j = 0; j < hypersenses->sensesize[i]-1; j++) {
		  printf("%s, ", hypersenses->ptr[i][j]); free(hypersenses->ptr[i][j]);
		}
		printf("%s\n", hypersenses->ptr[i][j]); free(hypersenses->ptr[i][j]);
		free(hypersenses->ptr[i]);
	  }
	  free(hypersenses);
	}
	printf("======================================\n");
	printf("top hypernyms\n======================================\n");
	for(senseidx = 1; senseidx <= sensenum; senseidx++) {
	  hypersenses = top_hypernym(argv[1], argv[2], senseidx);
	  printf("hyper num is %d\n", hypersenses->sensenum);
	  for(i = 0; i < hypersenses->sensenum; i++) {
		for(j = 0; j < hypersenses->sensesize[i]-1; j++) {
		  printf("%s, ", hypersenses->ptr[i][j]); free(hypersenses->ptr[i][j]);
		}
		printf("%s\n", hypersenses->ptr[i][j]); free(hypersenses->ptr[i][j]);
		free(hypersenses->ptr[i]);
	  }
	  free(hypersenses);
	}
	printf("======================================\n");
  }
  return 0;
}
