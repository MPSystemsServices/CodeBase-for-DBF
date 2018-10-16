/***********************************************************************\
*                                                                       *
*   RELATE2.C     Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 6 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 codeBase;
DATA4 *student = NULL;

void openDataFiles(void) ;

void openDataFiles(void)
{
    student = d4open(&codeBase,"student");

    error4exitTest(&codeBase);
}

void printRecord(DATA4 *dataFile)
{
    short j;

    for(j=1;j<=d4numFields(dataFile);j++)
       printf("%s ",f4memoStr(d4fieldJ(dataFile,j)));
    printf("\n");
}

void query(DATA4 *dataFile,char *expr,char *order)
{
   RELATE4 *relation = NULL;
   int rc;

   relation = relate4init(dataFile);
   if(relation == NULL) exit(1);

   relate4querySet(relation,expr);
   relate4sortSet(relation,order);

   for(rc = relate4top(relation);rc != r4eof
                ;rc = relate4skip(relation,1L))
      printRecord(dataFile);

   printf("\n");

   code4unlock(&codeBase);
   relate4free(relation,0);
}

void main(void)
{
    code4init(&codeBase);
    openDataFiles();

    query(student,"AGE > 30","");
    query(student,"UPPER(L_NAME) = 'MILLER'","L_NAME + F_NAME");

    code4close(&codeBase);
    code4initUndo(&codeBase);
}
