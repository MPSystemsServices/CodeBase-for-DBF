/***********************************************************************\
*                                                                       *
*   SHOWDATA.C    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#ifdef S4MACINTOSH
   #include <console.h>
#endif

int main(int argc,char *argv[])
{
   CODE4 codeBase;
   DATA4 *dataFile = NULL;
   FIELD4 *field = NULL;

   int rc, numFields;
   short j ;

   #ifdef S4MACINTOSH
      argc = ccommand(&argv) ;
   #endif

   if(argc != 2)
   {
      printf(" USAGE: SHOWDATA <FILENAME.DBF> \n");      exit(0);
   }

   code4init(&codeBase);

   dataFile = d4open(&codeBase,argv[1]);
   error4exitTest(&codeBase);

   numFields = d4numFields(dataFile);
   for(rc = d4top(dataFile);rc == r4success
                      ;rc = d4skip(dataFile, 1L))
   {
      for(j = 1;j <= numFields;j ++)
      {
         field = d4fieldJ(dataFile,j);
         printf("%s ", f4memoStr(field));
      }
      printf("\n");
   }

   d4close(dataFile);
   code4initUndo( &codeBase );
   return 0;
}
