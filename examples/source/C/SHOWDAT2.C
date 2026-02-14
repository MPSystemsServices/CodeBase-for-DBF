/***********************************************************************\
*                                                                       *
*   SHOWDAT2.C    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.h"
#ifdef S4WINTEL
   #include <conio.h>
#endif

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#ifdef S4MACINTOSH
   #include <console.h>
#endif

CODE4 codeBase;
DATA4 *dataFile = NULL;
FIELD4 *field = NULL;
TAG4 *tag = NULL;

int rc;
short j;
const char *fieldContents;

void printRecords(void)
{
   for(rc = d4top(dataFile);rc == r4success
                       ;rc = d4skip(dataFile, 1L))
   {
      for(j = 1;j <= d4numFields(dataFile);j ++)
      {
         field = d4fieldJ(dataFile,j);
         fieldContents = f4memoStr(field);
         printf("%s ",fieldContents);
      }
      printf("\n");
   }
}

int main(int argc,char *argv[])
{
   #ifdef S4MACINTOSH
      argc = ccommand(&argv) ;
   #endif

   if(argc != 2)
   {
     printf(" USAGE: SHOWDAT2 <FILENAME.DBF> \n");      exit(0);
   }

   code4init(&codeBase);

   dataFile = d4open(&codeBase,argv[1]);
   error4exitTest(&codeBase);

   printf("Data File %s in Natural Order\n"
                                        ,argv[1]);
   printRecords();
   for(tag = d4tagNext(dataFile,NULL);tag !=NULL
                ; tag = d4tagNext(dataFile,tag))
   {
      printf("\nPress ENTER to continue:");
      #ifdef S4UNIX
         getchar() ;
      #else
         getch();
      #endif

      printf("\nData File %s sorted by Tag %s\n"
                             ,argv[1],t4alias(tag));
      d4tagSelect(dataFile,tag);
      printRecords();
   }

   d4close(dataFile);
   code4initUndo( &codeBase ) ;
   mem4reset() ;
   return 0;
}
