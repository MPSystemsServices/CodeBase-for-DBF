/***********************************************************************\
*                                                                       *
*   COPYDATA.C    Copyright (C) 1999 Sequiter Software Inc.             *
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

void main(int argc,char *argv[])
{
   CODE4      codeBase;
   DATA4      *dataFile,*dataCopy;
   FIELD4INFO *fieldInfo;

   #ifdef S4MACINTOSH
      argc = ccommand(&argv) ;
   #endif

   if(argc != 3)
   {
      printf("USAGE: COPYDATA <FROM FILE> <TO FILE>\n" ) ;
      exit(1);
   }

   code4init(&codeBase);
   codeBase.safety = 0;

   dataFile = d4open(&codeBase,argv[1]);
   error4exitTest(&codeBase);

   fieldInfo = d4fieldInfo(dataFile);

   dataCopy = d4create(&codeBase,argv[2],fieldInfo,0);

   u4free(fieldInfo);

   code4close(&codeBase);
   code4initUndo(&codeBase);
}
