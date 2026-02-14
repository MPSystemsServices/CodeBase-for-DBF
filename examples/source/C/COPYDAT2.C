/***********************************************************************\
*                                                                       *
*   COPYDAT2.C    Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.h"
#ifdef S4MACINTOSH
   #include <console.h>
#endif

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main(int argc,char *argv[])
{
   CODE4      codeBase;
   DATA4      *dataFile;
   INDEX4     *index;
   FIELD4INFO *fieldInfo;
   TAG4INFO   *tagInfo = NULL;

   #ifdef S4MACINTOSH
      argc = ccommand(&argv) ;
   #endif

   if(argc != 3)
   {
      printf("USAGE: COPYDATA <FROMFILE> <TOFILE>\n");
      exit(1);
   }

   code4init(&codeBase);
   codeBase.safety = 0;
   dataFile = d4open(&codeBase,argv[1]);
   error4exitTest(&codeBase);

   index = d4index(dataFile,argv[1]);
   if(index) tagInfo = i4tagInfo(index);

   fieldInfo = d4fieldInfo(dataFile);
   d4create(&codeBase,argv[2],fieldInfo,tagInfo);
   u4free(fieldInfo);
   u4free(tagInfo);
   code4close(&codeBase);
   code4initUndo(&codeBase);
}
