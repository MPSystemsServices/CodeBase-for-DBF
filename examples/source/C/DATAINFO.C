/***********************************************************************\
*                                                                       *
*   DATAINFO.C    Copyright (C) 1999 Sequiter Software Inc.             *
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
   CODE4    codeBase;
   DATA4    *dataFile;
   FIELD4   *field;

   short j;
   int numFields;
   int len, dec;
   int recWidth;
   const char *name;
   char type;
   const char *alias;
   long recCount;

   #ifdef S4MACINTOSH
      argc = ccommand(&argv) ;
   #endif

   if(argc != 2)
   {
      printf(" USAGE: FLDINFO <FILENAME.DBF> \n");
      exit(0);
   }

   code4init(&codeBase);

   dataFile = d4open(&codeBase,argv[1]);
   error4exitTest(&codeBase);

   recCount = d4recCount(dataFile);
   numFields = d4numFields(dataFile);
   recWidth = d4recWidth(dataFile);
   alias = d4alias(dataFile);

   printf("ษอออออออออออออออออออออออออออออออออป\n");
   printf("บ Data File: %12s         บ\n",argv[1]);
   printf("บ Alias    : %12s         บ\n",alias);
   printf("บ                                 บ\n");
   printf("บ Number of Records: %7ld      บ\n",recCount);
   printf("บ Length of Record : %7d      บ\n",recWidth);
   printf("บ Number of Fields : %7d      บ\n",numFields);
   printf("บ                                 บ\n");
   printf("บ Field Information :             บ\n");
   printf("ฬออออออออออออหออออออหออออออหออออออน\n");
   printf("บ Name       บ type บ len  บ dec  บ\n");
   printf("ฬออออออออออออฮออออออฮออออออฮออออออน\n");

   for(j = 1;j <= d4numFields(dataFile);j ++)
   {
      field = d4fieldJ(dataFile,j);
      name = f4name(field);
      type = f4type(field);
      len = f4len(field);
      dec = f4decimals(field);

      printf("บ %10s บ   %c  บ %4d บ %4d บ\n",name,type,len,dec);

   }
   printf("ศออออออออออออสออออออสออออออสออออออผ\n");

   d4close(dataFile);
   code4initUndo(&codeBase);
   return 0;
}
