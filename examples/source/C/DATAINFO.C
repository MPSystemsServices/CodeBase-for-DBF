/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

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

   printf("���������������������������������ͻ\n");
   printf("� Data File: %12s         �\n",argv[1]);
   printf("� Alias    : %12s         �\n",alias);
   printf("�                                 �\n");
   printf("� Number of Records: %7ld      �\n",recCount);
   printf("� Length of Record : %7d      �\n",recWidth);
   printf("� Number of Fields : %7d      �\n",numFields);
   printf("�                                 �\n");
   printf("� Field Information :             �\n");
   printf("���������������������������������͹\n");
   printf("� Name       � type � len  � dec  �\n");
   printf("���������������������������������͹\n");

   for(j = 1;j <= d4numFields(dataFile);j ++)
   {
      field = d4fieldJ(dataFile,j);
      name = f4name(field);
      type = f4type(field);
      len = f4len(field);
      dec = f4decimals(field);

      printf("� %10s �   %c  � %4d � %4d �\n",name,type,len,dec);

   }
   printf("���������������������������������ͼ\n");

   d4close(dataFile);
   code4initUndo(&codeBase);
   return 0;
}
