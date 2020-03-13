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
