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
*   NOGROUP1.C    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4  codeBase;
DATA4  *dataFile = 0;
TAG4   *nameTag,*idTag,*ageTag;

TAG4INFO tagInfo[] =
{
   {"STU_NAME","L_NAME+F_NAME",0,0,0},
   {"STU_ID","ID",0,r4unique,0},
   {"STU_AGE","AGE",0,0,0},
   {0,0,0,0,0}
} ;

int main(void)
{
   code4init(&codeBase);

   #ifdef S4CLIENT
      code4connect(&codeBase,0,0,0,0,0);
   #endif

   if (strcmp(code4indexExtension(&codeBase),"NTX") == 0)
   {
      codeBase.autoOpen = 0;
      codeBase.safety = 0;
      codeBase.accessMode = OPEN4DENY_RW;

      dataFile = d4open(&codeBase,"STUDENT.DBF");

      i4create( dataFile, 0, tagInfo ) ;
      nameTag = d4tag( dataFile, "STU_NAME" ) ;
      idTag = d4tag( dataFile, "STU_ID" ) ;
      ageTag = d4tag( dataFile, "STU_AGE" ) ;

      code4close(&codeBase);
   }
   else
      fprintf(stderr,"This example is only for Clipper.\n");

   code4initUndo(&codeBase);

   return 1;
}
