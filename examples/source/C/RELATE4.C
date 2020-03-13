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
*   RELATE4.C     Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 6 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4   codeBase;
DATA4   *student = NULL,*enrolment = NULL;
FIELD4  *id,*fName,*lName,*age,*cCode;
RELATE4 *master = NULL,*slave = NULL;
TAG4    *idTag,*nameTag;

void openDataFiles(void) ;
void setRelation(void) ;
void printRecord(void) ;

void openDataFiles(void)
{
   code4init(&codeBase);

   student = d4open(&codeBase,"student");
   enrolment = d4open(&codeBase,"enroll");

   id = d4field(student,"ID");
   fName = d4field(student,"F_NAME");
   lName = d4field(student,"L_NAME");
   age = d4field(student,"AGE");
   cCode = d4field(enrolment,"C_CODE_TAG");

   nameTag = d4tag(student,"STU_NAME");
   idTag = d4tag(enrolment,"ENR_ID");

   error4exitTest(&codeBase);
}

void setRelation(void)
{

   master = relate4init(student);
   slave = relate4createSlave(master, enrolment, "ID", idTag);
}

void seek(DATA4 *dataFile, TAG4 *tag, RELATE4 *relation, char *key)
{
   TAG4 *oldTag;

   oldTag = d4tagSelected(dataFile);
   d4tagSelect(dataFile,tag);

   d4seek(dataFile,key);
   relate4doAll(relation);

   d4tagSelect(dataFile,oldTag);
}

void printRecord(void)
{
   printf("%15s ",f4str(fName));
   printf("%15s ",f4str(lName));
   printf("%6s ",f4str(id));
   printf("%2s ",f4str(age));
   printf("%7s\n",f4str(cCode));
}

void main(void)
{
   openDataFiles();

   setRelation();

   seek(student
       ,nameTag
       ,master
       ,"Tyler           Harvey         ");

   printRecord();

   seek(student
       ,nameTag
       ,master,"Miller          Albert        ");

   printRecord();

   code4unlock(&codeBase);
   relate4free(master,0);

   code4close(&codeBase);
   code4initUndo(&codeBase);
}
