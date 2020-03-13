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
*   RELATE1.C     Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 6 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4   codeBase;
DATA4   *student = NULL,*enrollment = NULL;
RELATE4 *master = NULL,*slave = NULL;
TAG4    *idTag,*nameTag;

void openDataFiles(void) ;
void setRelation(void) ;
void printRecord(void) ;

void openDataFiles(void)
{
    code4init(&codeBase);

    student = d4open(&codeBase,"student");
    enrollment = d4open(&codeBase,"enroll");

    nameTag = d4tag(student,"STU_NAME");
    idTag = d4tag(enrollment,"ENR_ID");

    error4exitTest(&codeBase);
}

void setRelation(void)
{
    master = relate4init(student);
    if(master == NULL) exit(1);

    slave = relate4createSlave(master,enrollment,"ID",idTag);

    relate4type(slave,relate4scan);

    relate4top(master);
}

void printRecord(void)
{
   RELATE4 *relation;
   DATA4 *data;
   short j;

   for(relation = master;relation != NULL;relate4next(&relation))
   {
      data = relation->data ;

      for(j = 1;j <= d4numFields(data);j++)
         printf("%s ",f4memoStr(d4fieldJ(data,j)));
   }
   printf("\n");
}

void listRecords(void)
{
   int rc;

   for(rc = relate4top(master);rc != r4eof;rc = relate4skip(master,1L))
      printRecord();
   printf("\n");

   code4unlock(&codeBase);
}

void main(void)
{
   openDataFiles();

   setRelation();

   listRecords();

   relate4free(master,0);

   code4close(&codeBase);
   code4initUndo(&codeBase);
}
