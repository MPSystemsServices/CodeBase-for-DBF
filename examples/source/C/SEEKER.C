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
*   SEEKER.C      Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4    codeBase;
DATA4    *dataFile = 0;
FIELD4   *fName,*lName,*address,*age,*birthDate,*married,*amount,*comment;
TAG4     *nameTag,*ageTag,*amountTag,*addressTag,*birthdateTag;

void OpenDataFile(void)
{
   dataFile = d4open(&codeBase,"data1.dbf");

   fName = d4field(dataFile,"F_NAME");
   lName = d4field(dataFile,"L_NAME");
   address = d4field(dataFile,"ADDRESS");
   age = d4field(dataFile,"AGE");
   birthDate = d4field(dataFile,"BIRTH_DATE");
   married = d4field(dataFile,"MARRIED");
   amount = d4field(dataFile,"AMOUNT");
   comment = d4field(dataFile,"COMMENT");

   nameTag = d4tag(dataFile,"NAME_TAG");
   addressTag = d4tag(dataFile,"ADDR_TAG");
   ageTag = d4tag(dataFile,"AGE_TAG");
   birthdateTag = d4tag(dataFile,"DATE_TAG");
   amountTag = d4tag(dataFile,"AMNT_TAG");
}

void seekStatus(int rc,char *status)
{
   switch(rc)
   {
      case r4success:
         strcpy(status,"r4success");
         break;
      case r4eof:
         strcpy(status,"r4eof");
         break;
      case r4after:
         strcpy(status,"r4after");
         break;
      default:
         strcpy(status,"other");
         break;
   }
}

void printRecord(int rc)
{
   int      ageValue;
   double   amountValue;
   char     fNameStr[15],lNameStr[15];
   char     addressStr[20];
   char     dateStr[9];
   char     marriedStr[2];
   const char     *commentStr;
   char     status[15];

   f4ncpy(fName,fNameStr,sizeof(fNameStr));
   f4ncpy(lName,lNameStr,sizeof(lNameStr));
   f4ncpy(address,addressStr,sizeof(addressStr));
   ageValue = f4int(age);

   amountValue = f4double(amount);

   f4ncpy(birthDate,dateStr,sizeof(dateStr));

   f4ncpy(married,marriedStr,sizeof(marriedStr));

   commentStr = f4memoStr(comment);

   seekStatus(rc,status);

   printf("Seek status : %s\n",status);
   printf("---------------------------------\n");
   printf("Name     : %10s %10s\n",fNameStr,lNameStr);
   printf("Address  : %15s\n",addressStr);
   printf("Age:%3d BirthDate:%8s Married : %1s\n",ageValue,dateStr,marriedStr);
   printf("Comment: %s\n",commentStr);
   printf("Amount purchased : $%5.2lf \n",amountValue);

   printf("\n");
}

int main(void)
{
   int rc;

   code4init(&codeBase);

   OpenDataFile();

   d4tagSelect(dataFile,addressTag);

   rc = d4seek(dataFile,"123 - 45 Ave   ");
   printRecord(rc);

   rc = d4seek(dataFile,"12");
   printRecord(rc);

   rc = d4seek(dataFile,"12             ");
   printRecord(rc);

   d4tagSelect(dataFile,birthdateTag);

   rc = d4seek(dataFile,"19500101");
   printRecord(rc);

   printf("TAG AMOUNT, 2seeks\n");

   d4tagSelect(dataFile,amountTag);

   rc = d4seekDouble(dataFile,47.23);
   printRecord(rc);

   rc = d4seek(dataFile," 47.23");
   printRecord(rc);

   /*The following code finds all the occurances of John Albridge in the tag*/

   d4tagSelect(dataFile,nameTag);

   rc = d4seek(dataFile,"Albridge  John      ");
   for( rc; rc==r4success; rc=d4seekNext(dataFile,"Albridge  John      "))
   {
      printf("in for loop\n");
      printRecord(rc);
   }

   code4close(&codeBase);
   code4initUndo(&codeBase);

   return 0;
}
