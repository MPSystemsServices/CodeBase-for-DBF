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
*   NEWLIST2.C    Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4    codeBase;
DATA4    *dataFile;
FIELD4   *fName,*lName,*address,*age,*birthDate
         ,*married,*amount,*comment;
TAG4     *nameTag,*ageTag,*amountTag;

FIELD4INFO  fieldInfo [] =
{
   {"F_NAME",r4str,10,0},
   {"L_NAME",r4str,10,0},
   {"ADDRESS",r4str,15,0},
   {"AGE",r4num,2,0},
   {"BIRTH_DATE",r4date,8,0},
   {"MARRIED",r4log,1,0},
   {"AMOUNT",r4num,7,2},
   {"COMMENT",r4memo,10,0},
   {0,0,0,0},
};

TAG4INFO  tagInfo[] =
{
   {"NAME_TAG","F_NAME + L_NAME",".NOT. DELETED()",0,0},
   {"ADDR_TAG","ADDRESS",0,0,0},
   {"AGE_TAG","AGE","AGE >= 18",0,0},
   {"DATE_TAG","BIRTH_DATE",0,0,0},
   {"AMNT_TAG","AMOUNT",0,0,0},
   {0,0,0,0,0},
};

void  CreateDataFile(void)
{

   dataFile = d4create(&codeBase,"data1.dbf"
                            ,fieldInfo,tagInfo);

   fName = d4field(dataFile,"F_NAME");
   lName = d4field(dataFile,"L_NAME");
   address = d4field(dataFile,"ADDRESS");
   age = d4field(dataFile,"AGE");
   birthDate = d4field(dataFile,"BIRTH_DATE");
   married = d4field(dataFile,"MARRIED");
   amount = d4field(dataFile,"AMOUNT");
   comment = d4field(dataFile,"COMMENT");

   nameTag = d4tag(dataFile,"NAME_TAG");
   ageTag = d4tag(dataFile,"AGE_TAG");
   amountTag = d4tag(dataFile,"AMNT_TAG");
}

void PrintRecords(void)
{
   int      rc,ageValue;
   double   amountValue;
   char     fNameStr[15],lNameStr[15];
   char     addressStr[20];
   char     dateStr[9];
   char     marriedStr[2];
   const char     *commentStr;

   for(rc = d4top(dataFile);rc == r4success
                    ;rc =   d4skip(dataFile, 1L))
   {
      f4ncpy(fName,fNameStr,sizeof(fNameStr));
      f4ncpy(lName,lNameStr
                           ,sizeof(lNameStr));
      f4ncpy(address,addressStr
                          ,sizeof(addressStr));
      ageValue = f4int(age);
      amountValue = f4double(amount);
      f4ncpy(birthDate,dateStr
                            ,sizeof(dateStr));
      f4ncpy(married,marriedStr,
                         sizeof(marriedStr));
      commentStr = f4memoStr(comment);

      printf("-------------------------------\n");

      printf("Name     : %10s %10s\n",fNameStr
                                     ,lNameStr);
      printf("Address  : %15s\n",addressStr);
      printf("Age : %3d   Married : %1s\n"
                          ,ageValue,marriedStr);
      printf("Comment: %s\n",commentStr);
      printf("Amount purchased this year: "
             " $%5.2lf\n\n", amountValue);
   }
}

void AddNewRecord(char *fNameStr
                  ,char *lNameStr
                  ,char *addressStr
                  ,int ageValue
                  ,int marriedValue
                  ,double amountValue
                   ,char *commentStr)
{
   d4appendStart(dataFile,0);

   f4assign(fName,fNameStr);
   f4assign(lName,lNameStr);
   f4assign(address,addressStr);
   f4assignInt(age,ageValue);
   if(marriedValue)
      f4assign(married,"T");
   else
      f4assign(married,"F");
   f4assignDouble(amount,amountValue);
   f4memoAssign(comment,commentStr);

   d4append(dataFile);
}


int main()
{
   code4init(&codeBase);
   codeBase.safety = 0;

   CreateDataFile();

   AddNewRecord("Sarah","Webber","132-43 St.",32,1
                          ,147.99,"New Customer");
   AddNewRecord("John","Albridge"
                    ,"1232-76 Ave.",12,0,98.99,"");

   PrintRecords();

   d4tagSelect(dataFile,nameTag);
   PrintRecords();

   d4tagSelect(dataFile,ageTag);
   PrintRecords();

   d4tagSelect(dataFile,amountTag);
   PrintRecords();

   code4close(&codeBase);
   code4initUndo( &codeBase ) ;     /* free up memory */
   return 0;
}
