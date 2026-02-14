/***********************************************************************\
*                                                                       *
*   CUSTLIST.C    Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */


#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif


int main( )
{
   CODE4    codeBase;
   DATA4    *dataFile = 0;
   FIELD4   *fName = 0
            ,*lName = 0
            ,*address = 0
            ,*age = 0
            ,*birthDate = 0
            ,*married = 0
            ,*amount = 0
            ,*comment = 0;

   int      rc,ageValue;
   double   amountValue;
   char     nameStr[25];
   char     addressStr[20];
   char     dateStr[9];
   char     marriedStr[2];
   const char     *commentStr;

   code4init(&codeBase);

   dataFile = d4open(&codeBase,"data1.dbf");
   error4exitTest(&codeBase);

   fName = d4field(dataFile,"F_NAME");
   lName = d4field(dataFile,"L_NAME");
   address = d4field(dataFile,"ADDRESS");
   age = d4field(dataFile,"AGE");
   birthDate = d4field(dataFile,"BIRTH_DATE");
   married = d4field(dataFile,"MARRIED");
   amount = d4field(dataFile,"AMOUNT");
   comment = d4field(dataFile,"COMMENT");

   for(rc = d4top(dataFile);rc == r4success;rc = d4skip(dataFile, 1L))
   {
      u4ncpy(nameStr,f4str(fName),sizeof(nameStr));
      u4ncpy(addressStr,f4str(address),sizeof(addressStr));
      ageValue = f4int(age);
      amountValue = f4double(amount);

      u4ncpy(dateStr,f4str(birthDate),sizeof(dateStr));
      u4ncpy(marriedStr,f4str(married),sizeof(marriedStr));
      commentStr = f4memoStr(comment);


      printf("-------------------------------\n");
      printf("Name     : %20s\n",nameStr);
      printf("Address  : %15s\n",addressStr);
      printf("Age : %3d   Married : %1s\n",ageValue,marriedStr);
      printf("Comment: %s\n",commentStr);
      printf("Purchased this year:$%5.2lf \n",amountValue);
      printf("\n");
   }
   d4close(dataFile);
   code4initUndo( &codeBase ) ;
   return 0;
}
