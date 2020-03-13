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

// BANK.C
// Creates new data file for TRANSFER.C program

#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4    codeBase ;
DATA4   *dataFile ;
FIELD4  *acctNo, *balance ;
TAG4    *acctTag, *balTag ;
FIELD4INFO  fieldInfo [] =
{
   {"ACCT_NO",r4num,5,0},
   {"BALANCE",r4num,8,2},
   {0,0,0,0},
};

TAG4INFO tagInfo [] =
{
   {"ACCT_TAG","ACCT_NO",0,0,0},
   {"BAL_TAG","BALANCE",0,0,0},
   {0,0,0,0,0},
};

void  OpenDataFile( void )
{
   dataFile = d4create( &codeBase,"BANK.DBF", fieldInfo, tagInfo ) ;

   acctNo = d4field( dataFile, "ACCT_NO" ) ;
   balance= d4field( dataFile, "BALANCE" ) ;

   acctTag= d4tag( dataFile, "ACCT_TAG" ) ;
   balTag = d4tag( dataFile, "BAL_TAG" ) ;
}

void PrintRecords( void )
{
	int rc ;

   printf("printrecords\n");
   for( rc = d4top(dataFile ); rc == r4success;
   rc = d4skip(dataFile, 1L ))
   {
      printf("-------------------------------\n") ;
      printf("Account Number: %ld\n", f4long( acctNo)) ;
      printf("Balance       : %f\n " , f4double(balance)) ;
   }
}

void AddNewRecord(long acct, double bal )
{
   d4appendStart(dataFile, 0 ) ;
   d4blank(dataFile ) ;

	d4lockFile(dataFile) ;
   f4assignLong( acctNo, acct ) ;
   f4assignDouble( balance, bal) ;

   d4append(dataFile ) ;
	d4unlock(dataFile) ;
}

int main( void )
{
   code4init( &codeBase ) ;
   codeBase.errOpen = 1;
   codeBase.safety = 0;

   OpenDataFile( ) ;


   AddNewRecord(12345L,600.00);

   AddNewRecord(55555L,300.00);

   PrintRecords( ) ;

   code4close( &codeBase ) ;
   code4initUndo( &codeBase ) ;
   return 0 ;
}

