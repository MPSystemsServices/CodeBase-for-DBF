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
*   TRANSFER.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 12 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

Code4 codeBase ;
Data4 dataFile ;
Field4 acctNo, balance ;
Tag4 acctTag, balTag ;

#ifndef S4CLIENT
void OpenLogFile( void )
{
   int rc ;

   rc = codeBase.logOpen( 0, "user1" );
   if ( rc == r4noOpen )
   {
      codeBase.errorCode = 0 ;
      codeBase.logCreate( 0, "user1" ) ;
   }
}
#endif

void OpenDataFile( void )
{
   dataFile.open( codeBase, "bank.dbf" ) ;

   acctNo.init( dataFile, "ACCT_NO" ) ;
   balance.init( dataFile, "BALANCE" ) ;

   acctTag.init( dataFile, "ACCT_TAG" ) ;
   balTag.init( dataFile, "BAL_TAG" ) ;
}

int Credit( long toAcct, double amt )
{
   dataFile.select( acctTag ) ;
   int rc = dataFile.seek( toAcct ) ;

   if (rc != r4success ) return rc;
   double newBal = double( balance ) + amt ;
   balance.assignDouble( newBal ) ;
   return r4success;
}

int Debit( long fromAcct, double amt )
{
   return Credit( fromAcct, -amt ) ;
}

void Transfer( long fromAcct, long toAcct, double amt )
{
   dataFile.lockAll( ) ;
   codeBase.tranStart() ;

   int rc1 = Debit( fromAcct, amt ) ;
   int rc2 = Credit( toAcct, amt );

   if (rc1 == r4success && rc2 == r4success)
         codeBase.tranCommit() ;
   else
         codeBase.tranRollback() ;
   dataFile.unlock( ) ;
}

void PrintRecords( void )
{
   for( int rc = dataFile.top(); rc == r4success; rc = dataFile.skip())
   {
      cout <<"-----------------------------------"<< endl
           <<"Account Number: "<< (long) acctNo << endl
           <<"Balance       : "<< (double) balance << endl ;
   }
   cout <<"========================================="<< endl ;
}

void main( void )
{
   codeBase.errOpen = 0 ;
   codeBase.safety = 0 ;
   codeBase.lockAttempts = 5 ;
   codeBase.lockEnforce = 1 ;

   #ifndef S4CLIENT
      OpenLogFile() ;
   #endif

   OpenDataFile() ;

   PrintRecords() ;

   // The account number 56789 doesn't exist in the database,
   // the transfer is aborted and database is not affected

   Transfer( 12345, 56789, 200.00 ) ;
   PrintRecords() ;

   // Both accounts exist so the transfer is completed
   // and the database is updated

   Transfer( 12345, 55555, 150.50 ) ;
   PrintRecords() ;

   codeBase.initUndo( ) ;
}
