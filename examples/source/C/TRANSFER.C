/***********************************************************************\
*                                                                       *
*   TRANSFER.C    Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 12 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; /*for all Borland Compilers*/
#endif

CODE4 codeBase ;
DATA4 *datafile ;

FIELD4 *acctNo, *balance ;
TAG4 *acctTag, *balTag ;

void OpenLogFile( void )         /*Opening or creating a log file is  */                           /*only required for STAND-ALONE mode.*/
{
   int rc ;

                        /*NULL is passed as the log file name */
                        /*so the default "C4.LOG" is used as  */
                        /*the log file name.                  */
   rc = code4logOpen( &codeBase, 0, "user1") ;
   if ( rc == r4noOpen )
   {
      codeBase.errorCode = 0 ;
      rc = code4logCreate( &codeBase, 0, "user1" ) ;
   }
}

void OpenDataFile( void )
{
   datafile = d4open( &codeBase, "bank.dbf" ) ;

   acctNo = d4field( datafile, "ACCT_NO" ) ;
   balance = d4field( datafile, "BALANCE" ) ;

   acctTag = d4tag( datafile, "ACCT_TAG" ) ;
   balTag = d4tag( datafile, "BAL_TAG" ) ;
}

int Credit( long toAcct, double amt )
{
   int rc ;
   double newBal ;

   d4tagSelect( datafile, acctTag ) ;
   rc = d4seekDouble( datafile, toAcct ) ;

   if (rc != r4success ) return rc;

   newBal = f4double( balance ) + amt ;
   d4lockFile( datafile ) ;
   f4assignDouble( balance, newBal ) ;
   return r4success;
}

int Debit( long fromAcct, double amt )
{
   return Credit( fromAcct, -amt ) ;
}

void Transfer( long fromAcct, long toAcct, double amt )
{
   int rc, rc1, rc2 ;

   code4tranStart( &codeBase ) ;

   rc1 = Debit( fromAcct, amt ) ;
   rc2 = Credit( toAcct, amt );

   if (rc1 == r4success && rc2 == r4success)
         rc = code4tranCommit( &codeBase ) ;
   else
      rc = code4tranRollback( &codeBase ) ;
}

void PrintRecords( void )
{
   int rc ;

   for( rc = d4top( datafile ); rc == r4success; rc = d4skip( datafile, 1L ))
   {
      printf("-----------------------------------\n") ;
      printf("Account Number: %ld\n", f4long( acctNo )) ;
      printf("Balance       : %f\n", f4double( balance )) ;
   }
   printf("=========================================\n") ;
}

void main( void )
{
   code4init( &codeBase ) ;

   codeBase.errOpen = 0 ;
   codeBase.safety = 0 ;
   codeBase.lockAttempts = 5 ;
   codeBase.lockEnforce = 1 ;

   #ifndef S4CLIENT
      OpenLogFile() ;
   #endif

   OpenDataFile() ;

   PrintRecords() ;

   /* The account number 56789 doesn't exist in the database,*/
   /* the transfer is aborted and database is not changed    */

   Transfer( 12345, 56789, 200.00 ) ;
   PrintRecords() ;

   /* Both accounts exist so the transfer is completed       */
   /* and the database is updated                            */

   Transfer( 12345, 55555, 150.50 ) ;
   PrintRecords() ;

   code4initUndo( &codeBase ) ;
}


