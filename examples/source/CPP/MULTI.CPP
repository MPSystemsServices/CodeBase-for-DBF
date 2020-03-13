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
*   MULTI.CPP       Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 11 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

Code4 cb ; Field4 fieldName ;
Data4 dataNames ;
Tag4 tagName ;

void addRecord( void ) ;
void findRecord( void ) ;
void modifyRecord( void ) ;
void listData( void ) ;

void main()
{
   cb.accessMode = OPEN4DENY_NONE ;
   cb.readOnly = 0 ;
   cb.readLock = 0 ;
   cb.lockAttempts = WAIT4EVER ;
   cb.lockEnforce = 1 ;

   dataNames.open( cb, "names" ) ;
   cb.exitTest(  ) ;

   fieldName.init( dataNames, "NAME" ) ;
   tagName.init( dataNames, "NAMENAME" ) ;

   dataNames.top( ) ;

   for(;;)
   {
      cb.errorCode = 0 ;

      cout << endl << "Record #: " << dataNames.recNo( )
           << "   Name: " << fieldName.str( ) << endl ;

      cout << "Enter Command ('a','f','l','m' or 'x') " << endl ;

      int command ;
      do
      {
         command =  getchar( ) ;
      } while (command == 10) ;
      switch( command )
      {
         case 'a':
            addRecord( ) ;
            break ;
         case 'f':
            findRecord( ) ;
            break ;
         case 'l':
            listData( ) ;
            break ;
         case 'm':
            modifyRecord( ) ;
            break ;
         case 'x':
            cb.closeAll( ) ;
            cb.exit( ) ;
      }
   }
   cb.initUndo( ) ;
}

void addRecord()
{
   char buf[100] ;
   printf( "Enter New Record\n" ) ;
   scanf( "%s", buf ) ;

   dataNames.appendStart( ) ;
   fieldName.assign( buf ) ;
   dataNames.append( ) ;
   dataNames.unlock( ) ;
}

void findRecord()
{
   char buf[100] ;
   printf( "Enter Name to Find\n" ) ;
   scanf( "%s", buf ) ;

   dataNames.select( tagName ) ;
   dataNames.seek( buf ) ;
}

void modifyRecord()
{
   int oldLockAttempts = cb.lockAttempts ;
   cb.lockAttempts = 1 ; // Only make one lock attempt

   int rc = dataNames.lock(dataNames.recNo( ) ) ;
   if(rc == r4locked)
      cout << "Record locked. Unable to Edit" << endl ;
   else
   {
      char buf[100] ;
      cout << "Enter Replacement Record" << endl ;
      scanf( "%s", buf ) ;

      fieldName.assign( buf ) ;
      dataNames.flush( ) ;
      dataNames.unlock( ) ;
   }
   cb.lockAttempts = oldLockAttempts ;
}

void listData()
{
   cb.optStart( ) ;
   dataNames.optimize( OPT4ALL ) ;
   dataNames.select( tagName ) ;

   for( dataNames.top( ); ! dataNames.eof( ); dataNames.skip( ) )
      cout << dataNames.recNo( ) << " " << fieldName.str( ) << endl;

   dataNames.optimize( OPT4OFF ) ;
   cb.optSuspend( ) ;
}
