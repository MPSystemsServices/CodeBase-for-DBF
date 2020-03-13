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

//ex46.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  /* Borland Only */

void main()
{
   Code4  cb ;
   Data4 data ;

   data.open( cb, "INFO" ) ;

   cb.lockAttempts = 4 ; // Try four times

   int rc = data.lock( 5 ) ;
   if( rc == r4success )
      cout << "Record 5 is now locked." << endl ;
   else if( rc == r4locked )
      cout << "Record 5 is locked by another user" << endl ;

   cb.lockAttempts = WAIT4EVER ; // Try forever
   rc = data.lock( 5 ) ;

   if ( rc == r4success )
      cout << "This will always happen." << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
