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

//ex43.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  /* Borland Only */

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   cb.lockAttempts = 0 ;  /* Do not wait when locking. */
   cb.readLock = 1 ;

   int rc =  data.go( 3L ) ;
   if ( rc == r4locked )
   {
      cout << "\nRecord 3 was locked by another user." << endl ;

      cb.readLock = 0 ; // Turn automatic locking off.

      rc =  data.go( 3L ) ;
      if ( rc == r4locked )
      {
         cout << "This will never happen because" ;
         cout << "'Code4::readLock' is false." << endl ;
      }
   }

   data.close( ) ;
   cb.initUndo( ) ;
}
