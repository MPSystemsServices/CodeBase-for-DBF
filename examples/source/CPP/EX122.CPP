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

//ex122.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 test ;

   if( test.open( cb, "TEXT.FIL") < 0 )
      cb.exit( ) ;

   cb.lockAttempts = 2 ; // attempt a lock 2 times
   if( test.lock( 0, LONG_MAX ) != 0 )
   {
      cout << "Unable to lock the file" << endl ;
      test.close( ) ;
      cb.initUndo( ) ;
      cb.exit( ) ;
   }
   // double the file size for fun
   test.setLen( test.len( ) * 2 ) ;

   test.close( ) ; // File4::close automatically unlocks the file
   cb.initUndo( ) ;
}
