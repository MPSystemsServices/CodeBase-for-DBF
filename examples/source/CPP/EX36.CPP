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

//ex36.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  /* Borland Only */

/* Check the validity of an index file. */
void main( int argc, char *argv[ 2 ] )
{
   if ( argc > 2 )
   {
      Code4 cb ;
      cb.autoOpen = 0 ;  // open index file manually.
      cb.errOff = 1;

      Data4 checkData( cb, argv[1] ) ;

      // Demonstration of Index4::open instead of Index4::Index4
      Index4 testIndex ;
      testIndex.open( checkData, argv[2] ) ;

      cb.exitTest( ) ;
      cb.optStart( ) ;

      cb.errorCode = 0;
      if ( checkData.checkIndex( ) == 0 )
         cout << endl << "Index is OK !!" << endl;
      else
         cout << endl << "Problem with Index" << endl ;
      cb.initUndo( ) ;
   }
   else
      cout << endl << "PROGRAM  DataFile  IndexFile" << endl ;
}
