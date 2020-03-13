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

//ex115.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 file ;
   char readInfo[50] ;

   cb.safety = 0 ;
   file.create( cb, "TEXT.FIL", 0 ) ;
   if( ! file.isValid( ) )
   {
      cb.initUndo( ) ;
      cb.exit( ) ;
   }

   file.write( 0, "Some File Information", 21 ) ;
   unsigned lenRead = file.read( 10, readInfo, sizeof( readInfo) ) ;

   if( memcmp(readInfo, "Information" , lenRead ) == 0 )
      cout << "This is always true" << endl ;

   if( lenRead == 11 )
      cout << "This is always true, too" << endl ;

   file.close( ) ;
   cb.initUndo( ) ;
}
