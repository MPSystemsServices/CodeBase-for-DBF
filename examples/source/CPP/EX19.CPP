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

//ex19.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void openAFile( Code4 &cb )
{
   // 'd' falls out of scope.  Data file is still open
   Data4 d( cb, "INFO" ) ;
}

void main( )
{
   Code4 cb ;
   openAFile( cb ) ;

   Data4 d = cb.data( "INFO" ) ; // obtain a new Data4 object

   if( d.isValid( ) )
   {
      cout << "INFO has " << d.recCount( ) << " records." << endl ;

      d.top( ) ;
      cb.data( "INFO").close( ) ; // an alternate way to close the file
   }
   cb.initUndo( ) ;
}
