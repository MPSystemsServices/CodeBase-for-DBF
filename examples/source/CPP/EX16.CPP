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

//ex16.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

Code4 cb ;  // create a global Code4 object

void main( )
{
   cb.accessMode = OPEN4DENY_RW ;  // open files exclusively
   cb.autoOpen = 0 ;  // do not automatically open index files.
   Data4 data( cb, "INFO" ) ;
   data.top( ) ;

   Str4flex string( cb ) ;
   string.assign( data.record( ), data.recWidth( ) ) ;

   cout << "Copy of the current Record Buffer: " << endl ;
   cout << string.str( ) << endl ;

   cb.closeAll( ) ;
   cb.accessMode = OPEN4DENY_NONE ; // let other users use the file as well
   data.open( cb, "DATAFILE" ) ;

   cb.initUndo( ) ;
}
