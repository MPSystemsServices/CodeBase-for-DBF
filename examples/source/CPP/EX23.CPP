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

//ex23.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( void )
{
   Code4 cb ;
   Data4 data1( cb, "DATA1" ), data2( cb, "DATA2" ) ;

   data1.top( ) ;  data2.top( ) ;

   data1.lockAddFile( ) ;
   data2.lockAddAppend( ) ;

   long numRecords = data2.recCount( ) ;
   data2.lockAdd( numRecords ) ;
   data2.lockAdd( numRecords-1 ) ;
   data2.lockAdd( numRecords-2 ) ;

   if( cb.lock( ) == r4success )
      cout << "All locks were successfully performed" << endl ;

   cb.initUndo( ) ;
}
