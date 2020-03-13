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

//ex149.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Sort4 sort ;
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO") ;
   Field4 field( data, 1 ) ;
   cb.exitTest( ) ;

   sort.init( cb, field.len( ), sizeof(char) ) ;
   for( data.top( ) ; !data.eof( ) ; data.skip( ) )
   {
      sort.put( field.ptr( ), data.record( ), data.recNo( ) ) ;
   }
   sort.getInit( ) ;
   while( sort.get( ) == 0 )
   {
      Str4len outField( sort.result, field.len( ) ) ;

      cout << *(char *) sort.resultOther << " "
           << outField.str( ) << endl ;
   }
   sort.free( ) ;
   cb.initUndo( ) ;
}
