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

//ex103.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

char *createBufCopy( Field4 f )
{
   char *buf = (char *)u4alloc( f.len( ) + 1 ) ;
   memcpy( buf, f.ptr( ), f.len( ) );
   buf[f.len()] = 0;
   return buf ;
}

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Field4 field( data, "NAME" ) ;
   char *buffer ;

   data.top( ) ;
   buffer = createBufCopy( field ) ;
   cout << buffer << " is a copy of the field " << endl ;
   u4free( buffer ) ;
   cb.initUndo( ) ;
}
