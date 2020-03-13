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

//ex187.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int searchAll( Data4 d, char *value )
{
   Tag4 origTag, tag ;
   origTag.initSelected( d ) ;  // Save the current tag
   long origRecNo = d.recNo( ) ;

   for( tag.initLast( d ) ; tag.isValid( ); tag.initPrev( ) )
   {
      d.select( tag ) ;
      if( d.seek( value ) == 0 )
      {
         d.select( origTag ) ;
         return d.recNo( ) ;
      }
   }
   d.select( origTag ) ;
   d.go( origRecNo ) ;
   return -1 ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   data.top( ) ;
   int rc = searchAll( data, "Abbot" ) ;
   cout << data.record( ) << endl ;
   cout << rc << " is the record number" << endl ;
   cb.initUndo( ) ;
}
