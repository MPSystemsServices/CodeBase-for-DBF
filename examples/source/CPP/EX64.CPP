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

//ex64.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int SeekSeries( Data4 d, const char *s )
{
   int rc ;
   rc = d.seekNext( s ) ;

   if( rc == r4noTag || rc == r4entry || rc == r4locked || rc < 0 )
      return rc ;

   if( rc == r4after || rc == r4eof )
      rc = d.seek( s ) ;

   return rc ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "PEOPLE" ) ;
   Tag4 nametag( data, "PPL_NAME" ) ;

   data.select( nametag ) ;
   int rc = data.seek( "mickey" ) ;
   for (rc ; rc == r4success ;rc = SeekSeries( data, "mickey" ))
      cout << "found search string " << endl ;
   cb.initUndo( ) ;
}
