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

//ex136.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int addLotsOfRecords( Data4 d )
{
   // get the secondary index file
   Index4 index = d.index( "INFO2" ) ;
   if( index.isValid( ) )
      index.close( ) ;

   d.top( ) ;
   for( int i = 200 ; i ; i -- )
   {
      // make 200 copies of record 1
      d.appendStart() ;
      d.append( ) ; 
   } 

   // open the index file and update it
   index.open( d, d.alias( ) ) ;
   return index.reindex( ) ;
}

void main( )
{
   Code4 cb ;
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO" ) ;
   Index4 index;
   index.open( data, "INFO2" ) ;
   addLotsOfRecords( data ) ;
   cb.initUndo( ) ;
}
