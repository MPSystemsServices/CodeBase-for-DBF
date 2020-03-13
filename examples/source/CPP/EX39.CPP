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

//ex39.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // Borland compilers only

void main( )
{
   Code4 cb ;
   Data4 data ;

   cb.accessMode = OPEN4DENY_RW ; // open file exclusively to speed pack

   data.open( cb, "INFO" ) ;
   cb.exitTest( ) ;
   cb.optStart( ) ;

   for( data.top( ); ! data.eof( ); data.skip( 2 ) )
       data.deleteRec( ) ;  // Mark the record for deletion

   data.pack( ) ; // Physically remove the deleted records from the disk

   data.close( ) ;
   cb.initUndo( ) ;
}
