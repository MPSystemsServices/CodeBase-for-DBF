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

//ex138.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   cb.autoOpen = 0 ; // don't automatically open index file
   Data4 data( cb, "INFO" ) ;

   Index4 index ;
   index.open( data, "INFO2" ) ; // open a secondary index file

   cb.lockAttempts = WAIT4EVER ; // wait until the lock succeeds
   data.lockAll( ) ;
   if( index.reindex( ) == 0 )
      cout << "Reindexed successfully" << endl ;

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
