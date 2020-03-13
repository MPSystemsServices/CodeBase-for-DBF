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

//ex188.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;

   // open the datafile but not the index file
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO" ) ;

   Tag4info tags( cb ) ;
   tags.add( "retired", "age", "age>55", 0, r4descending) ;
   tags.add( "inf_name", "name", 0, r4uniqueContinue ) ;

   Index4 index ;
   cb.safety = 0 ; // overwrite an existing file
   index.create( data, "INFO2", tags.tags( ) ) ;
   if( cb.errorCode )
      cout << "An error occurred" << endl ;

   cb.initUndo( ) ; // Tag4info destructor called
}
