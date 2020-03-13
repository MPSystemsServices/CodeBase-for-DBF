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

//ex57.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

long recsInFile( Code4 &cb, Str4ten &fileName )
{
   Data4 data( cb, fileName.ptr( ) ) ;
   if( cb.errorCode ) return -1L ; // an error occurred

   long count = data.recCount( ) ; // save the record count

   data.close( ) ;               // close the data file
   return count ;
}

void main( )
{
   Code4 cb ;
   Str4ten name( "INFO" ) ;
   long rc = recsInFile( cb, name ) ;
   cout << rc << " records in the file" << endl ;
   cb.initUndo( ) ;
}
