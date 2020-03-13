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

//ex13.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

int retry(  )
{
   char rc ;
   cout << "Record locked by another user." << endl ;
   cout << "Retry? (Y or N)" ;
   cin >> rc ;
   if( rc == 'N' ) return 0 ;
   return 1 ;
}

int lockARecord( Data4 d, long rec )
{
   int rc ;
   while( ((rc = d.go( rec )) == r4locked) && (retry( ) == 1) ) ;

   if( rc == r4locked ) return 0 ;

   return 1 ;
}

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   lockARecord( data, 3L ) ;

   // other code

   cb.initUndo( ) ;
}
