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

#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

#define VOTE_AGE 18.0

void main( )
{
   Code4 cb ;
   Data4 db( cb, "INFO" ) ;
   Expr4 expr( db, "AGE" ) ;

   long count = 0 ;
   for( int rc = db.top( ) ; rc != r4eof ; rc = db.skip( ) )
      if( double( expr ) >= VOTE_AGE )
         count ++ ;

   cout << "Possible voters: " << count << endl ;
   
   expr.free( ) ;
   cb.initUndo( ) ;
}
