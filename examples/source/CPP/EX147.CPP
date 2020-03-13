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

//ex147.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

/* Sort Item  NUM*/
typedef struct
{
   int  number;
   char otherStuff;
} NUM;

/* and it's comparison function */
int compNum(S4CMP_PARM p1,S4CMP_PARM p2, size_t len)
{
   if(((NUM *) p1) ->number > ((NUM *) p2) ->number)
      return 1;
   if(((NUM *) p1) ->number < ((NUM *) p2) ->number)
      return -1;
   return 0;
}

void main( )
{
   Code4 cb ;
   Sort4 sort( cb, sizeof( NUM ), 0 ) ;
   NUM st1, st2, st3 ;

   sort.assignCmp( compNum ) ;

   st1.number = 123 ;
   st2.number = 432 ;
   st3.number = 321 ;

   sort.put( &st1, NULL, 1L ) ;
   sort.put( &st2, NULL, 2L ) ;
   sort.put( &st3, NULL, 3L ) ;

   sort.getInit( ) ;
   while( sort.get( ) == 0 )
   {
      cout << "Sorted Item: " << ((NUM *)sort.result)->number ;
      cout << " RecNo : " << sort.resultRec << endl ;
   }
   sort.free( ) ;
   cb.initUndo( ) ;
}
