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

/*ex128.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

/* the Sort Item */
typedef struct myStructSt
{
   int number ;
   char otherStuff ;
} NUM ;

#ifdef __BORLANDC__
   #pragma argsused
#endif
int compNum( S4CMP_PARM p1, S4CMP_PARM p2, size_t len )
{
   if( ((NUM *) p1)->number > ((NUM *) p2)->number ) return 1 ;
   if( ((NUM *) p1)->number < ((NUM *) p2)->number ) return -1 ;
   return 0 ;
}

void main( void )
{
   CODE4 cb ;
   SORT4 sort ;
   NUM st1, st2, st3, *st ;
   long recNo ;
   char *notUsed ;

   code4init( &cb ) ;
   sort4init( &sort, &cb, sizeof( NUM ), 0 ) ;
   sort4assignCmp( &sort, compNum ) ;

   st1.number = 123 ;
   st2.number = 432 ;
   st3.number = 321 ;

   sort4put( &sort, 1L, &st1, NULL ) ;
   sort4put( &sort, 2L, &st2, NULL ) ;
   sort4put( &sort, 3L, &st3, NULL ) ;

   sort4getInit( &sort ) ;

   while( sort4get( &sort, &recNo, &st, &notUsed ) == 0 )
   {
      printf( "Sorted Item: %d\n", st->number ) ;
   }

   sort4free( &sort ) ;
   code4initUndo( &cb ) ;
}
