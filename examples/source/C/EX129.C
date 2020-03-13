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

/*ex129.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct myStructSt
{
   char name[7] ;
} MY_STRUCT ;

#ifdef __BORLANDC__
   #pragma argsused
#endif
int myStructCmp( S4CMP_PARM p1, S4CMP_PARM p2, size_t len )
{
   return strcmp( ((MY_STRUCT *)p1)->name, ((MY_STRUCT *)p2)->name ) ;
}

void main( void )
{
   CODE4 cb ;
   SORT4 sort ;
   MY_STRUCT st1, st2, st3, st4, *st ;
   long recNo ;
   char *notUsed ;

   code4init( &cb ) ;
   sort4init( &sort, &cb, sizeof( MY_STRUCT ), 0 ) ;
   sort4assignCmp( &sort, myStructCmp ) ;

   strcpy( st1.name, "hello" ) ;
   strcpy( st2.name, "this" ) ;
   strcpy( st3.name, "apples" ) ;
   strcpy( st4.name, "face" ) ;

   sort4put( &sort, 1L, &st1, NULL ) ;
   sort4put( &sort, 2L, &st2, NULL ) ;
   sort4put( &sort, 3L, &st3, NULL ) ;
   sort4put( &sort, 4L, &st4, NULL ) ;

   sort4getInit( &sort ) ;
   while( sort4get( &sort, &recNo, &st, &notUsed ) == 0 )
   {
      printf( "Sorted Item: %s\n", st->name ) ;
   }
   sort4free( &sort ) ;
   code4initUndo( &cb ) ;
}
