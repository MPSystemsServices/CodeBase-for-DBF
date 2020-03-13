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

/*ex121.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

static MEM4 *memory ;

typedef struct myStructSt
{
   char buffer[9] ;
} MY_STRUCT ;

void main( void )
{
   CODE4 cb ;
   MY_STRUCT *ms1, *ms2, *ms3 ;

   code4init( &cb ) ;
   memory = mem4create( &cb, 2, sizeof( MY_STRUCT ), 2, 0 ) ;

   /* ms1 and ms2 use the first block allocated with mem4create. */
   ms1 = mem4alloc( memory ) ;
   ms2 = mem4alloc( memory );
   ms3 = mem4alloc( memory );
   /* The construction of ms3 causes two more units to be allocated. */

   strcpy( ms1->buffer, "I " ) ;
   strcpy( ms2->buffer, "WAS " ) ;
   strcpy( ms3->buffer, "HERE" ) ;
   printf( "%s%s%s\n", ms1->buffer, ms2->buffer, ms3->buffer ) ;

   mem4free( memory, ms1 ) ;
   mem4free( memory, ms2 ) ;
   mem4free( memory, ms3 ) ;

   /* memory still contains allocated memory enough for four MY_STRUCT
      sized structures */

    mem4release( memory ) ; /* free memory allocated with mem4create*/

    code4initUndo( &cb ) ;
}
