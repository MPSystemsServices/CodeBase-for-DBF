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

/*ex112.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 file ;
   char before[6], after[6] ;

   code4init( &cb ) ;
   file4open( &file, &cb, "TEXT.FIL", 0 ) ;
   memset( before, 0, sizeof( before ) ) ;
   memset( after, 0, sizeof( after ) ) ;

   file4optimize( &file, 1, OPT4OTHER) ;

   /* read the first 5 bytes and buffer it.*/
   file4read( &file, 0, before, sizeof( before )-1 ) ;
   file4read( &file, 0, after, sizeof( after )-1 ) ; /* read from memory, not disk*/

   if( strcmp( before, after ) )
      printf( "This will always be true, since the read was from memory.\n") ;

   printf( "Press ENTER to re-read information.\n" ) ;
   getchar( ) ;

   file4refresh( &file ) ; /* next read will be from disk*/

   file4read( &file, 0, after, sizeof( after )-1 ) ;
   if( strcmp( before, after ) == 0 )
      printf( "No changes detected.\n" ) ;
   else
      printf( "Good thing it was read from disk... \nSomeone has changed it.\n") ;

   file4close( &file ) ;
   code4initUndo( &cb ) ;
}
