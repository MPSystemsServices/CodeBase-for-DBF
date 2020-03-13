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

/*ex114.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int addToFile( CODE4 *cb, FILE4 *file, char *string )
{
   int oldLockAttempts ;
   long fileSize ;

   oldLockAttempts = cb->lockAttempts ;
   cb->lockAttempts = 1 ;

   fileSize = file4len( file ) ;
   if( file4lock( file, fileSize, LONG_MAX ) == r4locked )
   {
      printf( "Cannot add to file, another user is writing\n") ;
      cb->lockAttempts = oldLockAttempts ;
      return 1 ;
   }
   /* lock succeeded, I may add to the file without corrupting anyone else's
      writes */
   file4write( file, fileSize, string, strlen( string ) ) ;
   file4unlock( file, fileSize, LONG_MAX ) ;

   cb->lockAttempts = oldLockAttempts ;
   return 0 ;
}

void main()
{
   CODE4 cb ;
   FILE4 file ;
   char buffer[]= "Add this string to the file" ;

   code4init( &cb );
   file4open( &file, &cb, "TEXT.FIL", 0 ) ;

   addToFile( &cb, &file, buffer ) ;
   file4close( &file ) ;
   code4initUndo( &cb ) ;
}
