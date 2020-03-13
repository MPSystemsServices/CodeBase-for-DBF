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

/*ex103.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 file ;
   char readInfo[50] ;
   unsigned lenRead ;

   code4init( &cb ) ;
   cb.safety = 0 ;

   file4create( &file, &cb, "TEXT.FIL", 0 ) ;
   error4exitTest( &cb ) ;

   file4write( &file, 0, "Some File Information", 21 ) ;
   lenRead = file4read( &file, 10, readInfo, sizeof( readInfo) ) ;

   if( memcmp (readInfo, "Information" , lenRead ) == 0 )
      printf( "This is always true.\n" ) ;
   if( lenRead == 11 )
      printf( "This is always true, too.\n" ) ;
   file4close( &file ) ;
   code4initUndo( &cb ) ;
}
