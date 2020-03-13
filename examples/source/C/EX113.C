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

/*ex113.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main(  )
{
   CODE4 cb ;
   FILE4 primary, secondary ;
   int rc ;
   char buffer[15] ;

   code4init( &cb ) ;
   code4optStart( &cb ) ;
   cb.safety = 0 ;

   file4create( &primary, &cb, "PRI", 0 ) ;
   file4create( &secondary, &cb, "SEC", 0 ) ;

   file4write( &primary,  0, "PRIMARY FILE", 12 ) ;
   file4write( &secondary, 0, "SECONDARY FILE", 14 ) ;

   rc = file4replace( &primary, &secondary ) ;

   if( rc < 0 )
      printf( "An error occurred in file4replace.\n" ) ;
   cb.errOpen = 0 ;
   rc = file4open( &secondary, &cb, "SEC", 0 ) ;
   if( rc == 0 )
      printf( "This should never happen.\n") ;

   memset( buffer, 0, sizeof( buffer ) ) ;
   memset( buffer, ' ', sizeof( buffer ) - 1 ) ;
   file4read( &primary, 0, buffer, sizeof( buffer )-1 ) ;

   if( strcmp( buffer, "SECONDARY FILE" ) == 0 )
      printf( "This should always be true.\n" ) ;

   file4close( &primary ) ;
   code4initUndo( &cb ) ;
}
