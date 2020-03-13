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

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4  cb ;
   DATA4 *data ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;

   cb.lockAttempts = 4 ; /* Try lock four times */

   rc = d4lock( data, 4 ) ;
   if( rc == r4success )
      printf( "Record 4 is now locked.\n") ;
   else if( rc == r4locked )
      printf( "Record 4 is locked by another user.\n" ) ;

   cb.lockAttempts = WAIT4EVER ; /* Try forever, d4lock will not return r4locked */
   rc = d4lock( data, 4 ) ;

   if ( rc == r4locked )
      printf( "This should never Happen\n") ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
 }
