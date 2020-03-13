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
   DATA4 *from, *to ;
   long iRec ;

   code4init( &cb ) ;
   from = d4open ( &cb, "DATABASE" ) ;
   code4optStart( &cb ) ;

   to = d4open( &cb, "DATA2" ) ;
     /* Database 'DATA2' has an identical database
        structure as database 'DATABASE'. */

   if ( d4recWidth( from ) != d4recWidth( to ) )
   {
      error4( &cb, e4result, 0 ) ;
      code4exit( &cb ) ;
   }

   error4exitTest( &cb ) ;
   for ( iRec = 1L; iRec <= d4recCount( from ); iRec++ )
   {
       /* Read the database record. */
      d4go( from, iRec ) ;
        /* Copy the database buffer. */
      d4appendStart( to, 0 ) ;
      memcpy( d4record( to ), d4record( from ), d4recWidth( to ) ) ;
        /* Append the database record. */
      d4append( to ) ;
   }
   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
