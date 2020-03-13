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

/***********************************************************************\
*                                                                       *
*   STR3EG.CPP    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 3 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

void main( )
{
   Code4 cb ;
   Data4 data( cb, "info" ) ;
   Field4 age( data, "AGE" ), name( data, "NAME" ) ;

   // Make everyone one year older, except those people
   // who are 39 'and holding'
   data.lockFile( ) ;
   for( data.top( ); !data.eof( ); data.skip( ) )
   {
      if( 39 != int( age ))
      {
         age.assignLong( long( age ) + 1L ) ;
         cout << name.str( ) << " is now " << int( age ) << endl ;
      }
      else
         cout << name.str( ) << " is 39 and holding" << endl ;
   }
   data.close( ) ;
   cb.initUndo( ) ;
}
