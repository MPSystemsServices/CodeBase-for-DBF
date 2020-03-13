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
*   STR2EG.CPP    Copyright (C) 1999 Sequiter Software Inc.             *
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
   cb.readOnly = 1 ;
   Data4 data( cb, "info" ) ;
   Field4 name( data, "NAME" ) ;
   Str4large max, min ;

   data.top( ) ;
   max.assign( name ) ;
   min.assign( name ) ;

   for( data.skip( ); !data.eof( ); data.skip( ) )
   {
      if( min > name )
         min.assign( name ) ;
      if( max < name )
         max.assign( name ) ;
   }

   if( min == max )
      cout << "All records have the same name: " << min.ptr( ) << endl ;
   else
   {
      min.trim( ) ;
      max.trim( ) ;
      cout << min.ptr( )
          << " would come first in an alphabetic listing" << endl ;
      cout << max.ptr( )
          << " would come last in an alpahbetic listing" << endl ;
   }

   data.close( ) ;
   cb.initUndo( ) ;
}
