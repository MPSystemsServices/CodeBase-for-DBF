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

//ex123.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void peek20( Code4 &cb, File4 &file )
{
   char buf[11] ;
   memset( buf, 0, sizeof( buf ) ) ; // ensure null termination for cout

   int pos = file.read( 0L, buf, sizeof( buf ) - 1 ) ;
   if( cb.errorCode < 0 ) return ;

   Str4ten buf2 ;
   buf2.setLen( 10 ) ;
   buf2.set( 0 ) ;

   if( pos )
      file.read( long (sizeof( buf ) - 1), buf2 ) ;
   cout << buf << buf2.ptr( ) << endl ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;
   peek20( cb, file ) ;
   cb.initUndo( ) ;
}
