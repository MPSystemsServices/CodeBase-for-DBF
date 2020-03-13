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

/* f4seq.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.hpp"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#include "d4data.hpp"

//File4seqRead &File4seqRead::operator>>( Str4 &s )
//{
//   unsigned num_read = read( s.ptr( ), s.len( ) ) ;
//   if( num_read < s.len( ) )
//      s.setLen( num_read ) ;
//   return *this ;
//}

File4seqWrite & File4seqWrite::operator<<( const long longVal )
{
   char t[16] ;
   char *pt;
   c4ltoa45( longVal, t, 15 ) ;
   t[15] = 0 ;
   for (pt = t;*pt && *pt == ' ';pt++)
      ;
   write( pt ) ;
   return *this;
}

