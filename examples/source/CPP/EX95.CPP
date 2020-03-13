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

//ex95.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 db( settings, "DATA" ) ;

   db.top( ) ;
   Str4large name ;
   Expr4 fullName( db, "TRIM( LNAME )+', '+FNAME" ) ;

   name.assign( fullName.vary( ), fullName.len( ) ) ;
   // copy the contents of the internal buffer.  Expr4::vary does not
   // guarantee a null termination, so Expr4::len is necessary.
   // For illustration purposes only:
   // Avoid using the Expr4 class when a Str4 or Field4 class will suffice

  name.trim( ) ;
  cout << name.ptr( ) << " is the first person in the data file" << endl ;

  settings.initUndo( ) ;
}
