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

//ex126.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main(  )
{
   Code4 cb ;
   cb.optStart( ) ;
   cb.safety = 0 ;

   File4 primary, secondary ;
   primary.create( cb, "PRI" ) ;
   secondary.create( cb, "SEC" ) ;

   primary.write(   0, "PRIMARY FILE", 12 ) ;
   secondary.write( 0, "SECONDARY FILE", 14 ) ;

   int rc = primary.replace( secondary ) ;

   if( rc < 0 )
      cout << "An error occurred in File4::replace" << endl ;

   if( secondary.isValid( ) )
      cout << "This should never happen" << endl ;

   Str4large buffer ;
   buffer.setLen( 14 ) ;
   primary.read( 0, buffer ) ;

   Str4ptr str( "SECONDARY FILE" ) ;
   if( buffer == str )
      cout << "This should always be true." << endl ;

   primary.close( ) ;
   cb.initUndo( ) ;
}
