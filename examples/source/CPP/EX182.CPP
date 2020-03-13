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

#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Str4large str1 ;

   str1.assign( "Roses are red,\nViolets are blue,\nSugar is " );
   str1.add( "sweet,\nAnd so are you!" ) ;
   cout << str1.ptr( ) << endl << endl  ;
   cout << "(This poem has " << str1.len( )
        << " characters)" << endl << endl ;

   str1.setLen( str1.at( Str4ptr("And")) ) ;
   str1.add( "And I love you!" ) ;
   cout << str1.ptr( ) << endl << endl ;
   cout << "(This poem has " << str1.len( ) << " characters)" << endl ;

}
