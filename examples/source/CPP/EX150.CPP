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

//ex150.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

class myClass
{
public:
   myClass( char *n )
   { name = (char *) u4alloc( strlen( n )+1 ) ; strcpy(name, n ) ; }
   ~myClass( ) { u4free( name ) ;}
   char *str( ) { return name; }
private:
   char *name ;
} ;

int myClassCmp( S4CMP_PARM p1, S4CMP_PARM p2, size_t len )
{
  return strcmp( ((myClass *)p1)->str( ), ((myClass *)p2)->str( ) ) ;
}

void main( )
{
   Code4 cb ;
   Sort4 sort;

   sort.init( cb, sizeof( myClass ) ) ;
   sort.assignCmp( myClassCmp ) ;

   myClass obj1( "hello" ) ;
   myClass obj2( "this" ) ;
   myClass obj3( "apples" ) ;
   myClass obj4( "face" ) ;

   sort.put( &obj1 ) ;
   sort.put( &obj2 ) ;
   sort.put( &obj3 ) ;
   sort.put( &obj4 ) ;

   sort.getInit( ) ;
   while( sort.get( ) == 0 )
   {
      cout << "Sorted Item: " << ((myClass *) sort.result)->str() << endl ;
   }
   cb.initUndo( ) ;
}
