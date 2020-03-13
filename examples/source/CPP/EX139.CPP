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

//ex139.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

static Mem4 memory ;
class myClass
{
public:
   char buf[20] ;
   void assign( char *str ) { strcpy( buf, str) ;}
   void * operator new( size_t ) { return memory.alloc( ) ;}
   void operator delete( void *mc) { memory.free( mc ) ;}
};

void main( )
{
   Code4 cb ;
   memory.create( cb, 2, sizeof( myClass ), 2, 0 ) ;

   // mc1 and mc2 use the first block allocated with Mem4::create
   myClass *mc1 = new myClass ;
   myClass *mc2 = new myClass ;
   myClass *mc3 = new myClass ;
   // mc3 causes two more units to be allocated

   mc1->assign( "I " ) ;
   mc2->assign( "WAS " ) ;
   mc3->assign( "HERE" ) ;

   cout << mc1->buf << mc2->buf << mc3->buf << endl ;

   delete mc1 ;
   delete mc2 ;
   delete mc3 ;

   // memory is still available for four myClass sized objects
   memory.release( ) ; // free memory allocated with Mem4::create

   cb.initUndo( ) ;
}
