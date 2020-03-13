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

//ex143.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void displayRelationTree( Relate4 relate )
{
   int pos = 0 ;
   Relate4iterator tree ;
   tree = relate ;

   for(; tree.isValid( ); )
   {
      for( int i = pos; i > 0; i-- )
         cout << "   " ;
      cout << tree.data( ).fileName( ) << endl ;

      pos +=  tree.nextPosition( ) ;
   }
}

void main( )
{
   Code4 cb ;
   Data4 master( cb, "M1" ) ;
   Data4 sl1( cb, "SL1" ) ;
   Data4 sl2( cb, "SL2" ) ;
   Data4 sl3( cb, "SL3" ) ;
   Data4 sl4( cb, "SL4" ) ;

   Relate4set MasterRelation( master ) ;

   // create the tree
   Relate4( MasterRelation, sl1, "TOSL1", Tag4( sl1, "FRM" ) ) ;
   Relate4 relate2( MasterRelation, sl2, "TOSL2", Tag4( sl2, "FRM") ) ;
   Relate4( relate2, sl3, "TOSL3", Tag4( sl3, "FRMSL2" ) ) ;
   Relate4( MasterRelation, sl4, "TOSL4", Tag4( sl4, "FRM" ) ) ;
   cb.exitTest( ) ;

   MasterRelation.top( ) ;
   displayRelationTree( MasterRelation ) ;

   MasterRelation.free( 1 ) ;
   cb.initUndo( ) ;
}
