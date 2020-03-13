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

/* ex126.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void displayRelationTree( RELATE4 *relate )
{
   DATA4 *data ;
   int i, pos = 0 ;

   for(; relate; )
   {
      printf( "\n" ) ;
      for(i = pos; i > 0; i-- )
         printf( "   " ) ;
      data = relate4data( relate );
      printf( "%s\n", d4fileName( data ) ) ;

      pos +=  relate4next( &relate ) ;
   }
}

void main( void )
{
   CODE4 cb ;
   DATA4 *master ;
   DATA4 *sl1 ;
   DATA4 *sl2 ;
   DATA4 *sl3 ;
   DATA4 *sl4 ;

   RELATE4 *MasterRelation ;
   RELATE4 *relate1, *relate2, *relate3, *relate4 ;

   code4init( &cb ) ;
   master = d4open( &cb, "M1" ) ;
   sl1 = d4open( &cb, "SL1" ) ;
   sl2 = d4open( &cb, "SL2" ) ;
   sl3 = d4open( &cb, "SL3" ) ;
   sl4 = d4open( &cb, "SL4" ) ;

   /* create the tree*/
   MasterRelation = relate4init( master ) ;
   relate1 = relate4createSlave( MasterRelation, sl1, "TOSL1", d4tag( sl1, "FRM" ) ) ;
   relate2 = relate4createSlave( MasterRelation, sl2, "TOSL2", d4tag( sl2, "FRM") ) ;
   relate3 = relate4createSlave( relate2, sl3, "TOSL3", d4tag( sl3, "FRMSL2" ) ) ;
   relate4 = relate4createSlave( MasterRelation, sl4, "TOSL4", d4tag( sl4, "FRM" ) ) ;

   error4exitTest( &cb ) ;
   relate4top( MasterRelation ) ;
   displayRelationTree( MasterRelation ) ;

   relate4free( MasterRelation, 1 ) ;
   code4initUndo( &cb ) ;
}
