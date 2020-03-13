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

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void showExpr( EXPR4 *ex )
{
   switch( expr4type( ex ) )
   {
      case  r4date:
         printf( "type is r4date\n" ) ;
         break ;
      case  r4dateDoub:
         printf( "type is r4dateDoub\n" ) ;
         break ;
      case  r4log:
         printf( "type is r4log\n" ) ;
         break ;
      case  r4num:
         printf( "type is r4num\n" ) ;
         break ;
      case  r4numDoub:
         printf( "type is r4numDoub\n" ) ;
         break ;
      case  r4str:
         printf( "type is r4str\n" ) ;
         break ;
      case  r4memo:
         printf( "type is r4memo\n" ) ;
         break ;
   }
}

void main( )
{
   CODE4 cb ;
   DATA4 *db ;
   EXPR4 *ex ;

   code4init( &cb ) ;
   db = d4open( &cb, "info" ) ;
   d4top( db ) ;

   ex = expr4parse( db, "NAME" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   ex = expr4parse( db, "AGE" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   ex = expr4parse( db, "BIRTH_DATE" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
