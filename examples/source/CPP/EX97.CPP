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

//ex97.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void showExpr( Expr4 &ex )
{
   cout << endl << "Value: " ;

   switch( ex.type( ) )
   {
      case  r4date:
         cout << Str4len( ex.vary( ), ex.len( ) ).str( ) << endl ;
         cout << "r4date" << endl ;
         break ;
      case  r4dateDoub:
         cout << *(double *) ex.vary( ) << endl ;
         cout << "r4dateDoub" << endl ;
         break ;
      case  r4log:
         cout << *(int*) ex.vary( ) << endl ;
         cout << "r4log" << endl ;
         break ;
      case  r4num:
         cout << Str4len( ex.vary( ), ex.len( ) ).str( ) << endl ;
         cout << "r4num" << endl ;
         break ;
      case  r4numDoub:
         cout << *(double*)ex.vary( ) << endl ;
         cout << "r4numDoub" << endl ;
         break ;
      case  r4str:
         cout << Str4len( ex.vary( ), ex.len( )).str( ) << endl ;
         cout << "r4str" << endl ;
         break ;
      case  r4memo:
         cout << Str4len( ex.vary( ), ex.len( )).str( ) << endl ;
         cout << "r4memo" << endl ;
         break ;
   }
}

void main( )
{
   Code4 cb ;
   Data4 db( cb, "info" ) ;
   db.top( ) ;

   Expr4 ex( db, "NAME" ) ;
   showExpr( ex ) ;
   ex.free( ) ;

   ex.parse( db, "AGE" ) ;
   showExpr( ex ) ;
   ex.free( ) ;

   ex.parse( db, "AGE+1" ) ;
   showExpr( ex ) ;
   ex.free( ) ;

   ex.parse( db, "BIRTH_DATE" ) ;
   showExpr( ex ) ;
   ex.free( ) ;

   ex.parse( db, "BIRTH_DATE+1" ) ;
   showExpr( ex ) ;
   ex.free( ) ;

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
