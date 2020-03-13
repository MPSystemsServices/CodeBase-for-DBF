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

//ex5.cpp
#include "d4all.hpp"
extern unsigned _stklen = 15000; // for all Borland complilers

void main( )
{
   Code4 code ;
   Data4 data( code, "INFO" ) ;

   char badExpr[] = "NAME = 5" ;

   Expr4 expression( data, badExpr ) ;

   cout << endl << "An error message just displayed" << endl ;

   code.errorCode = code.errExpr = 0 ;

   expression.parse( data, badExpr ) ;
   cout << "No error message displayed." << endl ;

   if (expression.isValid())
      expression.free( ) ;

   code.initUndo( ) ;
}
