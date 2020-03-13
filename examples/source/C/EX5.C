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

void main( )
{
   CODE4 code ;
   DATA4 *data ;
   EXPR4 *expression ;
   char badExpr[] = "NAME = 5" ;

   code4init( &code ) ;
   data = d4open( &code, "INFO" );
   expression = expr4parse( data, badExpr ) ;

   printf( "\nAn error message just displayed\n") ;

   code.errorCode = code.errExpr = 0 ;
   expression= expr4parse( data, badExpr ) ;
   printf("No error message displayed.\n") ;

   if ( expression )
      expr4free( expression ) ;

   code4initUndo( &code ) ;
}
