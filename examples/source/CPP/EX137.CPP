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

//ex137.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

static FIELD4INFO fieldInfo[ ] =
{
   { "FIELD_NAME", 'C', 10, 0 },
   { "VALUE", 'N', 7, 2 },
   { 0,0,0,0 }
} ;

TAG4INFO tagInfo[ ] =
{
   { "T_NAME", "FIELD_NAME", "FIELD_NAME > 'A'", 0,0 },
   { "NAME_TWO", "VALUE", "", e4unique, r4descending },
   { 0,0,0,0,0 }
} ;

void main( )
{
   Code4 cb ;
   Data4 data ;
   Index4 index ;

   cb.safety = 0 ;
   data.create( cb, "DB_NAME", fieldInfo ) ;
   if ( data )
      index.create( data, "name", tagInfo ) ;

   data.close( ) ;
   cb.initUndo( ) ;
}
