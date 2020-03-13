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

//ex8.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

FIELD4INFO fields[] =
{
   { "NAME_FLD", 'C', 20, 0 },
   { "AGE_FLD", 'N', 3, 0 },
   { 0,0,0,0 }
} ;

void main( )
{
   Code4 cb ;
   cb.errOpen = 0 ;
   // no error message is displayed if NO_FILE does not exist

   Data4 data( cb, "NO_FILE" ) ;

   if( data.isValid() == 0 )
   {
      // Data file does not exist
      cb.safety = 0 ;
      data.create( cb, "NO_FILE", fields ) ;
      if( ! data.isValid( ) )
         cout << "Could not create NO_FILE" << endl ;
   }
   cb.initUndo( ) ;
}
