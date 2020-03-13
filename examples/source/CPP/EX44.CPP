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

//ex44.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 data( settings, "INFO" ) ;

   // Since Code4::autoOpen is by default TRUE, the INFO index file
   // should have been opened.

   Index4 index ;

   index = data.index( "INFO" ) ;

   if( index.isValid( ) )
      cout << "INDEX: INFO has been opened" << endl ;

   index = data.index( "JUNK" ) ;

   if( !index.isValid( ) )
      cout << "INDEX: JUNK has not been opened" << endl ;

   data.close( ) ;
   settings.initUndo( ) ;
}
