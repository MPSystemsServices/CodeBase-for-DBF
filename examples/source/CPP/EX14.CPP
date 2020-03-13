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

//ex14.cpp
#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  // for all Borland compilers
#endif

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Tag4 tag ;

   // This example assumes that drive W is a read-only drive.
   Data4 prices( cb, "w:\\datafile.DBF" ) ;
   cb.exitTest( ) ;

   tag.initFirst( prices ) ;
   prices.select( tag ) ;
   if( prices.seek( "SMITH" ) == 0 )
      cout << "SMITH is found" << endl ;
   else
      cout << "SMITH is not found" << endl ;

   prices.close( ) ;
   cb.initUndo( ) ;
}
