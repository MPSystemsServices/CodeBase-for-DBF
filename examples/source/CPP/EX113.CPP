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

//ex113.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA3" ) ;

   data.top( ) ;
   Field4memo comments( data, "COMMENTS" ) ;
   data.lockAll( ) ;
   comments.setLen( 0x4000 ) ; // 16K
   comments.assign( "First characters of a 16k memo field" ) ;

   // Flush changes to disk and close the data and memo files
   data.close( ) ;

   data.open( cb, "DATA2" ) ;
   comments.init( data, "COMMENT" ) ;
   data.top( ) ;

   cout << "Memo field Value:" << endl << comments.str( ) << endl
        << "Length of memo field: " << comments.len( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
