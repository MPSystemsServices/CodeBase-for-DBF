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

//ex130.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;
#define LINE_SIZE 79

void main( int argc, char **argv )
{
   Code4 cb ;
   char buffer[ 0x1FFF ] ;  // 7K buffer for reads
   if( argc < 2 )
   {
      cout << "Usage: PROGRAM <fileName> " << endl ;
      cb.initUndo( ) ;
      cb.exit( ) ;
   }

   File4 fileName(cb, argv[1]);

   File4seqRead text( fileName, 0, buffer, sizeof( buffer ) ) ;

   if( !fileName.isValid( ) )
   {
      cb.initUndo( ) ;
      cb.exit( ) ;
   }

   Str4large line ;
   line.setLen( LINE_SIZE ) ;
   line.set( '\0') ;

   while( 1 )
   {
      text >> line ; // read one line worth of text
      if( line.len( ) == LINE_SIZE )
         cout << line.str( ) << endl;
      else
      {
        line.setLen( line.len( ) -1 ) ; // remove end of file marker
        cout << line.str( ) << endl;
        break ;
      }
   }
   fileName.close( ) ;
   cb.initUndo( ) ;
}
