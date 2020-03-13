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

//ex133.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( int argc, char **argv )
{
   Code4 cb ;
   char buffer[ 0x1FFF ] ;
   File4 file( cb, "TEXT.FIL" ) ;
   File4seqWrite seqFile( file, 0, buffer, sizeof( buffer ) ) ;

   // write 1-100 to file as integers
   int i ;
   Str4len strI( &i, sizeof(i) ) ;
   for( i = 1 ; i <=100 ; i++ )
      seqFile << (int)strI ;

   // write the program's name and the number of parameters to the file
   seqFile << Str4ptr( argv[0] ) << Str4len( &argc, sizeof(int) ) ;

   seqFile.flush( ) ; // flush the changes and close the file
   file.close( ) ;
   cb.initUndo( ) ;
}
