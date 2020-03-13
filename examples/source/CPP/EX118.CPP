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

//ex118.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 textFile ;

   cb.accessMode = OPEN4DENY_RW ;
   cb.errCreate = 0 ; // Handle the errors at the application level
   int rc = textFile.create( cb, "C:\\TEMP\\TEXT.FIL" ) ;

   if( rc < 0 || rc == r4noCreate)
   {
      cout << "File 'C:\\TEMP\\TEXT.FIL' NOT created. Make sure it does not exist." << endl ;
      cb.exit( ) ;
   }
   textFile.write( 0, "Some Sample Text", 16 ) ;

   textFile.close( ) ;
   cb.initUndo( ) ; // flush changes and close file.
}
