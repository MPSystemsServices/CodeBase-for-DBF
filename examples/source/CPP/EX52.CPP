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

//ex52.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;

   // Open the file exclusively, default optimization is the same as if
   // Data4::optimize( OPT4EXCLUSIVE ) were called.
   Data4  info( cb, "INFO" ) ;

   // open a shared file.
   cb.accessMode = OPEN4DENY_NONE ;
   Data4 extra( cb, "DATA" ) ;

   extra.optimize( OPT4ALL ) ;  // read optimize the "DATA" file

   int rc = cb.optStart( ) ;  // Begin the memory optimizations.
   if ( rc == r4success )
      cout << "Memory optimiztion is implemented" << endl ;
   else
      cout << "Either the S4OFF_OPTIMIZE switch is defined or "
            << "there is insufficient memory for optimization" << endl ;

   // .... Some other code ....

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
