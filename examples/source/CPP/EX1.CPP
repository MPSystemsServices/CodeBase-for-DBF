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

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  // for all Borland compilers
#endif

void main( )
{
   Code4 codeBase ;
   codeBase.accessMode = OPEN4DENY_RW ;
   codeBase.safety = 0 ;  // Ensure the create overwrites any existing file

   Field4info fields( codeBase )  ;

   fields.add( "NAME", 'C', 20 ) ;
   fields.add( "AGE", 'N', 3, 0 ) ;
   fields.add( "BIRTHDATE", 'D') ;

   Data4 newDataFile ;
   newDataFile.create( codeBase, "NEWDBF", fields.fields( ) ) ;

   newDataFile.close( ) ;

  // open in shared mode
   codeBase.accessMode = OPEN4DENY_NONE ;
   newDataFile.open( codeBase, "NEWDBF" ) ;

   // ... some other code ...

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
