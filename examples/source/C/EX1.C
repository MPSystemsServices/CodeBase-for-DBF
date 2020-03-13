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

#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  /* for all Borland compilers*/
#endif

void main( )
{
   CODE4 codeBase ;
	DATA4 *newDataFile ;
	FIELD4INFO fieldInfo [] =
	{
		{"NAME", 'C', 20, 0},
		{"AGE", 'N', 3, 0},
		{"BIRTHDATE", 'D', 8, 0},
		{0, 0, 0, 0},
	};

	code4init( &codeBase );
   codeBase.accessMode = OPEN4DENY_RW ;
   codeBase.safety = 0 ;  /* Ensure the create overwrites any existing file*/

   newDataFile = d4create( &codeBase, "NEWDBF", fieldInfo, 0 ) ;

   d4close( newDataFile ) ;

  /* open in shared mode*/
   codeBase.accessMode = OPEN4DENY_NONE ;
   newDataFile = d4open( &codeBase, "NEWDBF" ) ;

   /* ... some other code ...*/

   code4close( &codeBase ) ;
   code4initUndo( &codeBase) ;
}
