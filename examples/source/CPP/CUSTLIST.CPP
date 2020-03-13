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

/***********************************************************************\
*                                                                       *
*   CUSTLIST.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int main( void )
{
   Code4    codeBase;
   Data4    dataFile (codeBase, "data1.dbf" ) ;
   Field4   fNameFld( dataFile, "F_NAME" ),
            lNameFld( dataFile, "L_NAME" ),
            addressFld( dataFile, "ADDRESS" ),
            ageFld( dataFile, "AGE" ),
            birthDateFld( dataFile, "BIRTH_DATE" ),
            marriedFld( dataFile, "MARRIED" ),
            amountFld( dataFile, "AMOUNT" );
   Field4memo commentFld( dataFile, "COMMENT" );

   codeBase.exitTest( ) ;
   Date4 birthDate ;
   Str4ten purchased ;
   Str4large name ;
   for( int rc = dataFile.top( ); rc == r4success; rc = dataFile.skip( ))
   {
      purchased.assignDouble( (double) amountFld, 8, 2 ) ;
      birthDate.assign( birthDateFld.ptr( ) ) ;
      name.assign( fNameFld ) ;
      name.trim( ) ;
      name.add( " " );
      name.add( lNameFld ) ;

      cout << "-------------------------------" << endl ;
      cout << "Name     : " << name.ptr( ) << endl ;
      cout << "Address  : " << addressFld.str( ) << endl ;
      cout << "Age : " << (int) ageFld << " Married : "
           << marriedFld.str( ) << endl ;
      cout << "Birth Date: " << birthDate.format("MMMMMMM DD, CCYY" ) << endl ;
      cout << "Comment: " << commentFld.str( ) << endl ;
      cout << "Purchased this year:$" << purchased.ptr( ) << endl ;
   }
   dataFile.close( ) ;
   codeBase.initUndo( ) ;
   return 0 ;
}
