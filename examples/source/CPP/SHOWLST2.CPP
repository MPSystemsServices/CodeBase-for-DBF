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
*   SHOWLST2.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

Code4    codeBase ;
Data4    dataFile ;
Field4    fName, lName, address, age, birthDate, married, amount ;
Field4memo comment ;
Tag4      nameTag, ageTag, amountTag, addressTag, birthdateTag ;

TAG4INFO  tagInfo[ ] =
{
   {"PPL_NAME", "L_NAME+F_NAME", ".NOT. DELETED()", 0, 0},
   {"PPL_ADDR", "ADDRESS", 0, 0, 0},
   {"PPL_AGE", "AGE", "AGE >= 18", 0, 0},
   {"PPL_BRTH", "BIRTH_DATE", 0, 0, 0},
   {"PPL_AMNT", "AMOUNT", 0, 0, 0},
   {0, 0, 0, 0, 0},
};

void  OpenDataFile( void )
{
   codeBase.accessMode = OPEN4DENY_RW ;
   codeBase.autoOpen = 0;
   codeBase.safety = 0;
   dataFile.open( codeBase, "people.dbf" ) ;
   Index4 index ;
   index.create( dataFile, "SHOWLST2", tagInfo ) ;

   fName.init( dataFile, "F_NAME" ) ;
   lName.init( dataFile, "L_NAME" ) ;
   address.init( dataFile, "ADDRESS" ) ;
   age.init( dataFile, "AGE" ) ;
   birthDate.init( dataFile, "BIRTH_DATE" ) ;
   married.init( dataFile, "MARRIED" ) ;
   amount.init( dataFile, "AMOUNT" ) ;
   comment.init( dataFile, "COMMENT" ) ;

   nameTag.init( dataFile, "PPL_NAME" ) ;
   addressTag.init( dataFile, "PPL_ADDR" ) ;
   ageTag.init( dataFile, "PPL_AGE" ) ;
   birthdateTag.init( dataFile, "PPL_BRTH" ) ;
   amountTag.init( dataFile, "PPL_AMNT" ) ;
}

void PrintRecords( void )
{
   Date4 bDate ;
   Str4ten purchased ;
   Str4large name ;

   for( int rc = dataFile.top( ); rc == r4success; rc = dataFile.skip( ))
   {
      purchased.assignDouble( (double) amount, 8, 2 ) ;
      bDate.assign( birthDate ) ;
      name.assign( fName ) ;
      name.trim( ) ; name.add( " " ) ;
      name.add( lName ) ;

      cout << "\t\t-------------------------------" << endl ;
      cout << "\t\tName     : " << name.ptr( ) << endl ;
      cout << "\t\tAddress  : " << address.str( ) << endl ;
      cout << "\t\tAge : " << (int) age << " Married : "
           << married.str( ) << endl ;
      cout << "\t\tBirth Date: "
           << bDate.format( "MMMMMMMM DD, CCYY" ) << endl ;
      cout << "\t\tComment: " << comment.str( ) << endl ;
      cout << "\t\tPurchased this year:$" << purchased.ptr( ) << endl ;
   }
}

void main( void )
{
   OpenDataFile( ) ;

   dataFile.select( nameTag ) ;

   cout << "Order by Name" << endl ;
   PrintRecords( ) ;
   getchar( ) ;

   dataFile.select( ageTag ) ;
   cout << "Order by Age" << endl ;
   PrintRecords( ) ;
   getchar( ) ;

   dataFile.select( amountTag ) ;
   cout << "Order by Amount" << endl ;
   PrintRecords( ) ;
   getchar( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
