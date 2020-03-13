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
*   SEEKER.C      Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

Code4 codeBase ;
Data4 dataFile ;
Field4 fName, lName, address, age, birthDate, married, amount ;
Field4memo comment ;
Tag4 nameTag, ageTag, amountTag, addressTag, birthdateTag;

void  OpenDataFile( void )
{
   dataFile.open( codeBase, "people.dbf" ) ;
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

void seekStatus(int rc, Str4ten &status)
{
   switch(rc)
   {
      case r4success:
         status.assign( "r4success" ) ;
         break;

      case r4eof:
         status.assign( "r4eof" ) ;
         break;

      case r4after:
         status.assign( "r4after" ) ;
         break;

      default:
         status.assign( "other" ) ;
         break;
   }
}

void printRecord( int rc )
{
   Date4 bDate ;
   Str4ten purchased ;
   Str4large name ;
   Str4ten status ;
   seekStatus( rc, status ) ;

   purchased.assignDouble( (double) amount, 8, 2 ) ;
   bDate.assign( birthDate ) ;
   name.assign( fName ) ;
   name.trim( ) ;
   name.add( " " ) ;
   name.add( lName ) ;
   cout << "Seek status : " << status.ptr( ) << endl ;
   cout << "---------------------------------" << endl ;

   cout << "Name     : " << name.ptr( ) << endl ;
   cout << "Address  : " << address.str( ) << endl ;
   cout << "Age : " << (int) age << " Married : "
               << married.str( ) << endl ;
   cout << "Birth Date: "
               << bDate.format( "MMMMMMMM DD, CCYY" ) << endl ;
   cout << "Comment: " << comment.str( ) << endl ;
   cout << "Purchased this year:$" << purchased.ptr( ) << endl ;
}

void main( void )
{
   int rc;

   OpenDataFile( ) ;

   dataFile.select( addressTag ) ;
   rc = dataFile.seek( "123 - 76 Ave   ") ;
   printRecord( rc ) ;

   rc = dataFile.seek( "12") ;
   printRecord( rc ) ;

   rc = dataFile.seek( "12             ") ;
   printRecord( rc ) ;

   dataFile.select( birthdateTag) ;
   rc = dataFile.seek( "19581111") ; // October 11, 1958
   printRecord( rc ) ;

   dataFile.select( amountTag) ;
   rc = dataFile.seek( 98.99) ;
   printRecord( rc ) ;

   rc = dataFile.seek( " 98.99") ;
   printRecord( rc ) ;

   //The following code finds all the occurences
   //John Albridge in the tag
   dataFile.select( nameTag) ;
   rc = dataFile.seekNext( "John      Albridge  ") ;

   for (rc; rc == r4success; rc = dataFile.seekNext("John      Albridge  ") )
      printRecord( rc );

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
