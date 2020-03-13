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
*   NEWLIST2.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

Code4 codeBase ;
Data4 dataFile ;
Field4 fName, lName, address, age, birthDate, married, amount;
Field4memo comment ;
Tag4 nameTag, ageTag, amountTag ;

FIELD4INFO  fieldInfo [] =
{
   {"F_NAME",r4str,10,0},
   {"L_NAME",r4str,10,0},
   {"ADDRESS",r4str,15,0},
   {"AGE",r4num,2,0},
   {"BIRTH_DATE",r4date,8,0},
   {"MARRIED",r4log,1,0},
   {"AMOUNT",r4num,7,2},
   {"COMMENT",r4memo,10,0},
   {0,0,0,0},
};

TAG4INFO tagInfo[] =
{
   {"PPL_NAME","F_NAME + L_NAME",".NOT. DELETED()",0,0},
   {"PPL_ADDR","ADDRESS",0,0,0},
   {"PPL_BRTH","BIRTH_DATE","AGE >= 18",0,0},
   {"PPL_AGE","AGE",0,0,0},
   {"PPL_AMNT","AMOUNT",0,0,0},
   {0,0,0,0,0},
};

void  CreateDataFile( void )
{
   dataFile.create( codeBase, "people.dbf", fieldInfo, tagInfo ) ;

   fName.init( dataFile, "F_NAME");
   lName.init( dataFile, "L_NAME");
   address.init( dataFile, "ADDRESS");
   age.init( dataFile, "AGE");
   birthDate.init( dataFile, "BIRTH_DATE");
   married.init( dataFile, "MARRIED");
   amount.init( dataFile, "AMOUNT");
   comment.init( dataFile, "COMMENT");

   nameTag.init( dataFile, "PPL_NAME");
   ageTag.init( dataFile, "PPL_AGE");
   amountTag.init( dataFile, "PPL_AMNT");
}

void PrintRecords( void )
{
   Date4 bDate ;
   Str4ten purchased ;
   Str4large name ;

   for( int rc = dataFile.top( ); rc == r4success;
   rc = dataFile.skip( ))
   {
      purchased.assignDouble( (double) amount, 8, 2 ) ;
      bDate.assign( birthDate ) ;
      name.assign( fName ) ;
      name.trim( ) ; name.add( " " ) ;
      name.add( lName ) ;

      cout << "-------------------------------" << endl ;
      cout << "Name     : " << name.ptr( ) << endl ;
      cout << "Address  : " << address.str( ) << endl ;
      cout << "Age : " << (int) age << " Married : "
           << married.str( ) << endl ;
      cout << "Birth Date: "
           << bDate.format( "MMMMMMMM DD, CCYY" ) << endl ;
      cout << "Comment: " << comment.str( ) << endl ;
      cout << "Purchased this year:$" << purchased.ptr( ) << endl ;
   }
}

void AddNewRecord(char *fNameStr, char*lNameStr, char *addressStr
                 ,char *dateStr, int marriedValue, double amountValue
                 ,char *commentStr = NULL )
{
   dataFile.lockAll( ) ;
   dataFile.appendStart( ) ;
   dataFile.blank( ) ;

   fName.assign( fNameStr ) ;
   lName.assign( lNameStr ) ;
   address.assign( addressStr ) ;

   Date4 bDate, today ;
   bDate.assign( dateStr ) ;
   today.today( ) ;
   // approximate age -- ignore leap year
   long ageValue = ((long) today - (long) bDate) / 365 ;
   age.assignLong( ageValue ) ;
   birthDate.assign( bDate ) ;

   if( marriedValue )
      married.assign( "T" ) ;
   else
      married.assign( "F" ) ;

   amount.assignDouble( amountValue ) ;
   if( commentStr )
      comment.assign( commentStr ) ;

   dataFile.append( ) ;
   dataFile.unlock( ) ;
}

void main( void )
{
   codeBase.safety = 0 ;
   codeBase.lockEnforce = 0 ;

   CreateDataFile( ) ;

   AddNewRecord( "Sarah", "Webber", "132-43 St.", "19600212", 1, 147.99
                      , "New Customer");
   AddNewRecord("John", "Albridge", "1232-76 Ave.", "19581023", 0, 98.99 ) ;

   PrintRecords( ) ;

   dataFile.select( nameTag ) ;
   PrintRecords( ) ;

   dataFile.select( ageTag ) ;
   PrintRecords( ) ;

   dataFile.select( amountTag ) ;
   PrintRecords( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
   return ;
}
