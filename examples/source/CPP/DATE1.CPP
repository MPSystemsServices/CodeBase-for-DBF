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
*   DATE.CPP    Copyright (C) 1999 Sequiter Software Inc.               *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 8 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

int  validDate( Date4 &date )
{
   if( long( date ) < 1)
      return 0;
   else
      return 1;
}

void howLongUntil( Date4 &tillDate, char *title )
{
   Date4 today ;
   today.today( ) ;

   cout << "Today's date is " << today.format( "MMM DD/CCYY" ) << endl ;

   int thisYear = today.year( ) ;

   Str4len year( tillDate.ptr( ), 4 ) ;
   year.assignLong( today.year( ) ) ;

   if( tillDate < today )
   {
      thisYear ++;
      year.assignLong( thisYear ) ;
   }

   long days = long(tillDate) - long(today) ;

   cout << "There are " << days << " days until "
        <<  title << endl
        << "(which is a " << tillDate.cdow( )
        << " this year)" << endl << endl ;
}

void main( void )
{
   Date4 christmas( "19801225" ) ;
   howLongUntil( christmas, "Christmas");

   Date4 birthday ;

   do
   {
      char birthdate[ 80 ] ;
      cout << "Please enter your birthdate in "
           << "\"DEC20/1993\" format: " ;
      cin >> birthdate ;

      birthday.assign( birthdate, "MMMDD/CCYY" ) ;
   } while( !validDate( birthday ) ) ;

   howLongUntil( birthday, "your next birthday");
}
