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
*   LIST1.CPP    Copyright (C) 1999 Sequiter Software Inc.              *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 9 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

class Ages: public LINK4
{
   public:
      Ages(int newAge ) { age = newAge ; }
      int age ;
} ;

void printList(List4 &ageList)
{
   Ages    *nodePtr;

   cout << endl << "There are " << ageList.numNodes( )
        << " links." << endl ;

   nodePtr = (Ages *) ageList.first( ) ;
   while( nodePtr != NULL )
   {
      cout << nodePtr->age ;
      nodePtr = (Ages *) ageList.next(nodePtr);
   }
   cout << endl ;
}

void main( void )
{
   List4   ageList ;
   Ages    firstAge(3), middleAge(5), lastAge(7);

   ageList.add( &middleAge ) ;
   ageList.addBefore( &middleAge, &firstAge ) ;
   ageList.addAfter( &middleAge, &lastAge ) ;

   printList( ageList );

   ageList.remove( (void *) &middleAge);

   printList( ageList );

   ageList.pop( );

   printList( ageList );
}
