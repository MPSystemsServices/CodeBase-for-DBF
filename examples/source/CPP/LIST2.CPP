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
*   LIST2.CPP    Copyright (C) 1999 Sequiter Software Inc.              *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 9*/

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

class AgeList : public List4
{
public:
   ~AgeList( ) { freeAges( ) ; }
   Ages *addAge (int) ;
   void freeAges( ) ;
   void removeAge( Ages * ) ;
   void printList( ) ;
} ;

Ages *AgeList::addAge ( int nAge )
{
   return (Ages *) add ( new Ages( nAge ) );
}

void AgeList::removeAge(Ages *rAge )
{
   remove( rAge );
   delete rAge;
}

void AgeList::freeAges( )
{
   Ages *a ;
   while( (a = (Ages *)pop( )) !=NULL)
      delete a ;
}

void AgeList::printList( )
{
   Ages *nodePtr;
   cout << endl << "There are " << numNodes( ) << " links." << endl;

   nodePtr = ( Ages *)first( ) ;
   while(nodePtr != NULL)
   {
      cout << nodePtr->age ;
      nodePtr = (Ages *) next(nodePtr) ;
   }
   if (numNodes( ) )
      cout << endl << ((Ages *) selected) ->age << " is selected " << endl;
}

void main(void)
{
   AgeList ageList ;

   ageList.addAge(3) ;
   Ages *agePtr = ageList.addAge(5);
   ageList.addAge(7);
   ageList.addAge(6);
   ageList.addAge(2);

   ageList.selected = agePtr ;

   ageList.printList( ) ;

   ageList.removeAge( agePtr ) ;
   ageList.printList( ) ;

   ageList.freeAges( ) ;
   ageList.printList( ) ;
}
