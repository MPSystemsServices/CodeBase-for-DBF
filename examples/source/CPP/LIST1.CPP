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
