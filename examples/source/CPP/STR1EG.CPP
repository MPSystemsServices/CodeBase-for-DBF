/***********************************************************************\
*                                                                       *
*   STR1EG.CPP    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 3 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

void main( )
{
   Str4ptr newName( "JOHN JONES" );
   cout << newName.str() << endl;

   Str4large newerName( newName ); // copy JOHN JONES
   Str4ptr temp("BILL");
   newerName.replace( temp );   // make it BILL JONES
   cout << newerName.str() << endl;

   Str4large newestName( newerName ) ;  // copy BILL JONES
   newestName.insert( temp = Str4ptr("A. "), 5 ) ;   // make it BILL A. JONES
   cout << newestName.str() << endl;
}
