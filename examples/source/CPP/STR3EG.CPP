/***********************************************************************\
*                                                                       *
*   STR3EG.CPP    Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 3 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

void main( )
{
   Code4 cb ;
   Data4 data( cb, "info" ) ;
   Field4 age( data, "AGE" ), name( data, "NAME" ) ;

   // Make everyone one year older, except those people
   // who are 39 'and holding'
   data.lockFile( ) ;
   for( data.top( ); !data.eof( ); data.skip( ) )
   {
      if( 39 != int( age ))
      {
         age.assignLong( long( age ) + 1L ) ;
         cout << name.str( ) << " is now " << int( age ) << endl ;
      }
      else
         cout << name.str( ) << " is 39 and holding" << endl ;
   }
   data.close( ) ;
   cb.initUndo( ) ;
}
