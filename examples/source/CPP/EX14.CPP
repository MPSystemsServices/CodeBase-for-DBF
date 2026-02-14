//ex14.cpp
#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  // for all Borland compilers
#endif

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Tag4 tag ;

   // This example assumes that drive W is a read-only drive.
   Data4 prices( cb, "w:\\datafile.DBF" ) ;
   cb.exitTest( ) ;

   tag.initFirst( prices ) ;
   prices.select( tag ) ;
   if( prices.seek( "SMITH" ) == 0 )
      cout << "SMITH is found" << endl ;
   else
      cout << "SMITH is not found" << endl ;

   prices.close( ) ;
   cb.initUndo( ) ;
}
