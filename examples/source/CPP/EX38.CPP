//ex38.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 codeBase ;
   Data4 file( codeBase, "INFO") ;

   codeBase.optStart( ) ;

   long count = 0 ;

   for( file.top( ); !file.eof( ); file.skip( ) )
      if( file.deleted( ) )
         count++ ;

   cout << "\"INFO\" has " << count << " deleted records" << endl ;

   codeBase.initUndo( ) ;
}
