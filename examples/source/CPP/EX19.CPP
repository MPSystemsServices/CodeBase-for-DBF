//ex19.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void openAFile( Code4 &cb )
{
   // 'd' falls out of scope.  Data file is still open
   Data4 d( cb, "INFO" ) ;
}

void main( )
{
   Code4 cb ;
   openAFile( cb ) ;

   Data4 d = cb.data( "INFO" ) ; // obtain a new Data4 object

   if( d.isValid( ) )
   {
      cout << "INFO has " << d.recCount( ) << " records." << endl ;

      d.top( ) ;
      cb.data( "INFO").close( ) ; // an alternate way to close the file
   }
   cb.initUndo( ) ;
}
