/* ex25.cpp */
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( )
{
  Code4 cb ;
  Data4 data( cb, "INFO" ) ;

  data.lockAll( ) ;
  data.optimizeWrite( 1 ) ;
  cb.optStart( ) ;
  // .. some other code

  cb.optSuspend( ) ; // flush & free optimization memory.
  data.unlock( ) ; // let other users make modifications.

  //  ... some other code

  cb.optStart( ) ;

  // ... other code
  cb.initUndo( ) ;
}
