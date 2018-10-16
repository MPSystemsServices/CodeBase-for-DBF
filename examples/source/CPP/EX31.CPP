//ex31.cpp
#include "d4all.hpp"

#ifdef __TURBOC__  // for all Borland compilers
   extern unsigned _stklen = 10000 ;
#endif

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   // Add 5 blank records
   for( int i = 5 ; i ; i-- )
      data.appendBlank( ) ;

   // Close the data file.
   data.close( ) ;
   cb.initUndo( ) ; // Free up any memory used.
}
