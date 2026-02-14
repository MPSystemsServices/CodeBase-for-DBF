//ex32.cpp
#include "d4all.hpp"

#ifdef __TURBOC__  // for all Borland compilers
   extern unsigned _stklen = 10000 ;
#endif

void main()
{
   Code4 cb ;
   Data4 data( cb, "DATA1") ;
   data.lockAll( ) ;
   for( data.top( ) ; !data.eof() ; data.skip() )
      data.blank( )  ;  // blank out all records in the data file

   cb.initUndo( ) ;    // close all files and release any memory
}
