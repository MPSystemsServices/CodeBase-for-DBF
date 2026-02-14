//ex128.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers
#define DATE_OFFSET 16

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;
   File4 file ;
   Date4 runDate ;

   runDate.today( ) ; // initialize to the system clock

   if( file.open( cb, "TEXT.FIL" ) !=0 )
      return ;

   // file is opened exclusively - no need to lock
   file.write( DATE_OFFSET, runDate );
   file.close( ) ;
   cb.initUndo( ) ;
}
