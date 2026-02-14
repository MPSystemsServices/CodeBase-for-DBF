//ex23.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( void )
{
   Code4 cb ;
   Data4 data1( cb, "DATA1" ), data2( cb, "DATA2" ) ;

   data1.top( ) ;  data2.top( ) ;

   data1.lockAddFile( ) ;
   data2.lockAddAppend( ) ;

   long numRecords = data2.recCount( ) ;
   data2.lockAdd( numRecords ) ;
   data2.lockAdd( numRecords-1 ) ;
   data2.lockAdd( numRecords-2 ) ;

   if( cb.lock( ) == r4success )
      cout << "All locks were successfully performed" << endl ;

   cb.initUndo( ) ;
}
