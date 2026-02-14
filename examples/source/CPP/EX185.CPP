//ex185.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main( void )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Tag4 tag( data, "INF_NAME" ) ;

   d4tagSelect( (DATA4 *) data, (TAG4 *) tag ) ; // Call a CodeBase C function

   data.top( ) ;
   cout << data.record( ) << endl ;

   cb.initUndo( ) ;
}
