#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ), data2 ;

   cb.safety = 0 ;
   Field4info fields( data ) ;
   fields.add( "COMMENT", 'M' ) ;

   data2.create( cb, "DATA2", fields.fields( ) ) ;
   if( cb.errorCode == 0 )
      cout << "Created successfully" << endl ;
   fields.free( ) ;
   cb.initUndo( ) ;
}
