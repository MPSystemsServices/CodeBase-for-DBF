#include "d4all.hpp"

extern unsigned _stklen = 10000; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   data.go( data.recCount( ) + 1 ) ;

   cout << "An error message was displayed" << endl ;   
   cb.errorCode = cb.errGo = 0 ;

   data.go( data.recCount( ) + 1 ) ;
   cout << "No error message was displayed" << endl ;

   cb.initUndo( ) ;
}
