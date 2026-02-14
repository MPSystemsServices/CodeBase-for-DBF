//ex42.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;
   Data4 data( cb, "INFO" );
   Field4 age( data, "AGE" ) ;

   data.go( 2 ) ;
   age.assignLong( 49 ) ;

   // Explicitly flush the change to disk in case power goes out
   data.flush( ) ;

   // some other code

   data.close( ) ;
   cb.initUndo( ) ;
}
