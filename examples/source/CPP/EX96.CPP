#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA" ) ;
   Data4 info( cb, "INFO" ) ;
   Expr4 expr ;

   expr.parse( data, "FNAME+' '+DTOS( INFO->BIRTH_DATE)" ) ;

   data.top( ) ; 
   info.top( ) ;
   
   cout << "First name from DATA and birth date from INFO: "
        << Str4len( expr.vary( ), expr.len() ).str() << endl ;
   
   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
