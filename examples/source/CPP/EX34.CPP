#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   data.bottom( ) ;
   cout << "Last Name added: " << Field4( data, "NAME").str() << endl ;

   data.close() ;
   cb.initUndo() ;
}
