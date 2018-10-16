//ex21.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void exitToSystem( Code4 &cb )
{
   cout << "Shutting down application ... " << endl ;
   cb.closeAll( ) ;
   cb.exit( ) ;
}

void main( )
{
   Code4 cb ;
   exitToSystem( cb ) ;
}
