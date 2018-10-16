//ex28.cpp
#include "d4all.hpp"

#ifdef __TURBOC__ // for all Borland compilers
   extern unsigned _stklen = 10000 ;
#endif

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   if( cb.errorCode )
   {
      cout << "An error occurred in the Data4 constructor" << endl ;
      cb.exit( ) ;
   }

   data.top() ;
   cout << "Number of records in " << data.fileName( ) <<": " ;
   cout << data.recCount( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
