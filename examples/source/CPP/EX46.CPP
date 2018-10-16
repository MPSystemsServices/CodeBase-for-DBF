//ex46.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  /* Borland Only */

void main()
{
   Code4  cb ;
   Data4 data ;

   data.open( cb, "INFO" ) ;

   cb.lockAttempts = 4 ; // Try four times

   int rc = data.lock( 5 ) ;
   if( rc == r4success )
      cout << "Record 5 is now locked." << endl ;
   else if( rc == r4locked )
      cout << "Record 5 is locked by another user" << endl ;

   cb.lockAttempts = WAIT4EVER ; // Try forever
   rc = data.lock( 5 ) ;

   if ( rc == r4success )
      cout << "This will always happen." << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
