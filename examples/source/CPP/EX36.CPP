//ex36.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  /* Borland Only */

/* Check the validity of an index file. */
void main( int argc, char *argv[ 2 ] )
{
   if ( argc > 2 )
   {
      Code4 cb ;
      cb.autoOpen = 0 ;  // open index file manually.
      cb.errOff = 1;

      Data4 checkData( cb, argv[1] ) ;

      // Demonstration of Index4::open instead of Index4::Index4
      Index4 testIndex ;
      testIndex.open( checkData, argv[2] ) ;

      cb.exitTest( ) ;
      cb.optStart( ) ;

      cb.errorCode = 0;
      if ( checkData.checkIndex( ) == 0 )
         cout << endl << "Index is OK !!" << endl;
      else
         cout << endl << "Problem with Index" << endl ;
      cb.initUndo( ) ;
   }
   else
      cout << endl << "PROGRAM  DataFile  IndexFile" << endl ;
}
