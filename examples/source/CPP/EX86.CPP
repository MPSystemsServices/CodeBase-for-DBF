//ex86.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 dt( "19900101" ) ;
   dt += 30 ; // alter what is stored in the dt object.

   char dateValue[9] ;
   memcpy( dateValue, dt.ptr( ), dt.len( ) );
   dateValue[8] = 0 ;

   cout << "dateValue is " << dateValue << endl ;
}
