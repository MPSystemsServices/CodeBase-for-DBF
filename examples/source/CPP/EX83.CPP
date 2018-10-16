//ex83.cpp
#include "d4all.hpp"

void main( )
{
   Date4 today ;
   today.today( ) ;

   cout << "The current month is " << today.cmonth( ) << endl ;
   // Displays "The current month is January" if the system clock says
   // that it is.
}
