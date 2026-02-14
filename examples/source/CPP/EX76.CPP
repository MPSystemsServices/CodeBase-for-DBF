//ex76.cpp
#include "d4all.hpp"

void main( )
{
   long yesterday ;
   Date4 today ;
   today.today( ) ; // Get the current date from the system clock

   yesterday = today - 1L ;

   Date4 tomorrow = yesterday + 2L ; // Date4::Date4( long ) called.

   cout << "Today is " << today.format( "MMM DD, CCYY" ) << endl ;
   cout << "The julian date for yesterday is " << yesterday << endl ;
   cout << "The julian date for tomorrow is " << (long) tomorrow << endl ;
}
