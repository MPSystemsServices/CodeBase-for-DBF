//ex90.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 d ;
   d.today( ) ;
   cout << "Today is " << d.cdow( ) << "." << endl ;

   int daysToWeekEnd = 7 - d.dow( ) ;
   if( daysToWeekEnd == 0 || daysToWeekEnd == 6 )
      cout << "Better enjoy it!" << endl ;
   else
   {
      cout << "Only " << daysToWeekEnd
           << " more to go 'till the weekend." << endl ;
   }
}
