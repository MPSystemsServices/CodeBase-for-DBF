#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 today ;
   today.today( ) ;
   Date4 tomorrow = today + 1L ;

   if( Date4(today++) == tomorrow )
   {
      cout << "This will never happen, since today" 
           << " is incremented after" << endl ;
   }
   if( today == tomorrow)
      cout << "This will always happen" << endl ;

   if( Date4(++today) == tomorrow )
   {
      cout << "This will never happen, since today"
           << " is incremented first" << endl ;
   }
}
