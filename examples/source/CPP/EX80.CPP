//ex80.cpp
#include "d4all.hpp"

void subtractYear( Date4 &d, int numYears )
{
   cout << "The date " << numYears << " years ago is " ;
   for( ; numYears ; numYears -- )
   {
      d -= 365L ;
      if( d.isLeap( ) )
         d-- ;
   }
   cout << d.str( ) << endl ;
}

void main( )
{
   Date4 today ;
   today.today( ) ;
   subtractYear( today, 10L ) ; // subtract 10 years from today
}
