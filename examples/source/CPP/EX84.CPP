//ex84.cpp
#include "d4all.hpp"

void setEndOfWeek( Date4 &d )
{
   long tillEnd = 7 - d.dow( ) ;
   d += tillEnd ;
}

void main( )
{
   Date4 today ;
   today.today( ) ;
   cout << "today is " << today.str( ) << endl ;
   setEndOfWeek( today ) ;
   cout << "the end of the week is " << today.str( ) << endl ;
}
