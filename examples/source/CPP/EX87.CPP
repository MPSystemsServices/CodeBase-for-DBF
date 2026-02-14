//ex87.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

static int daysInMonth[] =
{ 0,31,28,31,30,31,30,31,31,30,31,30,31 } ;

void makeEndOfMonth( Date4 &d )
{
   int endOfMonth = daysInMonth[d.month( )] ;
   if( d.month( ) == 2 && d.isLeap( ) )
      endOfMonth++ ;

   d+= (endOfMonth - d.day( )) ;
}

void main( )
{
   Date4 today ;
   today.today( ) ;
   cout << today.str( ) << endl ;
   makeEndOfMonth( today ) ;
   cout << today.str( ) << endl ;
}
