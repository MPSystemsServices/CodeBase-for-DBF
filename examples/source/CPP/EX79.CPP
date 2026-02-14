//ex79.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

class myClass : public Date4
{
public:
   myClass( char *value, char *pict = NULL )
      { assign( value, (pict) ? pict:"CCYYMMDD" ) ; }
   void addWeek( long toAdd = 1 ) ;
} ;

void myClass::addWeek( long toAdd )
{
   *this += toAdd * 7L ;
}

void main( )
{
   myClass date( "Nov 11, 1995", "MMM DD, CCYY" ) ;
   cout << date.str( ) << " is the initial date" << endl ;
   date.addWeek( 2L ) ; // add 2 weeks to the date
   cout << date.str( ) << " is 2 weeks later" << endl ;
}
