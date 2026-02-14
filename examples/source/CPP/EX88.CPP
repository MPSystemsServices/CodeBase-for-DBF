#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void addAThousandYears( Date4 &d )
{
   *d.ptr( ) = (*d.ptr( ))+ 1 ;
}

void main( ) 
{
	Date4 today ;
	today.today( ) ;
	cout << today.str( ) << endl ;
	addAThousandYears( today ) ;
	cout << today.str( ) << endl ;
}

