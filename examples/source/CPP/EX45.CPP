//ex45.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; //for Borland compilers

void displayAlias( Data4 d )
{
   if( d.isValid( ) )
      cout << d.alias( ) << " is valid." << endl ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   displayAlias( data ) ;
   cb.initUndo( ) ;
}
