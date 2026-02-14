//ex17.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 db( cb, "DATA" ) ;
   db.top( ) ;

   Expr4 ex( db, "TRIM(LNAME)+', '+TRIM(FNAME)" ) ;
   cb.calcCreate( ex, "NAMES" ) ;
   cout << ex.vary( ) << endl ;

   Expr4 ex2 ;
   ex2.parse( db, "'HELLO '+NAMES()" ) ; // no space in dBASE function calls.

   cout << ex2.vary( ) << endl ;

   ex2.free( ) ;
   cb.calcReset( ) ;
   cb.initUndo( ) ;
}
