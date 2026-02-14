#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

#define VOTE_AGE 18.0

void main( )
{
   Code4 cb ;
   Data4 db( cb, "INFO" ) ;
   Expr4 expr( db, "AGE" ) ;

   long count = 0 ;
   for( int rc = db.top( ) ; rc != r4eof ; rc = db.skip( ) )
      if( double( expr ) >= VOTE_AGE )
         count ++ ;

   cout << "Possible voters: " << count << endl ;
   
   expr.free( ) ;
   cb.initUndo( ) ;
}
