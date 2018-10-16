//ex93.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 code ;
   Data4 db( code, "DATA" ) ;
   Expr4 exp( db, "LNAME=\'Smith\'" ) ; // a logical dBASE expression
   long count = 0 ;

   for( db.top( ) ; !db.eof( ) ; db.skip( ) )
      #ifndef S4USE_TRUE
      if( exp.isTrue( ) )
      #else
      if( exp.true( ) )
      #endif
         count++ ;

   cout << count << " record(s) had a last name of Smith" << endl ;

   code.initUndo( ) ;
}
