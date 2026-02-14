#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Field4info fields( cb ) ; // initialize with no fields
   
   fields.add( "NAME", 'C', 20 ) ;
   fields.add( "AGE",  'N',  3 ) ;
   fields.add( "REGISTER", 'L' ) ;
   fields.add( "birth",'D' ) ;
   
   // change your mind and delete them all
   if( fields.del( "nome   " ) < 0 )
      cout << "This will always happen" << endl ;
   fields.del( "name" ) ;
   fields.del( 0 ) ; // delete the 'AGE' field
   fields.del( 0 ) ; // delete the 'REGISTER' field
   
   fields.free( ) ; // delete all remaining
   
   cb.initUndo( ) ;
}
