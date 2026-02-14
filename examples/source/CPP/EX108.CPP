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
   
   Data4 data ;
   cb.safety = 0 ; // overwrite existing DATAFILE.DBF

   data.create( cb, "DATAFILE", fields.fields( ) ) ;
   if( data.isValid( ) && data.numFields( ) == 4 )
      cout << "CREATED SUCCESSFULLY" << endl ;
   
   fields.free( ) ;
   cb.initUndo( ) ;
}
