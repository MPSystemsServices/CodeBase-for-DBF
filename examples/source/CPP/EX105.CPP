#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

static FIELD4INFO statFields[] = 
{
   { "NAME", 'C', 20, 0 },
   { "AGE",  'N',  3, 0 },
   { "BDATE", 'D', 8, 0 },
   { 0,0,0,0 } 
} ;

void main( )
{
   Code4 cb ;
   Data4 oneDbf, twoDbf ;
   Field4info fields( cb ) ;
   cb.safety = 0 ;

   fields.add( "NAME",  'C', 20 ) ;
   fields.add( "AGE",   'N', 3 ) ;
   fields.add( "BDATE", 'D' ) ;
   
   // these two lines create identical datafiles
   oneDbf.create( cb, "EX105_1", fields.fields( ) ) ;
   twoDbf.create( cb, "EX105_2", statFields ) ;
   
   if( ! cb.errorCode )
      cout << "Created successfully" << endl ;
   else
      cout << "An error occurred" << endl ;    
      
   fields.free( ) ;
   cb.initUndo( ) ;
}
