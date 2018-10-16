#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

FIELD4INFO fields[ ] =
{
   { "NAME", 'C', 20, 0 },
   { "AGE", 'N', 3, 0 },
   { 0,0,0,0 },
} ;

void main( )
{
   Code4 cb ;
   DATA4 *data ;

   cb.safety = 0 ;
   cb.accessMode = OPEN4DENY_RW ;

   data = d4create( &cb, "PERSON", fields, 0 ) ;

   if( data )
      printf( "PERSON data file successfully created.\n" ) ;

   Tag4info tags( cb ) ;
   tags.add( "NAME_TAG", "NAME" ) ;

   Index4 index ;
   Data4 dataCpp( data ) ;
   index.create( dataCpp, tags.tags( ) ) ;
   if( index.isValid( ) )
      cout << "PERSON index file successfully created." << endl ;

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
