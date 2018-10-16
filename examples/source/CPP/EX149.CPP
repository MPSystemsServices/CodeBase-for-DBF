//ex149.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Sort4 sort ;
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO") ;
   Field4 field( data, 1 ) ;
   cb.exitTest( ) ;

   sort.init( cb, field.len( ), sizeof(char) ) ;
   for( data.top( ) ; !data.eof( ) ; data.skip( ) )
   {
      sort.put( field.ptr( ), data.record( ), data.recNo( ) ) ;
   }
   sort.getInit( ) ;
   while( sort.get( ) == 0 )
   {
      Str4len outField( sort.result, field.len( ) ) ;

      cout << *(char *) sort.resultOther << " "
           << outField.str( ) << endl ;
   }
   sort.free( ) ;
   cb.initUndo( ) ;
}
