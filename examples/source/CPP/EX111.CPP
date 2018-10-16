// ex111.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA3" ) ;
   Field4memo comments( data, "COMMENTS" ) ;

   cb.exitTest( ) ;
   long count = 0 ;
   for( data.top( ) ; !data.eof( ) ; data.skip( ) )
      if( comments.len( ) )
         count ++ ;

   cout << "There were " << count << " memo entries out of "
        << data.recCount( ) << " records." << endl ;
   cb.initUndo( ) ;
}
