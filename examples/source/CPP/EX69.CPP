//ex69.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Tag4 nameTag( data, "INF_NAME" ) ;
   Field4 name( data, "NAME" ) ;

   cb.exitTest( ) ; // check for errors
   data.lockAll( ) ;
   data.select( nameTag ) ;

   for( int rc = data.seek( "JONES" ) ; rc == 0 ; rc = data.skip( ) )
   {
      if( memcmp( name.ptr( ), "JONES", 5 ) == 0 )
         cout << "Jones in record " << data.recNo( ) << endl ;
      else
         break ;
   }
   data.unlock( ) ;

   data.close( ) ;
   cb.initUndo( ) ;
}
