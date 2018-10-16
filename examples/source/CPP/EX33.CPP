//ex33.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ;

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO" );
   Field4 field( data, 1 ) ;

   // output the first field of every record in reverse sequential order.
   for( data.bottom() ; !data.bof( ) ; data.skip( -1 ) )
      cout << field.str() << endl ;
   data.close( ) ;
   cb.initUndo( ) ;
}
