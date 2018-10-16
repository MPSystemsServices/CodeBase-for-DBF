//ex103.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

char *createBufCopy( Field4 f )
{
   char *buf = (char *)u4alloc( f.len( ) + 1 ) ;
   memcpy( buf, f.ptr( ), f.len( ) );
   buf[f.len()] = 0;
   return buf ;
}

void main( )
{
   Code4 cb ;
   cb.readOnly = 1 ;
   Data4 data( cb, "INFO" ) ;
   Field4 field( data, "NAME" ) ;
   char *buffer ;

   data.top( ) ;
   buffer = createBufCopy( field ) ;
   cout << buffer << " is a copy of the field " << endl ;
   u4free( buffer ) ;
   cb.initUndo( ) ;
}
