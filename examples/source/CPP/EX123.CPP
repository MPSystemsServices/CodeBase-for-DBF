//ex123.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void peek20( Code4 &cb, File4 &file )
{
   char buf[11] ;
   memset( buf, 0, sizeof( buf ) ) ; // ensure null termination for cout

   int pos = file.read( 0L, buf, sizeof( buf ) - 1 ) ;
   if( cb.errorCode < 0 ) return ;

   Str4ten buf2 ;
   buf2.setLen( 10 ) ;
   buf2.set( 0 ) ;

   if( pos )
      file.read( long (sizeof( buf ) - 1), buf2 ) ;
   cout << buf << buf2.ptr( ) << endl ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;
   peek20( cb, file ) ;
   cb.initUndo( ) ;
}
