//ex64.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int SeekSeries( Data4 d, const char *s )
{
   int rc ;
   rc = d.seekNext( s ) ;

   if( rc == r4noTag || rc == r4entry || rc == r4locked || rc < 0 )
      return rc ;

   if( rc == r4after || rc == r4eof )
      rc = d.seek( s ) ;

   return rc ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "PEOPLE" ) ;
   Tag4 nametag( data, "PPL_NAME" ) ;

   data.select( nametag ) ;
   int rc = data.seek( "mickey" ) ;
   for (rc ; rc == r4success ;rc = SeekSeries( data, "mickey" ))
      cout << "found search string " << endl ;
   cb.initUndo( ) ;
}
