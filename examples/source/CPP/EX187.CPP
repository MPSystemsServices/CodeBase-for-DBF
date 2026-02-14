//ex187.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int searchAll( Data4 d, char *value )
{
   Tag4 origTag, tag ;
   origTag.initSelected( d ) ;  // Save the current tag
   long origRecNo = d.recNo( ) ;

   for( tag.initLast( d ) ; tag.isValid( ); tag.initPrev( ) )
   {
      d.select( tag ) ;
      if( d.seek( value ) == 0 )
      {
         d.select( origTag ) ;
         return d.recNo( ) ;
      }
   }
   d.select( origTag ) ;
   d.go( origRecNo ) ;
   return -1 ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   data.top( ) ;
   int rc = searchAll( data, "Abbot" ) ;
   cout << data.record( ) << endl ;
   cout << rc << " is the record number" << endl ;
   cb.initUndo( ) ;
}
