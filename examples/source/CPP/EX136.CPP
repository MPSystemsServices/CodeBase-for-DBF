//ex136.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int addLotsOfRecords( Data4 d )
{
   // get the secondary index file
   Index4 index = d.index( "INFO2" ) ;
   if( index.isValid( ) )
      index.close( ) ;

   d.top( ) ;
   for( int i = 200 ; i ; i -- )
   {
      // make 200 copies of record 1
      d.appendStart() ;
      d.append( ) ; 
   } 

   // open the index file and update it
   index.open( d, d.alias( ) ) ;
   return index.reindex( ) ;
}

void main( )
{
   Code4 cb ;
   cb.autoOpen = 0 ;
   Data4 data( cb, "INFO" ) ;
   Index4 index;
   index.open( data, "INFO2" ) ;
   addLotsOfRecords( data ) ;
   cb.initUndo( ) ;
}
