//ex56.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

long recallAll( Data4 d )
{

   Tag4 saveSelected ;
   saveSelected.initSelected( d ) ;

   d.select( ) ; // use record ordering

   d.lockAll( ) ;

   long count = 0 ;

   for( d.top( ) ;  !d.eof( ) ; d.skip( ) )
   {
      d.recall( ) ;
      count++ ;
   }

   d.select( saveSelected ) ; // reset the selected tag.
   return count ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   recallAll( data ) ;
   cb.initUndo( ) ;
}
