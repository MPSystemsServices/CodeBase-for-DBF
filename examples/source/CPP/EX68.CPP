//ex68.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 info( settings, "INFO" ) ; // automatically open data & index file
   Tag4 firstTag ;
   firstTag.initFirst( info ) ;

   info.select( firstTag ) ; // Select first tag of the first open index

   long count = 0L ;
   for( info.top( ) ;! info.eof( ) ; info.skip( ) )
      count++ ;

   cout << count << " records in tag " ;
   cout << firstTag.alias( ) << endl ;

   info.close( ) ;
   settings.initUndo( ) ;
}
