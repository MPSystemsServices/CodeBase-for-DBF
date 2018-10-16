#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 info( settings, "INFO" ) ;

   // List the names of the tags in any production index file corresponding
   // to "INFO" 

   cout << "Production index file tags for data file: " ;
   cout << info.alias( ) << endl ;

   Tag4 tag ; 
   for( tag.initFirst( info ); tag.isValid( ); tag.initNext( ))
      cout << "Tag name: " << tag.alias( ) << endl ;

   settings.initUndo( ) ;
}
