//ex58.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Tag4 firstTag ;
   firstTag.initFirst( data ) ;

   data.select( firstTag  ) ; //select the first tag of the first opened index

   long count = 0 ;
   for( data.top( ); !data.eof( ) ; data.skip( ) )
   {
      cout << "position in the tag: " << ++count ;
      cout << "      Record Position: " << data.recNo( ) << endl ;
   }
   cb.initUndo( ) ;
}
