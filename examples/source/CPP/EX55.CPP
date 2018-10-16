//ex55.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Tag4 firstTag ;
   firstTag.initFirst( data ) ;

   cb.exitTest( ) ;

   data.select( firstTag ) ; // select the first tag of the first open index.
   data.position( .25 ) ; // move one quarter through the index file.
   cout << "Record number: " << data.recNo() << endl ;

   cout << "The current position is: " << data.position( ) << endl ;

   cb.initUndo( ) ;
}
