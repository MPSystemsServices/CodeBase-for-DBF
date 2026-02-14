//ex49.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void compressAll( Data4 d )
{
   if( d.pack( ) == 0 )
      cout << "Records marked for deletion are removed." << endl ;

   if( d.memoCompress( ) == 0 )
      cout << "Memo entries are compressed." << endl ;
}

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO") ;

   compressAll( data ) ;
   cb.initUndo( ) ;
}
