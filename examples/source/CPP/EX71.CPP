//ex71.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

long zapLast( Data4 info, long toDelete )
{
   cout << info.alias( ) << " has " << info.recCount( ) << " records." << endl ;

   // Remove the last 'toDelete' records in the data file.
   // endRec parameter defaults to 1 Billion

   info.zap( info.recCount( ) - toDelete + 1L) ;

   cout << endl << info.alias( ) << " now has " << info.recCount( )
        << " records." << endl ;

   return info.recCount( ) ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   zapLast( data, 10L ) ;
   cb.initUndo( ) ;
}
