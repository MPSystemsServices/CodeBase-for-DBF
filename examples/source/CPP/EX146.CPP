//ex146.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Field4 name( data, "NAME" ) ;

   Sort4 dbSort( cb, name.len( ), data.recWidth( ) + 1 ) ;

   for( int rc = data.top( ); rc == r4success; rc = data.skip( ) )
      dbSort.put( name.ptr( ), data.record( ), data.recNo( ) ) ;

   data.close( ) ; // database stored in dbSort.
   dbSort.getInit( ) ; // no more items to add.

   cout << "Database sorted on NAME: " << endl ;
   while( dbSort.get( ) == 0 )
      cout << "Record # " << dbSort.resultRec
           << ": " << (char *) dbSort.resultOther << endl ;

   dbSort.free( ) ;
   cb.initUndo( ) ;
}
