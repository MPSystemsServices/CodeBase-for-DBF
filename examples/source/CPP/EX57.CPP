//ex57.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

long recsInFile( Code4 &cb, Str4ten &fileName )
{
   Data4 data( cb, fileName.ptr( ) ) ;
   if( cb.errorCode ) return -1L ; // an error occurred

   long count = data.recCount( ) ; // save the record count

   data.close( ) ;               // close the data file
   return count ;
}

void main( )
{
   Code4 cb ;
   Str4ten name( "INFO" ) ;
   long rc = recsInFile( cb, name ) ;
   cout << rc << " records in the file" << endl ;
   cb.initUndo( ) ;
}
