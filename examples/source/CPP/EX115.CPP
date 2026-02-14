//ex115.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 file ;
   char readInfo[50] ;

   cb.safety = 0 ;
   file.create( cb, "TEXT.FIL", 0 ) ;
   if( ! file.isValid( ) )
   {
      cb.initUndo( ) ;
      cb.exit( ) ;
   }

   file.write( 0, "Some File Information", 21 ) ;
   unsigned lenRead = file.read( 10, readInfo, sizeof( readInfo) ) ;

   if( memcmp(readInfo, "Information" , lenRead ) == 0 )
      cout << "This is always true" << endl ;

   if( lenRead == 11 )
      cout << "This is always true, too" << endl ;

   file.close( ) ;
   cb.initUndo( ) ;
}
