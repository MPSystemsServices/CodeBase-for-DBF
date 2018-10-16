//ex13.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

int retry(  )
{
   char rc ;
   cout << "Record locked by another user." << endl ;
   cout << "Retry? (Y or N)" ;
   cin >> rc ;
   if( rc == 'N' ) return 0 ;
   return 1 ;
}

int lockARecord( Data4 d, long rec )
{
   int rc ;
   while( ((rc = d.go( rec )) == r4locked) && (retry( ) == 1) ) ;

   if( rc == r4locked ) return 0 ;

   return 1 ;
}

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   lockARecord( data, 3L ) ;

   // other code

   cb.initUndo( ) ;
}
