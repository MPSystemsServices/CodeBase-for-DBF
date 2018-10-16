//ex62.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;
   Data4 data( cb, "INFO" ) ;

   cout << "Reindexing " << data.alias( ) << endl <<"Please Wait" ;

   int rc = data.reindex();

   if( rc != 0 )
      cout << endl << "Reindex NOT successful." << endl ;
   else
      cout << endl << "Successfully reindexed." << endl ;

   cb.initUndo( ) ;
}
