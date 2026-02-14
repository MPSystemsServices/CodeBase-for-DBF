//ex125.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;

   Str4large before, after ;
   before.setLen( 5 ) ; after.setLen( 5 ) ;

   file.optimize( 1, OPT4OTHER) ;

   // read the first 5 bytes and buffer it.
   file.read( 0L, before ) ;
   file.read( 0L, after ) ; // read from memory, not disk

   if( before == after )
      cout << "This will always be true, since the file read was from memory." << endl ;

   cout << "Press ENTER to re-read information." << endl ;
   getchar();

   file.refresh( ) ; // next read will be from disk
   file.read( 0L, after ) ; // read from disk

   if( before == after )
      cout << "No changes detected." << endl;
   else
      cout << "Good thing it was read from disk..."
           << " someone has changed it. " << endl ;

   file.close( ) ;
   cb.initUndo( ) ;
}
