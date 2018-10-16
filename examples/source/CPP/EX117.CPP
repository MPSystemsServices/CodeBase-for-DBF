#include "d4all.hpp"
extern unsigned _stklen = 10000 ;  // for all Borland compilers

void main( ) 
{
   Code4 cb ;
   File4 autoexec( cb, "C:\\AUTOEXEC.BAT" ) ;

   autoexec.lock( 0, autoexec.len( ) ) ; // lock the entire file

   // ... some other code
   autoexec.close( ) ; // save changes and close the file
   cb.initUndo( ) ;
}   
