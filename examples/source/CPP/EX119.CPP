//ex119.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void displayLength( File4 &file )
{
   if( ! file.isValid( ) )
      return ;

   cout << "File name: " << file.fileName( ) << endl
        << "Length: " << file.len( ) << endl ;

   return ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "TEXT.FIL" ) ;
   displayLength( file ) ;
   cb.initUndo( ) ;
}
