//ex99.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void dumpDataFileToScreen( Data4 d )
{
   cout << "Contents of: " << d.alias( ) << endl ;
   for( d.top( ) ; !d.eof( ) ; d.skip( ) )
   {
      for( int j = 1 ; j <=d.numFields( ) ; j++ )
      {
         Field4 field( d, j ) ;
         cout << field.str( ) ;
      }
      cout << endl ;
   }
}

void main()
{
   Code4 cb;
   Data4 data(cb, "DATA");

   dumpDataFileToScreen( data);

   data.close();
   cb.initUndo();
}
