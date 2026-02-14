//ex41.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 settings ;
   Data4 infoFile( settings, "INFO" );

   for( int num = infoFile.numFields( ) ; num ; num -- )
   {
      Field4 field( infoFile, num ) ;
      if( num == infoFile.fieldNumber( field.name( )) )
         cout << "This is always true." << endl ;
   }
   infoFile.close( ) ;
   settings.initUndo( ) ;
}
