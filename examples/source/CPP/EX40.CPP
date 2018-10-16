//ex40.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 settings ;
   Data4 infoFile( settings, "INFO" ) ;

   // Go to the end of file and set the End of file flag
   infoFile.goEof( ) ;

   // Check to see if the end of file flag is set
   if( infoFile.eof( )  )
   {
      cout << "This is always true" << endl ;
      infoFile.bottom( ) ; // reset the eof flag
   }

   infoFile.close( ) ;
   settings.initUndo( ) ;
}
