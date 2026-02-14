/***********************************************************************\
*                                                                       *
*   SHOWDAT2.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

Code4    codeBase ;
Data4    dataFile ;
Tag4     tag ;
void printRecords( void )
{
   for(int rc = dataFile.top( ); rc == r4success
                       ; rc = dataFile.skip( ))
   {
      for(int j = 1;j <= dataFile.numFields( ); j ++)
      {
         Field4memo field( dataFile, j ) ;
         cout << field.str( ) ;
      }
      cout << endl ;
   }
   cout << endl ;
}

void main( int argc, char *argv[] )
{
   if (argc != 2)
      exit(0);

   dataFile.open( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   cout << "Data File " << argv[1] << " in Natural Order" << endl ;
   printRecords( ) ;

   for( tag.initFirst( dataFile ); tag.isValid( )
                ; tag.initNext( ))
   {
      cout << "Press ENTER to continue:" << endl ;
      getchar( );

      cout << endl << "Data File " << argv[1] << " sorted by Tag "
                   << tag.alias( ) << endl ;
      dataFile.select( tag ) ;
      printRecords( ) ;
   }

   dataFile.close( ) ;
   codeBase.initUndo( ) ;
}
