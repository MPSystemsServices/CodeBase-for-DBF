/***********************************************************************\
*                                                                       *
*   SHOWDATA.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;
#endif

int main(int argc,char *argv[])
{
   if(argc != 2)
   {
      cout << "USAGE: SHOWDATA <FILENAME.DBF>"<< endl;
      exit(0) ;
   }

   Code4 codeBase ;
   Data4 dataFile( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   int numFields = dataFile.numFields( ) ;
   for( int rc = dataFile.top( ); rc == r4success ; rc = dataFile.skip( ))
   {
      for( int j = 1; j <= numFields; j++ )
      {
         Field4memo field( dataFile, j ) ;
         cout << " " << field.str( ) ;
      }
      cout << endl ;
   }
   dataFile.close( ) ;
   codeBase.initUndo( ) ;
   return 0 ;
}
