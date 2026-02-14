/***********************************************************************\
*                                                                       *
*   COPYDAT2.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 5 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main(int argc,char *argv[])
{
   if(argc != 3)
   {
      printf("USAGE:COPYDATA <FROMFILE> <TOFILE>\n");
      exit(1);
   }

   Code4      codeBase;

   codeBase.safety = 0;
   Data4 dataFile( codeBase, argv[1] ) ;
   codeBase.exitTest( ) ;

   Field4info fields( dataFile ) ;  //copy the fields

   //obtain the Index4 object of the production index if one exists
   Index4 index = dataFile.index(dataFile.alias( ) ) ;
   Data4 dataCopy ;

   if( index.isValid( ) )
   {
      Tag4info tags( index ) ;
      dataCopy.create( codeBase, argv[2], fields.fields( ), tags.tags( ) ) ;
      tags.free( ) ;
   }
   else
      dataCopy.create(codeBase, argv[2], fields.fields( ) ) ;

   codeBase.closeAll( ) ;
   fields.free( ) ;
   codeBase.initUndo( ) ;
}
