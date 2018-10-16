/***********************************************************************\
*                                                                       *
*   COPYDATA.CPP    Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

void main(int argc,char *argv[])
{
   if(argc != 3)
   {
      cout << "USAGE: COPYDATA <FROM FILE>  <TO FILE>" << endl ;
      exit(1);
   }
   Code4 codeBase;
   Data4 dataFile( codeBase, argv[1] ), dataCopy ;
   codeBase.exitTest( ) ;

   Field4info fields( dataFile ) ;
   codeBase.safety = 0;
   dataCopy.create( codeBase,argv[2], fields.fields( )) ;
   fields.free( ) ;

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
