/***********************************************************************\
*                                                                       *
*   NOGROUP2.CPP  Copyright (C) 1999 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000; // for all Borland compilers
#endif

Code4 codeBase ;
Data4 dataFile ;
Tag4 nameTag, ageTag, idTag;

void printRecords( void )
{

}

void main( void )
{
   #if !defined(S4CLIPPER) && !defined(S4CLIENT)
      fprintf(stderr,"This example is only for Clipper.\n");
   #else
      #ifdef S4CLIENT
         codeBase.connect(0,0,0,0,0);
      #endif

      if (strcmp(codeBase.indexExtension(),"NTX") == 0)
      {
         codeBase.autoOpen = 0;
         codeBase.safety = 0;

         dataFile.open( codeBase, "STUDENT.DBF" ) ;

         nameTag.open( dataFile, "STU_NAME" ) ;
         ageTag.open( dataFile, "STU_AGE" ) ;
         idTag.open( dataFile, "STU_ID" ) ;

         dataFile.select( nameTag ) ;
         printRecords();

         codeBase.closeAll( ) ;
      }
      else
         fprintf(stderr,"This example is only for Clipper.\n");
   #endif

   codeBase.initUndo( ) ;
}
