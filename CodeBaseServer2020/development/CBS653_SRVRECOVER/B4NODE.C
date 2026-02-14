/* b4node.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TURBOC__ */

#if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
   #ifndef S4CLIPPER
      void b4nodeGetFilePosition( INDEX4FILE *i4, const B4NODE node, FILE4LONG *outPos )
      {
         file4longAssign( *outPos, b4node( node ), 0 ) ;
         file4longMultiply( *outPos, i4multiplier( i4 ) ) ;
      }
   #endif /* S4CLIPPER */



   #ifndef S4CLIPPER
      void b4nodeSetFromFilePosition( INDEX4FILE *i4, B4NODE *node, FILE4LONG filePos )
      {
         assert5( i4multiplier( i4 ) != 0 ) ;
         file4longDivide( filePos, i4multiplier( i4 ) ) ;
         assert5( file4longGetHi( filePos ) == 0 ) ;
         b4node( *node ) = file4longGetLo( filePos ) ;
      }
   #endif /* S4CLIPPER */



   #ifdef S4FOX
      void b4nodeSubtractBlocks( B4NODE *node, INDEX4FILE *i4, int numBlocksToSubtract )
      {
         b4node( *node ) -= ( numBlocksToSubtract * i4blockSize( i4 ) / i4multiplier( i4 )  ) ;
      }
   #endif /* S4FOX */



   #ifndef S4CLIPPER
      void b4nodeAddBlocks( B4NODE *node, INDEX4FILE *i4, int numBlocksToAdd )
      {
         b4node( *node ) += ( numBlocksToAdd * i4blockSize( i4 ) / i4multiplier( i4 ) ) ;
      }
   #endif /* S4CLIPPER */



   #ifdef S4FOX
      void b4getFilePosition( B4BLOCK *b4, FILE4LONG *outPos )
      {
         b4nodeGetFilePosition( b4->tag->indexFile, b4->fileBlock, outPos ) ;
      }
   #endif /* S4FOX */
#endif /* ! S4CLIENT && !OFF_INDEX */
