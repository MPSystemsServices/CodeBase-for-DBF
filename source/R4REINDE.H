/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* r4reindex.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef S4CLIENT
typedef struct
{
   /* must be before nKeys  */
   #ifdef S4CLIPPER
      short *blockIndex ;
      char *data;
   #endif

   #ifdef S4FOX
      B4STD_HEADER header ;
   #else
      short nKeys ;
   #endif

   #ifdef S4MDX
      char dummy[6] ;
      char info[2] ;
   #endif
} R4BLOCK_DATA ;

typedef struct  /* R4REINDEX  For 'i4reindex' */
{
   #ifndef S4CLIPPER
      short nTags ;
      INDEX4FILE *indexFile ;
   #endif

   TAG4FILE *tag ;

   #ifdef S4FOX
      char *curPos ;
      B4NODE_HEADER nodeHdr ;
      int nBlocksUsed ;
   #else
      #ifdef S4CLIPPER
         B4KEY_DATA *stranded ;
         int keysHalf ;
         unsigned long lastblockInc ;        /* How much to increment 'lastblock' by */
         unsigned long nBlocksUsed ;
      #else
         unsigned long lastblockInc ;        /* How much to increment 'lastblock' by */
         unsigned blocklen ;
      #endif
   #endif

   unsigned int grouplen ;             /* Entire key length */
   int valuelen ;             /* Length of sorted portion of key */
   int keysmax ;
   B4NODE lastblock ;            /* The block last written to */
   S4LONG keyCount ;

   SORT4 sort ;

   FILE4SEQ_WRITE seqwrite ;
   char *buffer ;      /* Buffer for 'seqwrite' and 'seqread' */
   unsigned int bufferLen ;

   R4BLOCK_DATA *startBlock ;
   int nBlocks ;
   int minKeysmax;  /* To calculate the number of blocks */

   DATA4 *data ;
   DATA4FILE *dataFile ;
   CODE4 *codeBase ;
} R4REINDEX ;

#ifdef __cplusplus
   extern "C" {
#endif

void r4reindexFree( R4REINDEX * ) ;

#ifdef S4CLIPPER
   int r4reindexTagHeadersCalc( R4REINDEX *, TAG4FILE * ) ;
   int r4reindexSupplyKeys( R4REINDEX *, TAG4FILE * ) ;
   int r4reindexWriteKeys( R4REINDEX *, TAG4FILE *, short int ) ;
   int r4reindexTagHeadersWrite( R4REINDEX *, TAG4FILE * ) ;
   int r4reindexInit( R4REINDEX *, TAG4 * ) ;
#else
   int r4reindexTagHeadersCalc( R4REINDEX * ) ;
   int r4reindexSupplyKeys( R4REINDEX * ) ;
   int r4reindexWriteKeys( R4REINDEX *, short int ) ;
   int r4reindexTagHeadersWrite( R4REINDEX * ) ;
   int r4reindexInit( R4REINDEX *, INDEX4 *, INDEX4FILE * ) ;
#endif
int r4reindexBlocksAlloc( R4REINDEX * ) ;

#ifndef S4CLIPPER
   TAG4FILE *index4fileFindITag( INDEX4FILE *, const int ) ;
#endif

/* Write key functions */
#ifdef S4FOX
   int r4reindexAdd( R4REINDEX *, const S4LONG, const unsigned char *, const char *, int * ) ;
   int r4reindexFinish( R4REINDEX *, char * ) ;
#else
   int r4reindexAdd( R4REINDEX *, const S4LONG, const unsigned char * ) ;
   int r4reindexFinish( R4REINDEX * ) ;
#endif

#ifdef __cplusplus
   }
#endif

#endif
