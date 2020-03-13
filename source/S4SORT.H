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

/* s4sort.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

struct RELATE4St ;

typedef struct
{
   char  S4PTR *ptr ;       /* Pointer to the starting memory */
   unsigned  pos ;    /* The current position withing 'ptr' */
   unsigned  len ;    /* The current data length pointed to by 'ptr' */

   // AS 10/16/00 - support for large files - change from S4LONG to FILE4LONG
   FILE4LONG   disk ;      /* Current Disk Position, Offset from start of 'spool' */
                      /* >= 'spoolBytes' means nothing is on disk. */
   int    spoolI ;   /* The spool number of this spool  */
} S4SPOOL ;

#ifdef S4CBPP
class S4CLASS SORT4
{
public:
#else
typedef struct
{
#endif
   CODE4 S4PTR *codeBase ;

   FILE4        file ;
/*   char          fileNameBuf[14] ;*/
   FILE4SEQ_WRITE   seqwrite ;
   char         S4PTR *seqwriteBuffer ;
   S4SPOOL S4PTR *spoolPointer ;
   unsigned int spoolsN ;      /* The current # of "spools" */
   unsigned int spoolsMax ;    /* The # of "spools" memory is allocated for */

   char S4PTR * S4PTR *pointers ;
   unsigned int pointersI ;    /* The next pointers to return using 'get'. */
   unsigned int pointersUsed ; /* The number of pointers assigned so far. */
   unsigned int pointersInit ; /* The number of pointers initialied so far. */
   unsigned int pointersMax ;  /* The number of pointers for which memory */
                         /* has been allocated. */

   LIST4 pool ;      /* A number of memory pools for sorting */
   MEM4 S4PTR *poolMemory ;
   unsigned int poolN ;        /* The number of pools */
   unsigned int poolEntries ;  /* Number of record entries in each pool */
   unsigned int sortLen ;

   S4LONG     spoolDiskLen ; /* The # of bytes in each disk spool */
   unsigned spoolMemLen ;  /* The # of bytes in each memory spool */

   unsigned int infoOffset ;   /* The spot to put the corresponding info. */
   unsigned int infoLen ;
   unsigned int totLen ;       /* The total length of each pool entry. */

   S4LONG   isMemAvail ;
   S4CMP_FUNCTION *cmp ;
#ifdef S4CBPP
} ;
#else
} SORT4 ;
#endif

/* The 'len' is passed to the compare function as its third parameter */
/* The compare function returns '(int) 1' iff 'v1>v2'. */
/* s4quick also assumes that there is a record number after the sort data */

#ifdef __cplusplus
   extern "C" {
#endif

int   s4quick( void S4PTR * S4PTR *, const int, S4CMP_FUNCTION *, const int ) ;
void  s4deleteSpoolEntry( SORT4 S4PTR * ) ;
int   s4flush( SORT4 S4PTR * ) ;   /* Flushes current entries to disk */
int   sort4getPtrPtr( SORT4 S4PTR *, char S4PTR * S4PTR * ) ;
void  sort4initPointers( SORT4 S4PTR *, char S4PTR *, unsigned ) ;
long  s4needed(SORT4 S4PTR *) ; /* Returns an estimate on the amount of memory needed */
int   s4nextSpoolEntry(SORT4 S4PTR *) ;

S4EXPORT int   S4FUNCTION sort4free( SORT4 S4PTR * ) ;    /* Frees any allocated memory and uninitializes */
S4EXPORT int   S4FUNCTION sort4get( SORT4 S4PTR *,  S4LONG S4PTR *, void S4PTR * S4PTR *, void S4PTR * S4PTR * ) ;
S4EXPORT int   S4FUNCTION sort4getInit( SORT4 S4PTR * ) ;
S4EXPORT int   S4FUNCTION sort4getInitFree( SORT4 *, struct RELATE4St * ) ;
S4EXPORT int   S4FUNCTION sort4getMemInit( SORT4 * ) ;
S4EXPORT int   S4FUNCTION sort4init( SORT4 S4PTR *, CODE4 S4PTR *, const int, const int ) ;
S4EXPORT int   S4FUNCTION sort4initAlloc( SORT4 S4PTR * ) ;
S4EXPORT int   S4FUNCTION sort4initFree( SORT4 S4PTR *, CODE4 S4PTR *, const int, const int, struct RELATE4St * ) ;
S4EXPORT int   S4FUNCTION sort4initSet( SORT4 S4PTR *, CODE4 S4PTR *, const int, const int ) ;
S4EXPORT int   S4FUNCTION sort4put( SORT4 S4PTR *, const S4LONG, const void S4PTR *, const void S4PTR * ) ;
S4EXPORT int   S4FUNCTION sort4spoolsInit( SORT4 S4PTR *, const int ) ;

S4EXPORT int S4FUNCTION sort4alloc( SORT4 S4PTR * S4PTR * ) ;
S4EXPORT void S4FUNCTION sort4assignCmp2( SORT4 S4PTR *, S4CMP_FUNCTION ) ;
S4EXPORT void S4FUNCTION sort4free2( SORT4 S4PTR * S4PTR * ) ;

#ifdef __cplusplus
   }
#endif
