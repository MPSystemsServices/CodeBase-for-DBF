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

/* o4opt.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* these defines must be here even if no optimization for documentation purposes */

#define OPT4AVAIL -1
#define OPT4NONE   0
#define OPT4DBF    1
#define OPT4INDEX  2
#define OPT4OTHER  3
// AS Jan 16/03 - Modified file type to indicate a user type so that it would become encrypted
#define OPT4USER   4

#ifndef S4OFF_OPTIMIZE

#define OPT4DBF_LO_MIN_LINK     .05
#define OPT4DBF_LO_MAX_TIME     .25
#define OPT4DBF_LO_MIN_TIME     .25
#define OPT4DBF_HI_MIN_LINK     .1
#define OPT4DBF_HI_MAX_TIME    1.0
#define OPT4DBF_HI_MIN_TIME    1.0
#define OPT4INDEX_LO_MIN_LINK   .05
#define OPT4INDEX_LO_MAX_TIME   .25
#define OPT4INDEX_LO_MIN_TIME   .25
#define OPT4INDEX_HI_MIN_LINK   .15
#define OPT4INDEX_HI_MAX_TIME  2.0
#define OPT4INDEX_HI_MIN_TIME  2.0
#define OPT4OTHER_MIN_LINK      .05
#define OPT4OTHER_MAX_TIME      .1
#define OPT4OTHER_MIN_TIME      .1

/* OPT4CHECK_RATE must be a minimum of 5 in order to optimize correctly */
#define OPT4CHECK_RATE        15

/* factor of 2 that estimates the hash distribution, #slots = between #blocks * OPT4BLOCK_DENSITY and #blocks * OPT4BLOCK_DENSITY * 2 */
#define OPT4BLOCK_DENSITY 2L

/* this structure must match OPT4BLOCK with opt4=file followed by pos=pos */
typedef struct
{
   FILE4 S4PTR *file ;
   // AS Oct 21/04 - support for large file optimization
   FILE4LONG pos ;
} OPT4CMP ;

#ifdef __cplusplus
   extern "C" {
#endif

S4EXPORT int S4FUNCTION c4calcType( unsigned long ) ;

int d4updatePrio( CODE4 * ) ;
void file4setWriteOpt( FILE4 *, int ) ;
#ifdef S4ADVANCE_READ
   void opt4fileAdvanceRead( FILE4 *, long, unsigned ) ;
#endif

int opt4blockClear( OPT4BLOCK * ) ;
int opt4blockRemove( OPT4 *, OPT4BLOCK *, int ) ;
// AS Nov 28/02 - need to extend out an existing block in some cases to contain more data if
// file4lenSet() is called.  We don't actually need to assign any data in that case, since
// at the low-level file we never actually are writing anything out.
void opt4fileExtend( FILE4 *file, const FILE4LONG newLen, const FILE4LONG startPos ) ;

int opt4fileDelete( FILE4 *, const FILE4LONG, const FILE4LONG ) ;
int opt4fileFlushList( OPT4 *, FILE4 *, LIST4 *, int ) ;
// AS Apr 13/04 - support for optimizing loarge files
long opt4fileHash( OPT4 *, FILE4 *, FILE4LONG ) ;
// #define opt4fileHash( opt, file, pos )  ((long)( (( (file)->hashInit + (pos) ) >> (opt)->blockPower ) & (opt)->mask ))
void opt4blockLruTop( OPT4BLOCK * ) ;
unsigned opt4fileRead( FILE4 *, FILE4LONG, void *, unsigned ) ;
OPT4BLOCK *opt4fileReturnBlock( FILE4 *, FILE4LONG, long ) ;
int opt4fileWrite( FILE4 *, FILE4LONG, unsigned, const void *, char ) ;

int opt4fileFlush( FILE4 *, const int ) ;
int opt4flushAll( OPT4 *, char ) ;
int opt4flushWriteBuffer( OPT4 * ) ;

#ifdef __cplusplus
   }
#endif

#endif /* S4OFF_OPTIMIZE */
