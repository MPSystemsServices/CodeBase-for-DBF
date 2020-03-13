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

/* m4check.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef S4STAND_ALONE

/* not supported for FoxPro FPT memo files or dBase III and Clipper memo files */
#ifndef S4MFOX
#ifndef S4MNDX

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4MEMO_OFF
int S4FUNCTION f4memoCheck( MEMO4FILE *f4memo, DATA4 *data )
{
   F4FLAG flags ;
   DATA4FILE *d4 ;
   int  rc, iField ;
   long numBlocks, memoId ;
   MEMO4CHAIN_ENTRY cur ;

   d4 = f4memo->data ;
   #ifndef S4OFF_MULTI
      rc = d4lockFile( data ) ;
      if ( rc != 0 )
         return rc ;
   #endif

   #ifndef S4OFF_WRITE
      if ( ( rc = d4update( data ) ) != 0 )
         return rc ;
   #endif

   if ( f4memo->blockSize == 0 ) // invalid
      return -1 ;

   if ( f4flagInit( &flags, d4->c4, file4longGetLo( file4lenLow( &f4memo->file ) ) / f4memo->blockSize, 0 ) < 0 )
      return error4stack( d4->c4, e4memory, E95206 ) ;

   /* Set flags for the data file entries */
   for ( d4top( data ) ; ! d4eof( data ) ; d4skip( data, 1L ) )
   {
      for ( iField = 0 ; iField < d4->nFieldsMemo ; iField++ )
      {
         numBlocks = (long)((long)f4memoLen(data->fieldsMemo[iField].field) + f4memo->blockSize - 1) / f4memo->blockSize ;
         memoId = f4long( data->fieldsMemo[iField].field ) ;

         if ( f4flagIsAnySet( &flags, memoId, numBlocks ) )
            return error4( d4->c4, e4info, E85204 ) ;
         if ( f4flagSetRange( &flags, memoId, numBlocks ) < 0 )
            return -1 ;
      }
   }

   /* Set flags for the free chain */
   memset( (void *)&cur, 0, sizeof(cur) ) ;
   memo4fileChainSkip( f4memo, &cur ) ;  /* Read in root */

   for ( memo4fileChainSkip( f4memo, &cur ) ; cur.next != -1 ; memo4fileChainSkip( f4memo, &cur ) )
   {
      rc = f4flagIsAnySet( &flags, cur.blockNo, cur.num) ;
      if ( rc < 0 )
         return rc ;
      if ( rc )
         return error4( d4->c4, e4info, E85204 ) ;
      if ( f4flagSetRange( &flags, cur.blockNo, cur.num ) < 0 )
         return -1 ;
   }

   rc = f4flagIsAllSet( &flags, 1, file4longGetLo( file4lenLow( &f4memo->file ) ) / f4memo->blockSize-1 ) ;
   u4free( flags.flags ) ;
   if ( rc == 0 )
      return error4( d4->c4, e4result, E85205 ) ;
   if ( rc < 0 )
      return -1 ;

   return 0 ;
}
#endif /*  ifndef S4MEMO_OFF */

#endif  /*   ifndef S4MFOX   */
#endif  /*   ifndef S4MNDX   */

#endif /* S4STAND_ALONE */
