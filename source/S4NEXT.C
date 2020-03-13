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

/* s4next.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

int s4nextSpoolEntry( SORT4 *s4 )
{
   // as 10/16/00 - support for large files
   FILE4LONG lastDiskPos, diskDataLeft ;
   S4LONG newRec, spoolRec ;
   unsigned lenRead, maxRead ;
   FILE4LONG filePos ;
   int low, high, pos, rc ;
   char *newData ;
   S4SPOOL saveSpool ;

   #ifdef E4PARM_LOW
      if ( s4 == 0 )
         return error4( 0, e4parm_null, E91908 ) ;
   #endif

   s4->spoolPointer->pos += s4->totLen ;
   if ( s4->spoolPointer->pos >= s4->spoolPointer->len )
   {
      s4->spoolPointer->pos = 0 ;
      // AS 10/25/00 - if S4FILE_EXTENDED not used, then the if below failed when value was ULONG_MAX (i.e. error)
      // if ( file4longGreaterEqZero( s4->spoolPointer->disk ) )

      // just need to know if not error
      if ( file4longError( s4->spoolPointer->disk ) != ULONG_MAX )
      {
         /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
         file4longAssign( lastDiskPos, s4->spoolDiskLen, 0L ) ;
         file4longMultiply( lastDiskPos, (s4->spoolPointer->spoolI + 1 ) );
         file4longAssignLong( diskDataLeft, lastDiskPos ) ;
         file4longSubtractLong( &diskDataLeft, &s4->spoolPointer->disk ) ;
         maxRead = s4->spoolMemLen ;
         if ( file4longLess( diskDataLeft, (S4UNSIGNED_LONG)s4->spoolMemLen ) )
         {
            assert5( file4longGetHi( diskDataLeft ) == 0 ) ;
            maxRead = file4longGetLo( diskDataLeft ) ;
         }
         file4longAssignLong( filePos, s4->spoolPointer->disk ) ;
         lenRead = file4readInternal( &s4->file, filePos, s4->spoolPointer->ptr, maxRead ) ;

         if ( error4code( s4->codeBase ) < 0 )
         {
            sort4free( s4 ) ;
            return error4stack( s4->codeBase, (short)error4code( s4->codeBase ), E91908 ) ;
         }

         s4->spoolPointer->len = lenRead ;
         file4longAdd( &s4->spoolPointer->disk, lenRead ) ;
         if ( lenRead != maxRead || lenRead == 0 )
         {
            if ( lenRead % s4->totLen )
            {
               sort4free( s4 ) ;
               return error4describe( s4->codeBase, e4read, E91908, s4->file.name, 0, 0 ) ;
            }
            file4longAssignError( s4->spoolPointer->disk ) ;
            if ( lenRead == 0 )
            {
               s4deleteSpoolEntry( s4 ) ;
               return 0 ;
            }
            else
               s4->spoolPointer->len = lenRead ;
         }
         else  /* Check if we are out of disk entries for the spool */
         {
            if ( file4longGreaterEqLong( s4->spoolPointer->disk, lastDiskPos ) )
               file4longAssignError( s4->spoolPointer->disk ) ;
         }
      }
      else
      {
         s4deleteSpoolEntry(s4) ;
         return 0 ;
      }
   }

   /* Position the new entry to the sorted location using a binary search */
   /* New entry is placed before 'result':  int pos >= 1  when complete */
   low = 1 ;
   high = s4->spoolsN ;
   newData = s4->spoolPointer->ptr + s4->spoolPointer->pos ;
   c4memcpy( (void *)&newRec, newData + s4->sortLen, sizeof(newRec) ) ;

   for(;;)
   {
      pos = ( low + high ) / 2 ;
      if ( pos == low && pos == high )  /* then found */
      {
         c4memcpy( (void *)&saveSpool, (void *)s4->spoolPointer, sizeof(S4SPOOL) ) ;
         c4memmove( s4->spoolPointer, s4->spoolPointer+1, sizeof(S4SPOOL)*(pos-1) ) ;
         c4memcpy( (void *)( s4->spoolPointer + pos - 1 ), (void *)&saveSpool, sizeof(S4SPOOL) ) ;
         return 0 ;
      }
      rc = (*s4->cmp)(newData, s4->spoolPointer[pos].ptr + s4->spoolPointer[pos].pos, s4->sortLen) ;
      if ( rc == 0 )
      {
         c4memcpy( (void *)&spoolRec, s4->spoolPointer[pos].ptr + s4->spoolPointer[pos].pos + s4->sortLen, sizeof(spoolRec) ) ;
         if ( newRec > spoolRec )
            rc = 1 ;
      }

      if ( rc > 0 )
         low = pos+1 ;
      else
         high = pos ;
      #ifdef E4ANALYZE
         if ( high < low )
            return error4( s4->codeBase, e4result, E91908 ) ;
      #endif
   }
}

