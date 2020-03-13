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

/* util5.cpp   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifdef OLEDB5ERROR
   #include "oledb5err.hpp"
#else
   #include "oledb5.hpp"
#endif

#if defined(OLEDB5BUILD) || defined(OLEDB5ERROR)

   extern "C" {

      #ifdef S4MAX_OR_SERVER
         // Returns the size of the memory previously allocated.
         // Note there is an OLE call to return this information.
         // Often the size allocated is greater than the size requested ;
         // Consequently, this function is currently not useful.
         static ULONG alloc5size( void *p )
         {
            assert5( pIMalloc5 != 0 ) ;
            return pIMalloc5->GetSize( p ) ;
         }
      #endif

      // AS 04/29/99 --> added a counting mechanism into the alloc5init/undo to handle multiple calls.
      // this is a superior implementation that avoids memory problems...

      #ifndef S4SERVER
         int g_numAllocers = 0 ;
      #endif

      HRESULT S4FUNCTION alloc5init()
      {
         #ifndef S4SERVER
            if ( pIMalloc5 != 0 )
            {
               g_numAllocers++ ;
               return S_OK;
            }

            HRESULT rc = CoGetMalloc( MEMCTX_TASK, &pIMalloc5 ) ;
            if ( rc != S_OK )
            {
               LPMALLOC pIMalloc5 = 0 ;
               return E_FAIL ;
            }
            g_numAllocers++ ;
         #endif
         return S_OK ;
      }



      #ifdef S4MEM_TEST_OLEDB
         Array5<void> allocArray ;
      #endif


      void S4FUNCTION alloc5initUndo()
      {
         #ifndef S4SERVER
            if ( pIMalloc5 == 0 )
               return ;
            g_numAllocers-- ;
            if ( g_numAllocers != 0 )
               return ;

            #ifdef S4MEM_TEST_OLEDB
               if ( !allocArray.isEmpty() )  // we should have 'free5()'d' everything
                  throw Err5internal( 0 ) ;
               allocArray.free() ;  // for bounds checker, free from executeable space
            #endif
            pIMalloc5->Release() ;
            pIMalloc5 = 0 ;
         #endif
      }



      #ifdef S4MAX_OR_SERVER
         #ifdef S4SEMAPHORE
            extern int memoryInitialized ;
            extern CRITICAL_SECTION critical4memory ;
         #endif
      #endif



      void *S4FUNCTION alloc5( ULONG amount )
      {
         void *p ;

         #ifdef S4SERVER
            p = u4alloc( amount ) ;
         #else
            assert5( pIMalloc5 != 0 ) ;

            p = pIMalloc5->Alloc( amount ) ;
         #endif

         if ( p == 0 )
            #ifndef OLEDB5ERROR
               throw Err5memory() ;
            #else
               return p ;
            #endif

         #ifdef S4MAX_OR_SERVER
            #ifdef S4SEMAPHORE
               mem4start( 0 ) ;
            #endif
            mem4allocated += alloc5size( p ) ;
            #ifdef S4SEMAPHORE
               mem4stop( 0 ) ;
            #endif
         #endif

         #ifdef S4MEM_TEST_OLEDB
            if ( allocArray.find( p ) != -1 )
               #ifndef OLEDB5ERROR
                  throw Err5memory() ;
               #else
                  return p ;
               #endif
            allocArray.add( p ) ;
            if ( allocArray.find( p ) == -1 )
               #ifndef OLEDB5ERROR
                  throw Err5memory() ;
               #else
                  return p ;
               #endif
         #endif

         return p ;
      }



      void *S4FUNCTION alloc5null( ULONG amount )
      {
         void *p ;

         #ifdef S4SERVER
            p = u4alloc( amount ) ;
         #else
            assert5( pIMalloc5 != 0 ) ;

            p = pIMalloc5->Alloc( amount ) ;
         #endif

         if ( p == 0 )
            #ifndef OLEDB5ERROR
               throw Err5memory() ;
            #else
               return p ;
            #endif

         memset( p, 0, amount ) ;

         #ifdef S4MAX_OR_SERVER
            #ifdef S4SEMAPHORE
               mem4start( 0 ) ;
            #endif
            mem4allocated += alloc5size( p ) ;
            #ifdef S4SEMAPHORE
               mem4stop( 0 ) ;
            #endif
         #endif

         #ifdef S4MEM_TEST_OLEDB
            if ( allocArray.find( p ) != -1 )
               #ifndef OLEDB5ERROR
                  throw Err5memory() ;
               #else
                  return p ;
               #endif
            allocArray.add( p ) ;
            if ( allocArray.find( p ) == -1 )
               #ifndef OLEDB5ERROR
                  throw Err5memory() ;
               #else
                  return p ;
               #endif
         #endif

         return p ;
      }



      void S4FUNCTION free5( void *p )
      {
         if ( p == 0 )
            return ;

         #ifdef S4SERVER
            u4free( p ) ;
         #else
            assert5( pIMalloc5 != 0 ) ;
         #endif

         #ifdef S4MEM_TEST_OLEDB
            short findEntry = allocArray.find( p ) ;
            if ( findEntry == -1 )  // wasn't allocated via alloc5
               throw Err5memory () ;
            allocArray.remove( findEntry ) ;
            if ( allocArray.find( p ) != -1 )
               throw Err5memory () ;
         #endif

         #ifdef S4MAX_OR_SERVER
            #ifdef S4SEMAPHORE
               mem4start( 0 ) ;
            #endif
            mem4allocated -= alloc5size( p ) ;
            #ifdef S4SEMAPHORE
               mem4stop( 0 ) ;
            #endif
         #endif

         #ifndef S4SERVER
            pIMalloc5->Free( p ) ;
         #endif
      }
   }
#endif /* OLEDB5BUILD */



// AS 01/21/00 - I think should be and..., else server fails, since based on... #if defined(OLEDB5BUILD) || defined(OLEDB5ERROR)
// #if !defined(OLEDB5BUILD) || !defined(OLEDB5ERROR)
#if !defined(OLEDB5BUILD) && !defined(OLEDB5ERROR)
   // need alloc5/free5 - use system...
   #define alloc5( amt ) malloc( amt )
   #define free5( ptr ) free( ptr )
#endif



// AS NOV 24/05 this is now used in the regular compile...OLEDB5BUILD not required
// moved to u4util.c
// #if defined( E4ANALYZE ) && defined( OLEDB5BUILD )
/*
#if defined( E4ANALYZE )
   void assert5info( char *fName, int lineNo )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "assert5() failure, file:  " ) ;
         log5( fName ) ;
         char lineBuf[11] ;
         memset( lineBuf, 0, sizeof( lineBuf ) ) ;
         log5( "  line #" ) ;
         c4ltoa45( lineNo, lineBuf, 10 ) ;
         log5( lineBuf ) ;
         log5( "\r\n" ) ;
      #endif
      throw Err5internal( 0 ) ;
   }
#endif
*/


extern "C" {
   // outx is an optional paramater
   WSTR5 * S4FUNCTION getUnicodeFromCharPtr5( const char *in, WSTR5 *outx )
   {
      WSTR5 *out ;
      int len ;

      len = strlen( in ) ;
      if ( outx == 0 )
         out = (WSTR5 *)alloc5( (len + 1) * 2 ) ;
      else
         out = outx ;

      #ifdef S4WIN32
         MultiByteToWideChar( CP_OEMCP, 0, in, -1, out, len + 1 ) ;
      #else
         /* LY 2001/09/13 mbsrtowcs( out, &in, len + 1, 0 ) ; */
         c4mbsToWcs( out, in, len + 1 ) ;
      #endif

      return out ;
   }



   // out is an optional paramater
   char *S4FUNCTION getCharPtrFromUnicode5( const WSTR5 *in, char *out )
   {
      char *hold ;
      int len ;

      assert5( in != 0 ) ;

      len = c4wcslen( in ) ;  /* LY 2001/09/13 : changed from wcslen */
      if ( out == 0 )
         hold = (char *)alloc5( (len+1) * 2 ) ;
      else
         hold = out ;

      if ( len != 0 )
      {
         #ifdef S4WIN32
            WideCharToMultiByte( CP_ACP, 0, in, len, hold, len, 0, 0 ) ;  // CS 2002/05/23 change from CP_OEMCP
         #else
            /* LY 2001/09/13 wcsrtombs( hold, &in, len + 1, 0 ) ; */
            c4wcsToMbs( hold, in, len + 1 ) ;
         #endif
      }

      hold[len] = 0 ;

      return hold ;
   }

   char *S4FUNCTION convert5unicodeToChar( const WSTR5 *in, long codePage, int numChars, char *out )
   {
      char *hold ;

      // should already be in a valid codepage value (not cp437, but 437) - called as such from odbc...
      assert5( codePage == 437 || codePage == 1252 ) ;

      assert5( in != 0 ) ;

      if ( out == 0 )
         hold = (char *)alloc5( (numChars+1) * 2 ) ;
      else
         hold = out ;

      if ( numChars != 0 )
      {
         #ifdef S4WIN32
            WideCharToMultiByte( codePage, 0, in, numChars, hold, numChars, 0, 0 ) ;
         #else
            /* LY 2001/09/13 wcsrtombs( hold, &in, numChars, 0 ) ; */
            c4wcsToMbs( hold, in, numChars ) ;
         #endif
      }

      hold[numChars] = 0 ;

      return hold ;
   }



   WSTR5 * S4FUNCTION convert5charToUnicode( const char *in, long codePage, int numChars, WSTR5 *outx )
   {
      // conversion using input code page
      WSTR5 *out ;

      // should already be in a valid codepage value (not cp437, but 437)
      assert5( codePage == 437 || codePage == 1252 ) ;

      if ( outx == 0 )
         out = (WSTR5 *)alloc5( (numChars + 1) * 2 ) ;
      else
         out = outx ;

      #ifdef S4WIN32
         MultiByteToWideChar( codePage, 0, in, -1, out, numChars ) ;
      #else
         /* LY 2001/09/13  mbsrtowcs( out, &in, numChars, 0 ) ; */
         c4mbsToWcs( out, in, numChars ) ;
      #endif

      return out ;
   }



}
