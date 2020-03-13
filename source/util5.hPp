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

/* util5.hpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef UTIL5INCLUDED
   #define UTIL5INCLUDED

   #ifndef S4SERVER
      extern LPMALLOC pIMalloc5 ;  // Refer to the gloabal 'alloc5' function.
   #endif

   extern "C" {
      //const char *S4FUNCTION error4text( CODE4 *c4, const long errCode2 ) ;
      void log4invokeDebugger() ;
      #ifdef S4OLEDEBUG_PRINT
         void log5enable() ;
         void log5( const char *ptrToWrite ) ;
         void log5closeFile() ;
         void log5query( const char *ptrToWrite ) ;
         void out5riidInfo( REFIID riid ) ;
      #endif

   #ifndef S5EXPORT
      // usually already defined, but maybe not if not oledb build...
      #define S5EXPORT
   #endif

      S4EXPORT char *S4FUNCTION getCharPtrFromUnicode5( const WSTR5 *in, char S4PTR *out = 0 ) ;
      S4EXPORT WSTR5 * S4FUNCTION getUnicodeFromCharPtr5( const char S4PTR *in, WSTR5 *outx = 0 ) ;

      S4EXPORT char *S4FUNCTION convert5unicodeToChar( const WSTR5 *in, long codePage, int numChars, char S4PTR *out = 0 ) ;
      S4EXPORT WSTR5 * S4FUNCTION convert5charToUnicode( const char S4PTR *in, long codePage, int numChars, WSTR5 *outx = 0 ) ;

      S5EXPORT void *S4FUNCTION alloc5( ULONG amount ) ; // Throws a 'err5memory()' exception if it could not be allocated.
      S5EXPORT HRESULT S4FUNCTION alloc5init() ; // Throws a 'err5memory()' exception if it could not be allocated.
      S5EXPORT void S4FUNCTION alloc5initUndo() ;
      S5EXPORT void *S4FUNCTION alloc5null( ULONG amount ) ; // Throws a 'err5memory()' exception if it could not be allocated.
                                                             // Sets memory to null after allocation.
      S5EXPORT void S4FUNCTION free5( void S4PTR * ) ;
   }
#endif /* UTIL5INCLUDED */
