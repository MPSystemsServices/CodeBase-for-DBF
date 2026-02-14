/* f4flush.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifdef __WATCOMC__
   #ifdef M_I386
      #define S4USE_EBX
   #endif
#endif

#ifdef S4OS2
   #define S4USE_DUP
#endif

/* returns 1 if no handles available */
static int file4lowFlush( FILE4 *file )
{
   #ifdef S4PALM
      return 0 ;
   #else
      int rc ;

      #ifdef S4USE_DUP
         #ifndef S4NO_DUP
            int handle ;
         #endif
      #else
         #ifdef S4WINTEL
         #ifndef S4WIN32
            union REGS dosFlush ;
         #endif
         #endif
      #endif

      #ifdef S4MACINTOSH
         short vRefNum ;
      #endif

      #ifdef E4PARM_LOW
         if ( file == 0 )
            return error4( 0, e4parm, E90610 ) ;
      #endif

      #ifndef S4OFF_OPTIMIZE
         if ( file->fileCreated == 0 )  /* file not open */
            return 0 ;
      #endif
// LY 2003/07/31      #ifdef S4WIN64 /* LY 00/09/20 */
//         if ( file->hand == NULL )  /* file not open */
//      #else
         if ( file->hand == INVALID4HANDLE )  /* file not open */
//      #endif
         return 0 ;

      #ifdef S4WRITE_DELAY
         if ( l4numNodes( &file->delayWriteFileList ) != 0 )
         {
            /* for now, just flush out any changed data.  Later allow reading
               of delay-write data */
            file4writeDelayFlush( file, 1 ) ;
         }

         EnterCriticalSection( &file->critical4file ) ;
      #endif

      rc = 0 ;

      #ifdef S4USE_DUP
         #ifndef S4NO_DUP
            handle = dup( file->hand ) ;
            if ( handle == INVALID4HANDLE )  /* means that there are no available handles */
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
            else
               if ( close( handle ) < 0 )
                  rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #endif
      #else
         #if   defined( S4MACINTOSH )
            if (GetVRefNum( file->hand, &vRefNum ) != 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
            else
               if (FlushVol( 0, vRefNum ) != 0 )
                  rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #elif defined( S4WIN32 )
            if ( !FlushFileBuffers( (HANDLE) file->hand ) )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #elif !defined( S4UNIX )
            dosFlush.h.ah = 0x68 ;
            #ifdef S4USE_EBX
               dosFlush.x.ebx = (unsigned int)file->hand ;
            #else
               dosFlush.x.bx = (unsigned int)file->hand ;
            #endif
            intdos( &dosFlush, &dosFlush ) ;
            if ( dosFlush.x.cflag != 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #else
            if ( fsync(file->hand) < 0 )
               rc = error4( file->codeBase, e4optFlush, E90610 ) ;
         #endif
      #endif

      #ifdef S4WRITE_DELAY
         LeaveCriticalSection( &file->critical4file ) ;
      #endif

      return rc ;
   #endif
}

#ifndef S4OFF_OPTIMIZE
int opt4fileFlush( FILE4 *file, const int doFree )
{
   FILE4LONG realLen ;
   int saveRc, rc ;

   #ifdef E4PARM_LOW
      if ( file == 0 || doFree < 0 || doFree > 1 )
         return error4( 0, e4parm, E90610 ) ;
   #endif

   if ( file->doBuffer == 1 )
   {
      switch( file->type )
      {
         case OPT4DBF:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.dbfLo.list, doFree ) ;
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.dbfHi.list, doFree ) ;
            break ;
         case OPT4INDEX:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.indexLo.list, doFree ) ;
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.indexHi.list, doFree ) ;
            break ;
         default:
            rc = opt4fileFlushList( &file->codeBase->opt, file, &file->codeBase->opt.other.list, doFree ) ;
            break ;
      }
      if ( file->codeBase->opt.writeFile == file )  /* flush out the final file changes */
      {
         saveRc = opt4flushWriteBuffer( &file->codeBase->opt ) ;
         if ( saveRc < 0 )
            rc = saveRc ;
         #ifdef E4ANALYZE
            if ( file->codeBase->opt.writeFile == file )  /* we should have fixed this... */
               rc = error4( file->codeBase, e4opt, E80603 ) ;
         #endif
      }

      // AS 12/01/00 - changed to unsigned long value...
      if ( file4longError( file->len ) != ULONG_MAX )
      {
         file->doBuffer = 0 ;
         realLen = file->len ;

         #ifdef S4OFF_OPTIMIZE
            if ( file->fileCreated == 0 )
            {
               #ifdef E4ANALYZE
                  if ( file4longGetLo( realLen ) != 0 )
                     return error4( file->codeBase, e4opt, E80603 ) ;
               #endif
            }
            else  /* for file4len lines below */
         #endif
            if ( file4longGetLo( file4lenLow( file ) ) != file4longGetLo( realLen ) )   /* make sure the length is updated */
               file4lenSetLow( file, realLen ) ;

         file->doBuffer = 1 ;
      }
      if ( rc )
         return rc ;
   }

   return 0 ;
}
#endif /* S4OFF_OPTIMIZE */

int S4FUNCTION file4flush( FILE4 *file )
{
   int rc, err ;

   #ifdef E4PARM_HIGH
      if ( file == 0 )
         return error4( 0, e4parm_null, E90611 ) ;
   #endif

   if ( file->isReadOnly == 1 )
      return 0 ;

   #ifndef S4OFF_OPTIMIZE
      opt4fileFlush( file, 0 ) ;
   #endif

   err = error4set( file->codeBase, 0 ) ;
   rc = file4lowFlush( file ) ;
   if ( err != 0 )
      error4set( file->codeBase, (short)err ) ;

   if ( rc < 0 )
      return error4stack( file->codeBase, (short)rc, E90611 ) ;

   return 0 ;
}

#ifndef _MSC_VER
   #ifdef P4ARGS_USED
   #pragma argsused
#endif
#endif
int S4FUNCTION file4refresh( FILE4 *file )
{
   #ifndef S4OPTIMIZE_OFF
      #ifndef S4SINGLE
         int rc ;
         #ifdef E4PARM_HIGH
            if ( file == 0 )
               return error4( 0, e4parm_null, E90612 ) ;
         #endif

         if ( file->doBuffer == 0 || file->lowAccessMode != OPEN4DENY_NONE )
            return 0 ;

         rc = opt4fileFlush( file, 1 ) ;
         if ( rc < 0 )
            return error4stack( file->codeBase, rc, E90612 ) ;
         file4longAssignError( file->len ) ;
      #endif
   #endif
   return 0 ;
}
