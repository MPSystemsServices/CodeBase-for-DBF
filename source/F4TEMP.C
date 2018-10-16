/* f4temp.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifdef S4TEMP
   #include "t4test.h"
#endif

#ifdef S4WINTEL
   #ifndef S4OFF_MULTI
      #ifndef S4WINCE   /* LY 2002/11/12 : added !S4WINCE */
         #ifndef S4IBMOS2
            #ifndef __TURBOC__
               #include <sys\locking.h>
               #define S4LOCKING
            #endif
            #ifdef _MSC_VER
               #include <sys\types.h>
               #include <sys\locking.h>
            #endif
         #endif
      #endif
   #endif
#endif

#ifdef S4CB51
int S4FUNCTION file4temp( FILE4 *file, CODE4 *c4, char *buf, const int auto_remove )
{
   int i, save_flag, rc, old_excl ;
   #ifndef S4OPTIMIZE_OFF
      int old_opt_wr, tf_do_buffer ;
      unsigned long tf_hash_init ;
      char tf_type, tf_buffer_writes, tf_write_buffer ;
      FILE4LONG tf_len ;
      LINK4 tf_link ;
   #endif
   time_t t ;
   #ifdef S4UNIX
      #ifndef S4NO_FTIME
         struct timeb timeVal ;
      #else
         struct timeval timeVal ;
      #endif
   #endif
   char name[20], *name_ptr ;

   #ifdef E4PARM_HIGH
      if ( file == 0 || c4 == 0  )
         return error4( c4, e4parm_null, E90623 ) ;
   #endif

   #ifndef S4OPTIMIZE_OFF
      if ( auto_remove && c4->opt.numBuffers != 0 )
      {
         if ( c4->opt.forceCurrent == 0 )
         {
            memset( (void *)file, 0, sizeof( FILE4 ) ) ;

            // AS Sep 17/01 assume it is not a datafile, since if it is we handle that later...
            file4setTemporary( file, 1, 0 ) ;

            file->codeBase = c4 ;
            file->fileCreated = 0 ;
            #ifndef S4OFF_MULTI
               file->lowAccessMode = 1 ;
            #endif
            old_opt_wr = c4->optimizeWrite ;
            c4->optimizeWrite = 1 ;
            file4optimize( file, 1, OPT4OTHER ) ;
            c4->optimizeWrite = old_opt_wr ;
            if ( buf != 0 )
               file->name = buf ;
            return 0 ;
         }
         else
         {
            tf_hash_init = file->hashInit ;
            tf_len = file->len ;
            tf_type = file->type ;
            tf_buffer_writes = file->bufferWrites ;
            tf_do_buffer = file->doBuffer ;
            tf_write_buffer = file->writeBuffer ;
            memcpy( (void *)&tf_link, (void *)&file->link, sizeof( LINK4 ) ) ;
         }
      }
   #endif

   name_ptr = buf ;
   if ( name_ptr == 0 )
      name_ptr = name ;

   memcpy( name_ptr, "TEMP", 5 ) ;  // AS Dec 13/05 vs 5.0 fixes
   for ( i = 0 ; i < 100 ; i++ )
   {
      u4delaySec() ;
      #ifndef S4UNIX
         time( &t ) ;
         t %= 10000L ;
      #else
         #ifndef S4NO_FTIME
            ftime(&timeVal) ;
            t = timeVal.millitm % 10000L ;
         #else
            gettimeofday(&timeVal) ;
            t = timeVal.tv_usec % 10000L ;
         #endif
      #endif
      c4ltoa45( t, name_ptr + 4, -4 ) ;
      memcpy( name_ptr + 8, ".TMP", 5 ) ;  // AS Dec 13/05 vs 5.0 fixes

      save_flag = c4getErrCreate( c4 ) ;
      c4setErrCreate( c4, 0 ) ;

      #ifndef S4OFF_MULTI
         old_excl = c4->accessMode ;
         c4->accessMode = 1 ; /* all temporary files are for exclusive access only */
      #endif
      rc = file4create( file, c4, name_ptr, (int)(buf == 0) ) ;
      #ifndef S4OFF_MULTI
         c4->accessMode = old_excl ;
      #endif

      c4setErrCreate( c4, save_flag ) ;
      if ( rc < 0 )
         return -1 ;
      if ( rc == 0 )
      {
         if ( auto_remove )
         {
            // AS Sep 17/01 assume it is not a datafile, since if it is we handle that later...
            file4setTemporary( file, 1, 0 ) ;
         }

         #ifndef S4OPTIMIZE_OFF
            if ( auto_remove && c4->opt.numBuffers != 0 )
            {
               if ( c4->opt.forceCurrent == 1 )
               {
                  file->hashInit = tf_hash_init ;
                  file->len = tf_len ;
                  file->type = tf_type ;
                  file->bufferWrites = tf_buffer_writes ;
                  file->doBuffer = tf_do_buffer ;
                  file->writeBuffer = tf_write_buffer ;
                  memcpy( (void *)&file->link, (void *)&tf_link, sizeof( LINK4 ) ) ;
               }
               else
               {
                  old_opt_wr = c4->optimizeWrite ;
                  c4->optimizeWrite = 1 ;
                  file4optimize( file, 1, OPT4OTHER ) ;
                  c4->optimizeWrite = old_opt_wr ;
               }
            }
         #endif
         return  0 ;
      }
   }

   return error4( c4, e4create, E80605 ) ;
}
#endif

// AS May 24/02 - created file4createInternal for internal use to indicate file types
int file4tempLow( FILE4 *file, CODE4 *c4, const int autoRemove, int useTempDirectory, const char *ext, char fileType )
{
   int i, saveFlag, rc ;
   #ifndef S4OFF_MULTI
      int oldExcl ;
   #endif
   #ifndef S4OPTIMIZE_OFF
      int oldOptWr ;
      int tfDoBuffer = 0 ;
      long tfHashInit = -1L ;
      char tfType = (char)-1 ;
      char tfBufferWrites = (char)-1 ;
      char tfWriteBuffer = (char)-1 ;
      long tfLen = -1L ;
      LINK4 tfLink ;
   #endif
   #if defined(S4WINCE) || defined(S4WIN32)
      SYSTEMTIME st ;
      WORD t ;
   #elif defined( S4PALM )
      UInt32 t;
   #else
      time_t t ;
   #endif
   char name[255], *env = 0 ;
   #ifdef S4WINCE
                WSTR5 nameU[255];
        #endif
   char fileName[13] ;
   char drive[3] ;

   #ifdef E4PARM_HIGH
      if ( file == 0 || c4 == 0  )
         return error4( c4, e4parm_null, E90618 ) ;
   #endif

   #ifndef S4OPTIMIZE_OFF
      // AS 04/20/01 - changed how optimization gets going
      // code4memStartMaxSet( c4, c4->memMaxPercent ) ;  /* start optimization if not enabled and not suspended */

      memset( &tfLink, 0, sizeof( tfLink ) ) ;

      if ( autoRemove && c4->opt.numBuffers != 0 )
      {
         if ( c4->opt.forceCurrent == 0 )
         {
            memset( (void *)file, 0, sizeof( FILE4 ) ) ;
            // AS Sep 3/03 - ensure the file handle is marked as invalid
            file->hand = INVALID4HANDLE ;
            // AS Sep 3/03 - Must set file->codeBase before file4setTemporary is called
            file->codeBase = c4 ;
            // AS Sep 17/01 assume it is not a datafile, since if it is we handle that later...
            file4setTemporary( file, 1, 0 ) ;
            file->fileCreated = 0 ;
            #ifndef S4OFF_MULTI
               file->lowAccessMode = OPEN4DENY_RW ;
            #endif
            oldOptWr = c4->optimizeWrite ;
            c4->optimizeWrite = 1 ;
            file4optimize( file, 1, OPT4OTHER ) ;
            c4->optimizeWrite = oldOptWr ;
            return 0 ;
         }
         else
         {
            tfHashInit = file->hashInit ;
            tfLen = file4longGetLo( file->len ) ;
            tfType = file->type ;
            tfBufferWrites = file->bufferWrites ;
            tfDoBuffer = file->doBuffer ;
            tfWriteBuffer = file->writeBuffer ;
            memcpy( (void *)&tfLink, (void *)&file->link, sizeof( LINK4 ) ) ;
            #ifdef E4ANALYZE
               if ( file->name != 0 )
                  return error4( c4, e4struct, E90618 ) ;
            #endif
         }
      }
   #endif

   saveFlag = c4getErrCreate( c4 ) ;
   c4setErrCreate( c4, 0 ) ;
   #ifndef S4OFF_MULTI
      oldExcl = c4->accessMode ;
      c4->accessMode = OPEN4DENY_RW ; /* all temporary files are for exclusive access only */
   #endif

   #if !defined( S4WINCE ) && !defined( S4PALM )
      if ( useTempDirectory )
      {
         // AS Dec 13/05 - under Windows getenv is becoming deprecated...
         #ifdef S4UNIX
            env = u4environ( c4, "TMPDIR", 0 ) ;
         #else
            env = u4environ( c4, "TEMP", 0 ) ;
            if ( env == 0 )
               env = u4environ( c4, "TMP", 0 ) ;
         #endif
      }
      else
         env = 0 ;
   #endif

   if ( env == 0 )
   {
      drive[0] = 0 ;
      env = "" ;
   }
   else
   {
      if ( env[1] == ':' )
      {
         drive[0] = env[0] ;
         drive[1] = env[1] ;
         drive[2] = 0 ;
      }
      else
         drive[0] = 0 ;
   }

   // AS Feb 2/10 - reduce by 3 characters to make more combinations possible
   c4strcpy( fileName, sizeof( fileName ), "T" ) ;  // AS Dec 13/05 vs 5.0 fixes
   Bool5 extNull = 0 ;
   if ( ext == NULL )
       extNull = 1 ;
   else
       if ( ext[0] == 0 )
           extNull = 1 ;
   if (extNull == 1 )
   {
      // Sofia Treinova Dec 3 2007 - changed to "sizeof( fileName ) - 8" to
      // avoid buffer overflow
      c4strcpy( fileName + 8, sizeof( fileName ) - 8, ".TMP" ) ;
      fileName[12] = 0 ;
   }
   else
   {
      fileName[8] = '.' ;
      c4strcpy( fileName + 9, sizeof( fileName ) - 9, ext ) ;
      fileName[9+c4strlen(ext)] = 0 ;
   }

   // AS Feb 2/10 use windows to get temporary name if possible; less likely to be a problem...
   #ifdef S4WIN32
      if ( extNull = 1 )
      {
         u4nameMake( name, sizeof( name ), drive, env + c4strlen( drive ), "" ) ;
         //  Generates a temporary file name.
         #ifdef S4WINCE
                                c4atou(name, nameU, strlen(name));
                                rc = GetTempFileName( nameU, // directory for tmp files
                                                                                                        TEXT("T"),     // temp file name prefix  (max 3 characters)
                                                                                                        0,      // create unique name  (0 means check that it doesn't exist)
                                                                                                        nameU);  // buffer for name
                        #else
                                rc = GetTempFileName( name, // directory for tmp files
                                                                                                        TEXT("T"),     // temp file name prefix  (max 3 characters)
                                                                                                        0,      // create unique name  (0 means check that it doesn't exist)
                                                                                                        name);  // buffer for name
                        #endif
         if ( rc == 0 )// means failure
            rc = 1 ;
         else
         {
            //in this case the file is created, so turn safety off to recreate it
            int saveSafety = c4->safety ;
            c4->safety = 0 ;
            // AS May 24/02 - created file4createInternal for internal use to indicate file types
            rc = file4createInternal( file, c4, name, 1, fileType ) ;
            c4->safety = saveSafety ;
         }
      }
      else
         rc = 1 ;  // if we have an extension use our system of creating file
      if (rc != 0)  // if it fails, use the old technique
   #endif
      {  // needed for if in win32 case
         // CS 2007/01/11 Move outside of loop.
         #if defined(S4WINCE) || defined(S4WIN32)
            GetLocalTime(&st) ;
            t = (st.wSecond * 1000) + st.wMilliseconds ;
         #elif defined( S4PALM )
            t = TimGetSeconds();
         #else
            time( &t );
         #endif
         for ( i = 0 ;; i++)
         {
            if ( i >= 1000 ) // AS Feb 2/10 - support more combinations in case many temp files in use
            {
               rc = error4describe( c4, e4create, E80605, name, 0, 0 ) ;
               break ;
            }
            // CS 2007/01/11 Make sure t does not exceed 999999.
            t = (short)((t + i) % 1000000L);
            c4ltoa45( t, fileName + 1, -7 ) ;  // AS Feb 2/10 - support 2 more characters for more combos
            u4nameMake( name, sizeof( name ), drive, env + c4strlen( drive ), fileName ) ;

            // AS May 24/02 - created file4createInternal for internal use to indicate file types
            rc = file4createInternal( file, c4, name, 1, fileType ) ;
            if ( rc < 0 )
               break ;

            // AS 06/07/99 --> there is a problem that the .dbf may get created, but if a file
            // of the same name with index or memo extension exists, then the creation fails.
            // therefore, if the extension is dbf, check that these other files don't exist...
            //CJ -06/11/99- This only really needs to be done for CodeBase OLEDB do not do for 16-bit applications
            #ifdef S4WIN32
               // AS May 16/03 - only need to do this check if rc == 0
               if ( rc == 0 && ext != 0 )
               {
                  if ( u4namecmp( ext, DBF4EXT, c4->ignoreCase ) == 0 )
                  {
                     char indexOrMemoNameBuf[LEN4PATH] ;
                     c4strcpy( indexOrMemoNameBuf, LEN4PATH, name ) ;  // AS Dec 13/05 vs 5.0 fixes
                     #ifdef S4CLIPPER
                        u4nameExt( indexOrMemoNameBuf, sizeof( indexOrMemoNameBuf ), ".CGP", 1 ) ;
                     #else
                        u4nameExt( indexOrMemoNameBuf, sizeof( indexOrMemoNameBuf ), code4indexExtension( c4 ), 1 ) ;
                     #endif
                     if ( file4exists( indexOrMemoNameBuf ) >= 0 ) // index exists, so don't allow create...
                     {
                        rc = -1 ;  // force loop to continue...
                        // AS Mar 24/04 - was not closing the file in this case...
                        file4close( file ) ;
                     }
                     else  // index ok, try memo...
                     {
                        u4nameExt( indexOrMemoNameBuf, sizeof( indexOrMemoNameBuf ), code4memoExtension( c4 ), 1 ) ;
                        if ( file4exists( indexOrMemoNameBuf ) >= 0 ) // memo exists, so don't allow create...
                        {
                           rc = -1 ;  // force loop to continue...
                           // AS Mar 24/04 - was not closing the file in this case...
                           file4close( file ) ;
                        }
                     }
                  }
               }
            #endif

            if ( rc == 0 )
               break ;

            // AS Oct 8/03 - revised to not require delay by using incrementor
            // CS 2001/03/09 no delay
            // AS Sep 19/03 - We do need a delay in here, otherwise it is possible that the function will loop through
            // so fast that we don't actually have a changed file name.
            // if (i > 10 )
            //    u4delayHundredth( i ) ;
         }
      }  // needed for if in win32 case

   #ifndef S4OFF_MULTI
      c4->accessMode = oldExcl ;
   #endif
   c4setErrCreate( c4, saveFlag ) ;

   if ( rc < 0 )
      return error4stack( c4, (short)rc, E90618 ) ;
   if ( autoRemove )
   {
      // AS Sep 17/01 assume it is not a datafile, since if it is we handle that later...
      file4setTemporary( file, 1, 0 ) ;
   }
   #ifndef S4OPTIMIZE_OFF
      if ( autoRemove && c4->opt.numBuffers != 0 )
      {
         if ( c4->opt.forceCurrent == 1 )
         {
            file->hashInit = tfHashInit ;
            /* LY July 7/03 : changed from 0 to 0L for Linux compiler */
            file4longAssign( file->len, tfLen, 0L ) ;
            file->type = tfType ;
            file->bufferWrites = tfBufferWrites ;
            file->doBuffer = tfDoBuffer ;
            file->writeBuffer = tfWriteBuffer ;
            memcpy( (void *)&file->link, (void *)&tfLink, sizeof( LINK4 ) ) ;
         }
         else
         {
            oldOptWr = c4->optimizeWrite ;
            c4->optimizeWrite = 1 ;
            file4optimize( file, 1, OPT4OTHER ) ;
            c4->optimizeWrite = oldOptWr ;
         }
      }
   #endif
   return 0 ;
}


// AS Sep. 17/01 - Must use function to change temporary setting to update validation tables
void file4setTemporary( FILE4 *file, Bool5 flag, Bool5 isDataFile )
{
   // if the file is a data file, set isDataFile flag to true.

   // AS Sep 2/03 - if file->codeBase is null, return out.
   if ( file == 0 || file->codeBase == 0 )
   {
      #ifdef E4ANALYZE
         error4( 0, e4parmNull, E90618 ) ;
      #endif
      return ;
   }

   // AS Oct 22/02 - make available to be called in multi-user (if user calls directly)
   #if !defined( S4CLIENT ) && !defined( S4OFF_INDEX )
      if ( isDataFile && file->codeBase->validationTable != 0 )
         code4validateModifyTemp( file->codeBase, file->name, flag ) ;
   #endif

   file->isTemporary = flag ;
}
