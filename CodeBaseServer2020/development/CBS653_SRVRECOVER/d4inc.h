/* d4inc.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef S4RESOURCE_BUILD
   #ifdef S4PALM
      #ifndef __PILOT_H__
         #include <PalmOS.h>
      #endif
      #include <StringMgr.h>
      #include <MemoryMgr.h>
      #include <FileStream.h>
      #include <DateTime.h>
      #include <FloatMgr.h>
      #include <FntGlue.h>
      #include <TxtGlue.h>
      #include <WinGlue.h>
   #else
      #include <stdlib.h>
      #include <string.h>
      /* LY 00/07/04 : added !S4WINCE */
      #if !defined( S4MACINTOSH ) && !defined( S4WIN16 ) && !defined( __BORLANDC__ ) \
                      && !defined( S4DOS ) && !defined( S4WINCE )    /* CS 2000/12/01 */
        #include <wchar.h>
      #endif
      #include <limits.h>
      #ifndef S4WINCE
        #include <stdio.h>
        #include <time.h>
      #endif

      #if defined(S4WIN16) || defined(S4WIN32) || defined(S4WINCE)
         #include <windows.h>
      #endif

      #if !defined(S4STAND_ALONE) && defined(S4WINSOCK)
         #include <winsock.h>
      #endif
   #endif

   #if defined( S4UNIX ) || defined( S4MACINTOSH ) || defined( S4PALM )
      #include "p4port.h"
   #else
      #include <stdarg.h>
      #ifndef S4WINCE
         #include <io.h>
      #endif
      #ifdef S4OS2
         #include <os2.h>
         #include <direct.h>
      #else
         #ifndef S4WINCE
            #include <dos.h>
         #endif
      #endif
   #endif

   #ifdef S4UNIX
      #include <dirent.h>
   #endif

   #ifndef S4WINCE
      #include "push4.h"
   #endif

   #include "d4defs.h"
   #ifdef __cplusplus
      #include "single4.hpp"
   #endif
   #ifdef OLEDB5BUILD
      #ifdef S4WIN32
         /* need err5 for m4mem2.h in some instances if ole-db... */
         #include "oledb.h"
         #include "oledberr.h"
         // #include "err5.hpp"
      #else
         #include "oledb5u.hpp"
      #endif
   #endif
   #if defined(S4WIN32) && defined(__cplusplus) /* CS 1999/09/20 */
      #include "m4mem2.h"
   #endif
   #ifdef __BORLANDC__  /* LY 00/10/19 : for CJ */
      #if __BORLANDC__ >= 0x550
         #include <oledb.h>
      #endif
   #endif
   #include "d4data.h"  /* LY 2001/04/12 : moved after oledb.h to avoid redefs */
   #include "d4declar.h"
   #include "timer5.h"
   #include "d4inline.h"
   #include "f4flag.h"
   #include "e4expr.h"
   #include "s4sort.h"
   #ifdef S4SERVER
      #include "d4secure.h"
   #endif
   #include "e4string.h"
   #include "e4error.h"

   #include "o4opt.h"

   #include "c4com.h"
   #ifndef S4STAND_ALONE
      #include "c4comlow.h"
   #endif

   #include "c4trans.h"

   #ifdef OLEDB5BUILD
      #include "oledb5.hpp"
   #endif

   #ifdef S4SERVER
      #include "d4server.h"
   #endif

   #ifdef OLEDB5BUILD
      // must be included after d4server.h
      #include "inline5.hpp"
   #endif

   #include "r4relate.h"

   #ifdef S4VBX
      #include "ctrl4vbx.h"
   #else
      #ifdef S4CONTROLS
         #include "ctrl4.h"
      #endif
   #endif

   #include "r4report.h"

   #ifdef S4CODE_SCREENS
      #include "w4.h"
   #endif

   #ifndef S4WINCE
      #include "pop4.h"
   #endif

#endif /* !S4RESOURCE_BUILD */
