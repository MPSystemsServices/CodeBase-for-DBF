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

// oledb5.hpp

#ifndef D4ALL_INC
   #include "d4all.h"
#else

#ifndef OELDB5_INC

#define OELDB5_INC

// ?? Want to remove these later ..
#define S4OFF_SCHEMA_OPTIMIZE
//#define E4DEBUG_KEN

//AS 02/16/99 - no longer needed for 2.1 #define OLEDBVER 0x0200

#define _UNICODE

// AS 08/31/98, should just be exporting relevant functions...
//#ifdef S4DLL_BUILD
//   #define S5CLASS __declspec(dllexport)
//#else
//   #define S5CLASS
//#endif
//#define class5 class S5CLASS
#define S5CLASS
#define class5 class

#ifdef S4WIN32
   // work-around for comdef.h which affects the byte alignment.  pop off our
   // one-byte alignment, and then push back on after the file
   #include "pop4.h"
   #include <comdef.h>   // AS 09/22/98 must be before other files or compiler errors out
   #include "push4.h"

   // #ifndef OLEDBVERCHK
   //    #define OLEDBVERCHK
   //    #ifdef OLEDBVER
   //       #error OLEDBVER defined outside of OLEDB.H
   //    #endif
   // #endif

   #include "oledb.h"
   // AS 12/18/00 - we support version 2.6 now...
   // AS OCT 18/01 - Allow for use with 2.7 - we haven't tried, but assume it will work...
   #if OLEDBVER < 0x0260
      #error oledbver < 0x0260  (Minimum of MDAC 2.6 required for compilation)
   #endif
   #include "oledberr.h"
   #include "msdaguid.h"
#endif

#include "defs5.hpp"
#include "err5.hpp"
#include "interfa5.hpp"
#include "unknown5.hpp"
#include "util5.hpp"
#include "list5.hpp"
#include "prop5.hpp"
#include "source5.hpp"
#include "array5.hpp"
#include "session5.hpp"
#include "convert5.hpp"
#include "blast5.hpp"
#include "field5.hpp"
#include "a5.hpp"
#include "rowset5.hpp"
#include "row5.hpp"
#include "table5.hpp"
#include "index5.hpp"
#include "schema5.hpp"
#ifndef S4SERVER
   #include "factory5.hpp"
#endif
#include "d4stamp.h"
#include "stamp5.hpp"

#ifdef S4WIN32
   #include <direct.h>
#endif
#include <math.h>

#ifdef S4TESTING
int checkDBPROPINFO(DBPROPINFO ofor, int property, int x);
#endif

#endif /* OELDB5_INC */
#endif /* D4ALL_INC */
