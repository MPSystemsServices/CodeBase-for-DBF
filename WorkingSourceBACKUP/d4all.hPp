/* d4all.hpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef S4CBPP
   #define S4CBPP
#endif

// AS Aug 4/03 - No longer required, gives error in newer Microsoft Compiler
// #ifndef _WIN32_WCE
//    #include <iostream.h>
// #endif
// LY Jan 6/05 : require iostream for VC++ .NET 2003 and examples that use console output
#if !defined(S4LIB_BUILD) && _MSC_VER >= 1300
   #include <iostream>
   using namespace std ;
#endif

#ifdef D4DLL_CPP
   #include "d4dll.h"
#else
   #include "d4all.h"
#endif
#include "d4data.hpp"

