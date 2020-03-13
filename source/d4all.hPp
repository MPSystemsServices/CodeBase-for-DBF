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

