/* f4seq.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.hpp"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#include "d4data.hpp"

//File4seqRead &File4seqRead::operator>>( Str4 &s )
//{
//   unsigned num_read = read( s.ptr( ), s.len( ) ) ;
//   if( num_read < s.len( ) )
//      s.setLen( num_read ) ;
//   return *this ;
//}

File4seqWrite & File4seqWrite::operator<<( const long longVal )
{
   char t[16] ;
   char *pt;
   c4ltoa45( longVal, t, 15 ) ;
   t[15] = 0 ;
   for (pt = t;*pt && *pt == ' ';pt++)
      ;
   write( pt ) ;
   return *this;
}

