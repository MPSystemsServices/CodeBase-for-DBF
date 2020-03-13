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

/* f4info_p.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.hpp"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

#ifndef S4JNI  /* LY 99/07/08 */
Data4 Field4::data()
{
   DEBUG4PTR(field==0, 61204L)
   #ifdef D4DLL_CPP
      return Data4( field->d4 ) ;
   #else
      return Data4(field->data) ;
   #endif
}
#endif

Field4info::Field4info( Code4 &code )
{
   codeBase = &code ;
   size = 0 ;
   length = 0 ;
   field = NULL ;
}

Field4info::Field4info( Data4 d )
{
   DEBUG4VOID(d.data == 0, E60282)
   size = 0 ;
   length = 0 ;
   field = NULL ;
   #ifdef D4DLL_CPP
      codeBase = (Code4 *) d.data->c4 ;
   #else
      codeBase = (Code4 *) d.data->codeBase ;
   #endif
   add( d ) ;
}

const FIELD4INFO * Field4info::operator[] ( int index )
{
   FIELD4INFO *t ;

   DEBUG4PTR(field == 0, E60280)
   if (index > size || index < 0)
   {
      t=NULL;
   }
   else
   {
      t=&field[index];
   }
   return t ;
}

int Field4info::add( Data4 d )
{
   short i ;
   for( i = 1 ; i <= d4numFields( d.data) ; i++ )
   {
      FIELD4 * f ;
      f = d4fieldJ( d.data, i ) ;
      // LY Apr 14/05 : added f4nullable() for fifth param to ::add()
      if( add( f4name( f ), (char)f4type( f ), (int)f4len( f ), f4decimals(f), (f4nullable( f ) ? r4null : 0) ) < 0 )
         return -1 ;
   }
   return 0 ;
}

int Field4info::add(const char *name, char type, int len, int dec, unsigned short allowNulls )
{
   char st_name[11] = "";
   size_t copyLen = 10;
   const char *spacePos = strchr( name, ' ' ) ;
   if (spacePos)
      copyLen = spacePos - name ;
   // AS May 26/06 - under Windows strcpy is becoming deprecated...
   // strncpy( st_name, name, copyLen > 10? 10: copyLen ) ;
   c4strncpy( st_name, sizeof( st_name ), name, copyLen > 10 ? 10 : copyLen ) ;
   c4upper( st_name ) ;

   if( u4allocAgain( codeBase, (char**)&field, &length, (size+2)*sizeof(FIELD4INFO)) != 0 )
      return -1 ;

   if( (field[size].name = (char *) u4allocEr( codeBase, 11 )) == 0 )
      return -1 ;

   u4ncpy( field[size].name, st_name, 11) ;

   field[size].type = (short int) type ;
   field[size].len = (unsigned short int) len ;
   field[size].dec = (unsigned short int) dec ;
   field[size].nulls = allowNulls ;

   size++ ;
   return 0 ;
}

#ifndef S4JNI  /* LY 99/07/08 */
int Field4info::add( Field4 fp )
{
   return add( fp.name(), (char)fp.type(), fp.len(), fp.decimals() ) ;
}

int Field4info::del( const char *name )
{
   DEBUG4INT(codeBase == 0, E60281)
   for( int i = 0 ; i < size ; i++ )
   {
      /* BCR 04/17/01 -- use c4stricmp for UNIX too */
      // AS May 26/06 - just always use c4stricmp...avoids deprecation problems
      // #if defined( S4WINCE ) || defined( S4UNIX )
         if( c4stricmp( field[i].name, (char*) name ) == 0 )
      // #else
      //    if( stricmp( field[i].name, name ) == 0 )
      // #endif
      {
          del( i ) ;
          return 0 ;
      }
   }
   codeBase->error( e4fieldName, E60281, name ) ;
   return -1 ;
}
#endif

int Field4info::del( int index )
{
   DEBUG4INT(codeBase == 0, E60281)
   if( index >= size || index < 0 )
   {
      codeBase->error( e4parm, E60281 ) ;
      return -1 ;
   }

   u4free( field[index].name ) ;
   memcpy( field+index, field+index+1, sizeof(FIELD4INFO) * (size-index) ) ;
   size-- ;
   return 0 ;
}

void Field4info::free( )
{
   for( int i = size-1 ; i >= 0 ; i-- )
      u4free( field[i].name ) ;
   size = 0 ;
   if( field )
   {
      u4free( field ) ;
      field = 0 ; length = 0 ;
   }
}

Field4info::~Field4info()
{
   free( ) ;
}

#ifndef S4JNI
   short Field4::isBinaryField()
   {
      switch ( f4type( field ) )
      {
         case r4int:
         // AS Jul 27/05 - float field support (C++)
         case r4floatBin:
         case r4double:
         case r4currency:
         case r4dateTime:
         case r5i2:
         case r5ui2:
         case r5ui4:
         case r5i8:
         case r5ui8:
            return 1 ;
      }
      return 0;
   }
#endif
