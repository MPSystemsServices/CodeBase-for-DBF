/* d4field.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

FIELD4 *S4FUNCTION d4field( DATA4 *data, const char *fieldName )
{
   int fieldIndex ;

   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E94001 ) ;
         return 0 ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94001 ) )
         return 0 ;
   #endif

   fieldIndex =  d4fieldNumber( data, fieldName ) - 1 ;
   if ( fieldIndex < 0 )
      return 0 ;

   return data->fields + fieldIndex ;
}

FIELD4 *S4FUNCTION d4fieldJ( DATA4 *data, short jField )
{
   #ifdef E4PARM_HIGH
      if ( data == 0 )
      {
         error4( 0, e4parm_null, E94002 ) ;
         return (FIELD4 *)0 ;
      }
   #endif

   #ifdef E4VBASIC
      if ( c4parm_check( data, 2, E94002 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( jField <= 0 || data->fields == 0 || jField > data->dataFile->nFields )
      {
         error4( data->codeBase, e4parm, E94002 ) ;
         return (FIELD4 *)0 ;
      }
   #endif

   return data->fields + jField - 1 ;
}

FIELD4 *S4FUNCTION d4fieldNull( DATA4 *data )
{
   /* returns the null field, if any, else NULL */
   #ifdef S4CLIENT_OR_FOX
      int lastFieldIndex = data->dataFile->nFields - 1 ;

      if ( data->fields[lastFieldIndex].type == '0' )   /* null flags field */
         return &(data->fields[lastFieldIndex]) ;
   #endif

   return 0 ;
}

int S4FUNCTION d4fieldNumber( DATA4 *data, const char *fieldName )
{
   char fieldNameNormalized[256] ;
   int fieldIterator ;

   #ifdef E4PARM_HIGH
      if ( data == 0 )
         return error4( 0, e4parm_null, E94003 ) ;
   #endif

   if ( fieldName )
   {
      u4ncpy( fieldNameNormalized, fieldName, sizeof( fieldNameNormalized ) ) ;
      c4trimN( fieldNameNormalized, sizeof( fieldNameNormalized ) ) ;
      c4upper( fieldNameNormalized ) ;

      for ( fieldIterator = 0 ; fieldIterator < data->dataFile->nFields ; fieldIterator++ )
         if ( !c4strcmp( fieldNameNormalized, data->fields[fieldIterator].name ) )
            return fieldIterator + 1 ;
   }

   if ( data->codeBase->errFieldName )
      return error4describe( data->codeBase, e4fieldName, E94003, fieldName, 0, 0 ) ;
   return e4fieldName ;
}

#ifdef S4FOX
   int S4FUNCTION f4nullable( const FIELD4 *field )
   {
      return field->null ;
   }
#endif
