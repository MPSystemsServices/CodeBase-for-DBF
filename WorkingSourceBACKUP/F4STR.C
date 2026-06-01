/* f4str.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* Returns a pointer to static string corresponding to the field.
   This string will end in a NULL character.
*/

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4OFF_WRITE
static void f4assignNumericN( FIELD4 *field, const char *ptr, const unsigned ptrLen )
{
   /* special function for assigning to numeric field such that the data is shifted to the left which
      is required for dbase standard, and in particular the descend() function.  AS 03/29/99
      internal function only
   */

   char *fieldRecordPtr = f4assignPtr( field ) ;

   unsigned short numBytesToCopy ;
   unsigned short offset ;   // the offset into the buffer to precede the data with blanks...

   if ( (unsigned long)ptrLen > (unsigned long)field->len )
   {
      numBytesToCopy = field->len ;
      offset = 0 ;
   }
   else
   {
      numBytesToCopy = ptrLen ;
      offset = field->len - numBytesToCopy ;

      // in this case, also need to blank out extra bytes in field.
      // use f4blank() because of possible null
      f4blank( field ) ;
   }

   /* Copy the data into the record buffer. */
   c4memcpy( fieldRecordPtr + offset, ptr, (size_t)numBytesToCopy ) ;
}




void S4FUNCTION f4assign( FIELD4 *field, const char *str )
{
   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90533 ) )
         return ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 || str == 0 )
      {
         error4( 0, e4parm_null, E90533 ) ;
         return ;
      }
   #endif

   #ifdef E4ANALYZE
      if ( field->data == 0 )
      {
         error4( 0, e4struct, E90533 ) ;
         return ;
      }
      if ( field->data->codeBase == 0 )
      {
         error4( 0, e4struct, E90533 ) ;
         return ;
      }
   #endif

   if ( error4code( field->data->codeBase ) < 0 )
      return ;

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
            if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
            {
               error4( field->data->codeBase, e4lock, E90533 ) ;
               return ;
            }
      #endif
   #endif

   /* AS  03/29/99 - in order for numeric fields to be valid, they must be right shifted to the end of the
      field.  Otherwise, function descend() produces inconsistent results (and also the field is invalid).
      However, it is nice to assign "1" to a field.  Therefore massage the field contents here... */
   switch( field->type )
   {
      case r4num:
      case r4float:
         f4assignNumericN( field, str, (unsigned)c4strlen(str) ) ;
         break ;
      default:
         f4assignN( field, str, (unsigned)c4strlen(str) ) ;
         break ;
   }
}



void S4FUNCTION f4assignN( FIELD4 *field, const char *ptr, const unsigned ptrLen )
{
   char *fPtr ;
   unsigned pLen ;

   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90534 ) )
         return ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 || ( ptr == 0 && ptrLen ) )
      {
         error4( 0, e4parm_null, E90534 ) ;
         return ;
      }
   #endif

   if ( error4code( field->data->codeBase ) < 0 )
      return ;

   #ifndef S4SERVER
      #ifndef S4OFF_ENFORCE_LOCK
         if ( field->data->codeBase->lockEnforce && field->data->recNum > 0L )
            if ( d4lockTest( field->data, field->data->recNum, lock4write ) != 1 )
            {
               error4( field->data->codeBase, e4lock, E90534 ) ;
               return ;
            }
      #endif
   #endif

   fPtr = f4assignPtr( field ) ;

   if ( (unsigned long)ptrLen > (unsigned long)field->len )
      pLen = field->len ;
   else
   {
      pLen = ptrLen ;

      // in this case, also need to blank out extra bytes in field.
      // use f4blank() because of possible null
      f4blank( field ) ;
   }

   /* Copy the data into the record buffer. */
   c4memcpy( fPtr, ptr, (size_t)pLen ) ;
}
#endif



unsigned long S4FUNCTION f4ncpy( FIELD4 *field, char *memPtr, const unsigned int memLen )
{
   unsigned numCpy ;

   if ( memLen == 0 )
      return 0 ;

   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90535 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 || memPtr == 0 )
      {
         error4( 0, e4parm_null, E90535 ) ;
         return 0 ;
      }
   #endif

   numCpy = field->len ;
   if ( memLen <= numCpy )
      numCpy = memLen - 1 ;

   /* 'f4ptr' returns a pointer to the field within the database record buffer. */
   c4memcpy( memPtr, f4ptr( field ), (size_t)numCpy ) ;

   memPtr[numCpy] = '\000' ;

   return numCpy ;
}

char *S4FUNCTION f4str( FIELD4 *field )
{
   CODE4 *codeBase ;

   #ifdef E4VBASIC
      if ( c4parm_check( field, 3, E90536 ) )
         return 0 ;
   #endif

   #ifdef E4PARM_HIGH
      if ( field == 0 )
      {
         error4( 0, e4parm_null, E90536 ) ;
         return 0 ;
      }
   #endif

   codeBase = field->data->codeBase ;

   if ( codeBase->bufLen < (unsigned)(field->len + 2))  // CS 1999/09/10 buffer length = field length + Unicode null (2)
   {
      if ( u4allocAgain( codeBase, &codeBase->fieldBuffer, &codeBase->bufLen, field->len + 2 ) < 0 )
      {
         #ifdef E4STACK
            error4stack( codeBase, e4memory, E90536 ) ;
         #endif
         return 0 ;
      }
   }

   // AS 02/07/01 - We always need to null out these value
   // else
   // {
      codeBase->fieldBuffer[(field->len) + 0] = 0 ;  // CS 1999/09/10 2 nulls for Unicode
      codeBase->fieldBuffer[(field->len) + 1] = 0 ;
   // }

   c4memcpy( codeBase->fieldBuffer, f4ptr( field ), field->len ) ;
   return codeBase->fieldBuffer ;
}

#ifdef S4VB_DOS

void f4assign_v( FIELD4 *fld, const char *data )
{
   f4assignN ( fld, data, StringLength((char near *)data) ) ;
}

int f4assignN ( FIELD4 *fld, const char *data, const int len )
{
   char *cBuf;

   if( (cBuf = (char *) u4alloc(len + 1) ) )
   {
      u4vtoc( cBuf, len+1, data ) ;
      f4assignN( fld, cBuf, len ) ;
      u4free( cBuf );
   }
   else
      return error4( fld->codeBase, e4memory, E90534 );
}

char *f4str_v( FIELD4 *fld )
{
   return v4str( f4str(fld) ) ;
}

#endif

