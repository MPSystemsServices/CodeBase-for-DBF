/* DBFENCRYPT.C    (c)Copyright Sequiter Inc., 1988-2009.
All rights reserved. */

#include "d4all.h"

#ifndef ENCRYPT
   #error ENCRYPT not defined. Should be 1 or 0.
#endif

#if ENCRYPT == 1
   #define TITLE "DBFEncrypt"
   #define ENC_OUTPUT 1
#endif
#if ENCRYPT == 0
   #define TITLE "DBFDecrypt"
   #define ENC_OUTPUT 0
#endif

typedef struct
{
   // AS Jan 15/03 - Need to be able to re-retrieve the source pointers in some cases.
   // The previous coding was altering the input paramaters.  Instead make a copyu
   char parmsCopy[512] ;
   LPSTR  ptr ;
} D4PARSE_STR;

void d4parseStringInit( D4PARSE_STR *pStr, LPSTR p )
{
   memset( pStr, 0, sizeof(D4PARSE_STR) ) ;

   // AS Jan 15/03 - Need to be able to re-retrieve the source pointers in some cases.
   // The previous coding was altering the input paramaters.  Instead make a copyu
   if ( p != 0 && strlen( p ) < sizeof( pStr->parmsCopy ) )
   {
      strcpy( pStr->parmsCopy, p ) ;
      pStr->ptr = pStr->parmsCopy ;
   }
   else
      pStr->ptr = p ;
}

char *d4parseStringNParm( D4PARSE_STR *pStr )
{
   unsigned i ;
   LPSTR newPtr ;

   if ( pStr->ptr == 0 )
      return (char *)pStr->ptr ;

   while (pStr->ptr[0] == ' ' )
   {
      pStr->ptr++ ;
   }

   for ( i = 0; ; i++ )
   {
      if ( pStr->ptr[i] == 0 || pStr->ptr[i] == ' ' )
      {
         newPtr = pStr->ptr ;
         if( pStr->ptr[i] != 0 )
         {
            pStr->ptr += i+1 ;
            *(newPtr+i) = '\0' ;
         }
         else
            pStr->ptr += i ;
         return (char *)newPtr ;
      }
   }
}

void displayUsage()
{
   char messageBuff[512] ;
   strcpy(messageBuff, TITLE);
   strcat( messageBuff, "\n(c)Copyright Sequiter Inc., 1988-2009. All rights reserved.\n\n" ) ;
   strcat( messageBuff, "Usage:\n\n\t");
   strcat(messageBuff, TITLE);
   strcat( messageBuff, " <originalDBFname> <newDBFname> [encryptionkey|@encryptionkeyfile]\n\n" ) ;
   strcat( messageBuff, "[encryptionkey] must be 1, 16, 24, or 32 bytes in length.\n" ) ;
   strcat( messageBuff, "[@encryptionkeyfile] must be simple text file containing encryption key in same format as [encryptionkey]." ) ;

   MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
}

int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE prevInstance, char *cmdLine, int cmdShow )
{
   CODE4 cb ;
   DATA4 *oldDb, *newDb ;
   int numFld ;
   short i;
   FIELD4 *oldFld, *newFld ;
   INDEX4 *index ;
   FIELD4INFO *fldInfo = 0 ;
   TAG4INFO *tagInfo = 0 ;
   FILE *file ;
   char *oldDbfName = 0, *newDbfName = 0, *encryptKey = 0 ;
   char encryptKeyFile[LEN4PATH+1], fileKey[33], messageBuff[255] ;
   D4PARSE_STR parms ;

   if ( strlen( cmdLine ) < 1 )
   {
      displayUsage() ;
      return 0 ;
   }

   d4parseStringInit( &parms, cmdLine ) ;
   oldDbfName = d4parseStringNParm( &parms ) ;
   if ( !oldDbfName || !strlen( oldDbfName ) )
   {
      displayUsage() ;
      return 0 ;
   }

   newDbfName = d4parseStringNParm( &parms ) ;
   if ( !newDbfName || !strlen( newDbfName ) )
   {
      displayUsage() ;
      return 0 ;
   }

   encryptKey = d4parseStringNParm( &parms ) ;
   if ( encryptKey && strlen( encryptKey ) )
   {
      if ( *encryptKey == '@' )
      {
         int encryptKeyFileNameLength;
         encryptKeyFileNameLength = min( strlen( encryptKey ) - 1, LEN4PATH );
         strncpy( encryptKeyFile, encryptKey + 1, encryptKeyFileNameLength ) ;
         encryptKeyFile[encryptKeyFileNameLength] = 0;
         file = fopen( encryptKeyFile, "r" ) ;
         if ( !file )
         {
            sprintf( messageBuff, "Error: could not open %s\nVerify that the file exists\n\nExiting application\0",
               encryptKeyFile ) ;
            MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
            return 0 ;
         }

         if ( !fgets( fileKey, 33, file ) )
         {
            sprintf( messageBuff, "Error: could not read %s\nVerify that the file is not empty\n\nExiting application\0",
               encryptKeyFile ) ;
            fclose( file ) ;
            MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
            return 0 ;
         }

         fileKey[32] = 0 ;
         fclose( file ) ;
         encryptKey = fileKey ;
      }

		switch ( strlen( encryptKey ) )
      {
			case 1:
			case 16:
			case 24:
			case 32:
				break;
			default:
				displayUsage() ;
				return 0 ;
      }
   }
   else
      encryptKey = 0 ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   cb.errOpen = 0 ;
   cb.errCreate = 0 ;
   code4largeOn( &cb ) ;

   cb.errOff = 1 ;
   if ( encryptKey )
      code4encryptInit( &cb, encryptKey, (short)strlen( encryptKey ) ) ;
   code4encryptFile( &cb, ENC_OUTPUT ) ;
   cb.errOff = 0 ;

   if ( cb.errorCode != 0 )
   {
      if ( cb.errorCode == -1090 )
         sprintf( messageBuff, "Error: encryption support DLL cannot be loaded.\nVerify that the DLL is in the same directory as the EXE.\n\nExiting application.\0" ) ;
      else
         sprintf( messageBuff, "Error: code %d while opening %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, oldDbfName ) ;
      MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
      code4initUndo( &cb ) ;
      return 0 ;
   }

   oldDb = d4open( &cb, oldDbfName ) ;
   if ( cb.errorCode != 0 )
   {
      if ( cb.errorCode == r4noOpen )
         sprintf( messageBuff, "Error: could not open %s.\nVerify that the file exists and is not already opened by another application.\n\nExiting application.\0",
            oldDbfName ) ;
      else
         sprintf( messageBuff, "Error: code %d while opening %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, oldDbfName ) ;
      MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
      code4initUndo( &cb ) ;
      return 0 ;
   }

   fldInfo = d4fieldInfo( oldDb ) ;
   if ( !fldInfo )
   {
      sprintf( messageBuff, "Error: code %d while obtaining field information from %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
         cb.errorCode, oldDbfName ) ;
      MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
      code4initUndo( &cb ) ;
      return 0 ;
   }

   index = d4index( oldDb, 0 ) ;
   if ( index )
   {
      tagInfo = i4tagInfo( index ) ;
      if ( !tagInfo )
      {
         sprintf( messageBuff, "Error: code %d while obtaining tag information from %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, oldDbfName ) ;
         MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
         if ( fldInfo )
            u4free( fldInfo ) ;
         code4initUndo( &cb ) ;
         return 0 ;
      }
   }

   newDb = d4create( &cb, newDbfName, fldInfo, tagInfo ) ;

   if ( cb.errorCode != 0 )
   {
      if ( cb.errorCode == r4noCreate )
         sprintf( messageBuff, "Error: could not create %s. Check that the file does not already exist.\n\nExiting application.\0",
            newDbfName ) ;
      else
         sprintf( messageBuff, "Error: code %d while creating %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, newDbfName ) ;
      MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
      if ( fldInfo )
         u4free( fldInfo ) ;
      if ( tagInfo )
         u4free( tagInfo ) ;
      code4initUndo( &cb ) ;
      return 0 ;
   }

   numFld = d4numFields( oldDb ) ;
   for ( d4top( oldDb ) ; !d4eof( oldDb ) ; d4skip( oldDb, 1 ) )
   {
      if ( d4appendStart( newDb, 1 ) != r4success )
      {
         sprintf( messageBuff, "Error: code %d while copying record to %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, newDbfName ) ;
         MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
         if ( fldInfo )
            u4free( fldInfo ) ;
         if ( tagInfo )
            u4free( tagInfo ) ;
         code4initUndo( &cb ) ;
         return 0 ;
      }
      for ( i = 1 ; i <= numFld ; i++ )
      {
         oldFld = d4fieldJ( oldDb, i ) ;
         newFld = d4fieldJ( newDb, i ) ;
         f4memoAssign( newFld, f4memoStr( oldFld ) ) ;
      }
      if ( d4append( newDb ) != r4success )
      {
         sprintf( messageBuff, "Error: code %d while copying record to %s.\nReport this code to technical support for assistance.\n\nExiting application.\0",
            cb.errorCode, newDbfName ) ;
         MessageBox( 0, messageBuff, TITLE, MB_OK ) ;
         if ( fldInfo )
            u4free( fldInfo ) ;
         if ( tagInfo )
            u4free( tagInfo ) ;
         code4initUndo( &cb ) ;
         return 0 ;
      }
   }

   sprintf(messageBuff, "%s created successfully.", newDbfName);
   MessageBox( 0, messageBuff, TITLE, MB_OK ) ;

   if ( fldInfo )
      u4free( fldInfo ) ;
   if ( tagInfo )
      u4free( tagInfo ) ;

   code4initUndo( &cb ) ;

   return 0 ;
}
