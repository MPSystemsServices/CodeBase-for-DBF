/* c4hook.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif  /* __TUROBC__ */
#endif  /* S4UNIX */



#ifdef S4TIMEOUT_HOOK
int code4timeoutHook( CODE4 *c4, int numAttempts, long elapsedTime )
{
   return 0 ;
}
#endif



#ifdef S4LOCK_HOOK
int code4lockHook( CODE4 *c4, const char *fileName, const char *userId, const char *networkId, long item, int numAttempts )
{
   return 0 ;
}
#endif



#ifndef S4TEST
#ifdef E4HOOK
/* Uncomment and add alternative error display code here.
   Function error4hook() may be placed in your own separate source code
   file, but ensure that the same function prototype as below is used.
void S4FUNCTION error4hook( CODE4 *c4, int errCode, long errCode2, const char *desc1, const char *desc2, const char *desc3 )
{
   return ;
}
*/
#endif
#endif




#ifdef S4ENCRYPT_HOOK

static long g_code4encryptKey ;
const void *code4encryptGetKey( short *outLen )
{
   // used to obtain the key. at initialization time.
   // for us, we will just hard-code it for now to a value

   g_code4encryptKey = 12345 ;
   *outLen = sizeof( g_code4encryptKey ) ;
   return &g_code4encryptKey ;
}


// the code included here is a simple sample encryption.  The functions are hook functions that are
// called by the CodeBase engine, and may be modified to provide superior encryption (and in particular
// your own encryption which is not coded here, and hence easily hacked)


// this array is used as the standard 1 to 1 encryption.  It is modified based on the key
// this array is used as part of the sample encryption, and is not required if an alternate encryption
// schema is created
static unsigned char g4arraySource[256] =
{
   // this was just created as a random sequence.  keys could be substituted to make a more private
   // implementation of the encryption
     0x7d,   0xB4,   0x79,   0x7f,   0x34,   0x7e,   0x6e,   0x5f,
     0xF5,   0xF1,   0xF2,   0xF7,   0xBc,   0xF6,   0x8e,   0xEf,
     0x05,   0x01,   0x02,   0x07,   0x44,   0x06,   0x96,   0x67,
     0x15,   0x11,   0x12,   0x17,   0xCc,   0x16,   0x4e,   0x6f,
     0x25,   0x21,   0x22,   0x27,   0xD4,   0x26,   0xA6,   0x8f,
     0xFd,   0xF9,   0xFa,   0xFf,   0xE4,   0xFe,   0xDc,   0x97,
     0x75,   0x70,   0x71,   0x73,   0x77,   0x76,   0x1c,   0x9f,
     0x2d,   0x28,   0x29,   0x2b,   0x2f,   0x2e,   0x54,   0x4f,
     0x3d,   0x38,   0x39,   0x3b,   0x3f,   0x3e,   0xAc,   0xA7,
     0x4d,   0x4c,   0x48,   0x13,   0x49,   0x4a,   0x4b,   0x2c,
     0xDd,   0xD8,   0xD9,   0xDb,   0xDf,   0xDe,   0xCe,   0x37,
     0x1d,   0x18,   0x19,   0x1b,   0x1f,   0x1e,   0xD6,   0xBf,
     0x55,   0x50,   0x51,   0x53,   0x57,   0x56,   0xE6,   0x42,
     0xAd,   0xA8,   0xA9,   0xAb,   0xAf,   0xAe,   0xB7,   0xC2,
     0x5d,   0x5c,   0x58,   0x59,   0x5a,   0x5b,   0x5e,   0x7c,
     0xEd,   0xEc,   0xE8,   0xE9,   0xEa,   0xEb,   0xEe,   0xF4,
     0x65,   0x64,   0x60,   0x61,   0x62,   0x63,   0x66,   0x04,
     0x85,   0x84,   0x80,   0x81,   0x82,   0x83,   0x86,   0x0c,
     0xB5,   0xB0,   0xB1,   0xAa,   0xB2,   0x7b,   0xB3,   0xB6,
     0x6d,   0x6c,   0x68,   0x7a,   0x69,   0x6a,   0x6b,   0x14,
     0x8d,   0x8c,   0x88,   0xF3,   0x89,   0x8a,   0x8b,   0x24,
     0x95,   0x94,   0x90,   0x03,   0x91,   0x92,   0x93,   0xFc,
     0x9d,   0x9c,   0x98,   0x0b,   0x99,   0x9a,   0x9b,   0x74,
     0xA5,   0xA4,   0xA0,   0x23,   0xA1,   0xA2,   0xA3,   0x3c,
     0x35,   0x30,   0x31,   0xFb,   0x32,   0x33,   0x36,   0x78,
     0xBd,   0xB8,   0xB9,   0x72,   0xBa,   0xBb,   0xBe,   0xF0,
     0x45,   0x40,   0x41,   0x2a,   0x43,   0x47,   0x46,   0x00,
     0xC5,   0xC0,   0xC1,   0x3a,   0xC3,   0xC7,   0xC6,   0x08,
     0xCd,   0xC8,   0xC9,   0xDa,   0xCa,   0xCb,   0xCf,   0x10,
     0xD5,   0xD0,   0xD1,   0x1a,   0xD2,   0xD3,   0xD7,   0x20,
     0x0d,   0x09,   0x0a,   0x0f,   0xC4,   0x0e,   0x9e,   0x87,
     0xE5,   0xE0,   0xE1,   0x52,   0xE2,   0xE3,   0xE7,   0xF8,
} ;



// this ENCRYPT4 structure is what CodeBase uses for its encryption
typedef struct
{
   unsigned char encryptArray[256] ;
   unsigned char decryptArray[256] ;
   unsigned char keyModifier ;
} ENCRYPT4 ;



void *code4encryptInitHook( CODE4 *c4, const void *key, short keyLen )
{
   // initializations on CODE4 level for encryption.
   // returns a pointer to data which is allocated here and can be used for encryption purposes
   // the data is passed to all the encryption functions
   // returns '0' if failure or if no encryption is to occur.
   // c4->errorCode should be set to negative value in case of failure.

   // note that this encryption must be on a byte by byte basis (i.e. may be reading/writing only 1 byte)
   #ifdef E4PARM_HIGH
      if ( c4 == 0 || key == 0 || keyLen <= 0 )
      {
         error4( c4, e4parm, E91001 ) ;
         return 0 ;
      }
   #endif

   // we have set up an ENCRYPT4 structure to store our encryption information
   ENCRYPT4 *encrypt = (ENCRYPT4 *)u4allocFree( c4, sizeof( ENCRYPT4 ) ) ;
   if ( encrypt == 0 )
      return 0 ;
   memset( encrypt, 0, sizeof( ENCRYPT4 ) ) ;

   // we use a simple permutation of 256.  To get the permutation, add all the members of 'key' together,
   // and take the remainder of dividing by 256

   for ( short keyIndex = 0 ; keyIndex < keyLen ; keyIndex++ )
   {
      encrypt->keyModifier += ((unsigned char *)key)[keyIndex] ;  // will roll over naturally
   }

   // now do the roll-over...
   for ( short arrayIndex = 0 ; arrayIndex < 256 ; arrayIndex++ )
   {
      unsigned char sourceArrayIndex = arrayIndex + encrypt->keyModifier ;
      encrypt->encryptArray[arrayIndex] = g4arraySource[sourceArrayIndex] ;
      encrypt->decryptArray[g4arraySource[sourceArrayIndex]] = (unsigned char)arrayIndex ;
   }

   return encrypt ;
}



void code4encryptInitUndoHook( CODE4 *c4, void *encryptInit )
{
   // encryptInit - pointer that was returned from code4encryptInit
   // uninitializations on CODE4 level for encryption.
   // usually just frees up memory allocated for encyptInit structure (if any)

   u4free( encryptInit ) ;
}



short code4encryptEnabledHook( CODE4 *c4, FILE4 *file, void *encryptInit )
{
   // called for every function on open/create
   // returns true if the file is to be treated as encrypted, false if not
   // encryptInit - pointer that was returned from code4encryptInit

   // for our implementation just use the default CODE4 provided value...
   return c4->encrypt ;
}



void code4encryptHook( CODE4 *c4, void *encryptInit, FILE4 *file, long fileOffset, const void *dataSource, long dataLen, void *dataResult )
{
   // encryptInit - pointer that was returned from code4encryptInit
   // dataSource is a pointer to the data to encrypt.
   // dataResult is a pointer to the encrypted data.  It must be the same length as dataLen
   // fileOffset is the fileOffsetition in the file where the data will be written
   // dataLen is the length of the data

   // a relatively simple algorithm, we use a combination of the key and the fileOffsetition to determine the algorithm
   // to use (one of four)
   ENCRYPT4 *encrypt = (ENCRYPT4 *)encryptInit ;
   #ifdef E4PARM_LOW
      if ( c4 == 0 || encrypt == 0 || encryptInit == 0 || file == 0 )
      {
         error4( c4, e4parmNull, E91001 ) ;
         return ;
      }
   #endif

   for ( long charIndex = 0 ; charIndex < dataLen ; charIndex++ )
   {
      long filePos = fileOffset + charIndex ;
      short encryptor = ( encrypt->keyModifier + filePos ) % 4 ;

      unsigned char charUnencrypted = ((unsigned char *)dataSource)[charIndex] ;
      charUnencrypted += (filePos) % (g4arraySource[filePos%256] +1) ;   // further alter the array entry based on file position, to ensure obvious patterns don't exist
      unsigned char encryptChar ;
      switch( encryptor )
      {
         case 0:
            encryptChar = encrypt->encryptArray[charUnencrypted] ;
            break ;
         case 1:
            encryptChar = ~encrypt->encryptArray[charUnencrypted] ;
            break ;
         case 2:
            encryptChar = encrypt->encryptArray[charUnencrypted] ;
            encryptChar = ( encryptChar >> 4 ) + ( encryptChar << 4 ) ;
            break ;
         case 3:
            encryptChar = encrypt->encryptArray[charUnencrypted] ;
            encryptChar = ~( ( encryptChar >> 4 ) + ( encryptChar << 4 ) ) ;
            break ;
      }
      ((unsigned char *)dataResult)[charIndex] = encryptChar ;
   }
}



void code4decryptHook( CODE4 *c4, void *encryptInit, FILE4 *file, long fileOffset, const void *dataSource, long dataLen, void *dataResult )
{
   // encryptInit - pointer that was returned from code4encryptInit
   // dataSource is a pointer to the data to decrypt.
   // dataResult is a pointer to the decrypted data.  It must be the same length as dataLen
   // fileOffset is the fileOffsetition in the file where the data was read from
   // dataLen is the length of the data
   ENCRYPT4 *encrypt = (ENCRYPT4 *)encryptInit ;
   #ifdef E4PARM_LOW
      if ( c4 == 0 || encrypt == 0 || encryptInit == 0 || file == 0 )
      {
         error4( c4, e4parmNull, E91001 ) ;
         return ;
      }
   #endif

   for ( long charIndex = 0 ; charIndex < dataLen ; charIndex++ )
   {
      long filePos = fileOffset + charIndex ;
      short encryptor = ( encrypt->keyModifier + filePos ) % 4 ;

      unsigned char charEncrypted =  ((unsigned char *)dataSource)[charIndex] ;
      unsigned char unencryptedChar ;
      switch( encryptor )
      {
         case 0:
            unencryptedChar = encrypt->decryptArray[charEncrypted] ;
            unencryptedChar -= (filePos) % (g4arraySource[filePos%256] +1) ;
            break ;
         case 1:
            unencryptedChar = ~charEncrypted ;
            unencryptedChar = encrypt->decryptArray[unencryptedChar] ;
            unencryptedChar -= (filePos) % (g4arraySource[filePos%256] +1) ;
            break ;
         case 2:
            unencryptedChar = ( charEncrypted >> 4 ) + ( charEncrypted << 4 ) ;
            unencryptedChar = encrypt->decryptArray[unencryptedChar] ;
            unencryptedChar -= (filePos) % (g4arraySource[filePos%256] +1) ;
            break ;
         case 3:
            unencryptedChar = ~( ( charEncrypted >> 4 ) + ( charEncrypted << 4 ) ) ;
            unencryptedChar = encrypt->decryptArray[unencryptedChar] ;
            unencryptedChar -= (filePos) % (g4arraySource[filePos%256] +1) ;
            break ;
      }
      ((unsigned char *)dataResult)[charIndex] = unencryptedChar ;
   }
}

#endif /* #ifdef S4ENCRYPT_HOOK */
