/*ex116.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct myStructSt
{
   short id ;
   char password[9] ;
} MY_STRUCT ;

void main( void )
{
   CODE4 cb ;
   FILE4SEQ_WRITE writePassFile ;
   FILE4SEQ_READ readPassFile ;
   FILE4 passFile ;
   MY_STRUCT person ;
   char buffer[ 0x1400 ] ; /* 5K bytes... space for 200 structures */
   int i ;

   code4init( &cb ) ;
   cb.safety = 0 ;

   file4create( &passFile, &cb, "TEST.FIL", 0 ) ;
   file4seqWriteInit( &writePassFile, &passFile, 0, buffer, sizeof( buffer ) ) ;
   for( i = 10 ; i ; i-- )
   {
      person.id = i ;
      strcpy( person.password, "PASSWORD") ;
      person.password[8] = 0 ;
      file4seqWrite( &writePassFile, &person, sizeof( MY_STRUCT ) ) ;
   } /* physically write only once.*/
   file4seqWriteFlush( &writePassFile ) ;

   file4seqReadInit( &readPassFile, &passFile, 0, buffer, sizeof(buffer) );

   for( i = 10 ; i ; i -- )
   {
      /* only one physical read occurs... the rest are in memory*/
      file4seqRead( &readPassFile, &person, sizeof( MY_STRUCT ) ) ;
      printf( "id: %d password: %s\n", person.id, person.password ) ;
   }
   file4close( &passFile ) ; /* writePassFile and readPassFile are invalid now*/
   code4initUndo( &cb ) ;
}
