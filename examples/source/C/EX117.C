/*ex117.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct myStructSt
{
   short id ;
   char password[9] ;
} MY_STRUCT ;

int getNextStructure( FILE4SEQ_READ *seqFile, MY_STRUCT *ms )
{
   if( file4seqRead( seqFile, ms, sizeof( MY_STRUCT ) ) != sizeof( MY_STRUCT ) )
   {
      memset( ms, 0, sizeof(MY_STRUCT ) ) ;
      return 1 ;
   }
   return 0 ;
}

void main()
{
   CODE4 cb ;
   FILE4 passFile ;
   FILE4SEQ_READ readFile ;
   MY_STRUCT *person ;
   char buffer[0x1400] ;

   code4init( &cb ) ;
   file4open( &passFile, &cb, "TEST.FIL", 0 ) ;
   file4seqReadInit( &readFile, &passFile, 0, buffer, sizeof(buffer) );
   person = ( MY_STRUCT *) malloc( sizeof( MY_STRUCT ) ) ;
   getNextStructure( &readFile, person ) ;
   if ( person != NULL )
      printf( "id: %d password: %s\n", person->id, person->password ) ;

   file4close( &passFile ) ;
   code4initUndo( &cb  ) ;
}
