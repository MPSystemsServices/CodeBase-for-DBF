/*ex111.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

typedef struct
{
   short id ;
   char password[9] ;
} MY_STRUCT ;

int readUserInfo( FILE4 *file, MY_STRUCT *ms, int user)
{
   int rc ;
   rc = file4readAll( file, user*sizeof(MY_STRUCT), ms, sizeof(MY_STRUCT) ) ;
   if( rc != 0 )
   {
      printf( "Could not read user # %d\n", user ) ;
      return rc ;
   }
   printf( "id: %d password %s\n", ms->id, ms->password ) ;
   return 0 ;
}

void main()
{
   CODE4 cb ;
   FILE4 testFile ;
   MY_STRUCT *info ;
   int userNum ;

   code4init( &cb  );
   file4open( &testFile, &cb, "TEST.FIL", 0 ) ;
   info = ( MY_STRUCT * )malloc( sizeof( MY_STRUCT ) ) ;
   for (userNum = 0; userNum <= 9 ; userNum++ )
      readUserInfo( &testFile, info, userNum ) ;
   file4close( &testFile ) ;
   code4initUndo( &cb ) ;
}
