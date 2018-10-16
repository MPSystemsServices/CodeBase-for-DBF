#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  /* for all Borland compilers*/
#endif

void main( )
{
   CODE4 codeBase ;
	DATA4 *newDataFile ;
	FIELD4INFO fieldInfo [] =
	{
		{"NAME", 'C', 20, 0},
		{"AGE", 'N', 3, 0},
		{"BIRTHDATE", 'D', 8, 0},
		{0, 0, 0, 0},
	};

	code4init( &codeBase );
   codeBase.accessMode = OPEN4DENY_RW ;
   codeBase.safety = 0 ;  /* Ensure the create overwrites any existing file*/

   newDataFile = d4create( &codeBase, "NEWDBF", fieldInfo, 0 ) ;

   d4close( newDataFile ) ;

  /* open in shared mode*/
   codeBase.accessMode = OPEN4DENY_NONE ;
   newDataFile = d4open( &codeBase, "NEWDBF" ) ;

   /* ... some other code ...*/

   code4close( &codeBase ) ;
   code4initUndo( &codeBase) ;
}
