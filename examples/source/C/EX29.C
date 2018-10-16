#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void disp( char *ptr )
{
	char buf[80] ;
	memcpy( buf, ptr, sizeof(buf) ) ;

	/* A null will be placed in the 80th byte of 'buf' to guarantee that it is null terminated */

	c4trimN( buf, sizeof(buf) ) ;
	printf( "Display Result: %s<-end of string after c4trimN\n", buf) ;
}


void main()
{
   char string1[] = "Hello, my Name is Joesph. I am here to demonstrate the c4trimN function        " ;
   char string2[] = "Hello, my Name is Joesph. I am here to demonstrate the c4trimN function. This string will exceed the buffer" ;

	printf( "Display Result: %s<-end of string before c4trimN\n", string1) ;
   disp( string1 ) ;

   /* Note: The second string would display a 't' at the end if the buffer was not NULL terminated. */
	printf( "Display Result: %s<-end of string before c4trimN\n", string2) ;   
   disp( string2 ) ;

   return ;
}
