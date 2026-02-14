/* c4long.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* this file is included directly... */

#ifndef S4NO_LONGLONG
char *c4longlongToA( char *out, LONGLONG val )
{
   // assumes out is large enough to hold a longlong conversion (i.e. 21 bytes if null included)
   // returns a pointer to the result, which will somewhere in 'out'
   int isNeg ;

   out += 20 ;

   out[0] = 0 ;  // place the null byte
   out-- ;

   if ( val < 0 )
      isNeg = 1 ;
   else
      isNeg = 0 ;

   if ( val == 0 )
   {
      out[0] = '0' ;
      return out ;
   }

   while ( val != 0 )
   {
      char valMod10 = (char)( val % 10 ) ;
      out[0] = '0' + (isNeg ? -valMod10 : valMod10 ) ;
      val = val / 10 ;
      out-- ;
   }

   if ( isNeg )
   {
      out[0] = '-' ;
      out-- ;
   }

   return out + 1 ;
}
#endif
