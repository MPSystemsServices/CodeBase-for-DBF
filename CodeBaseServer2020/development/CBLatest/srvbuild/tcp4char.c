/* tcp4char.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* tbis file is included directly in files needing this function... */

int tcp4charToAddress( const char *charFrom, unsigned char maxLenFrom, TCP4ADDRESS *address )
{
   unsigned char cnt = 0 ;

   unsigned char i ;
   for ( i = 0 ; i < 4 ; i++ )
   {
      unsigned char val = 0 ;
      for( ;; cnt++ )
      {
         unsigned char on ;
         if ( cnt == maxLenFrom )
         {
            if ( i == 3 )  // done
            {
               address->tcpAddress[i] = val ;
               return 0 ;
            }
            if ( i == 0 )  // check for blank address
               if ( ( memcmp( charFrom, "               ", 15 ) == 0 ) || ( memcmp( charFrom, "   .   .   .   ", 15 ) == 0 ) )
               {
                  memset( address, 0, sizeof( TCP4ADDRESS ) ) ;  // mark as blank or 0 address
                  return r4blankTcpAddress ;
               }
            return e4invalidTcpAddress ;
         }
         on = charFrom[cnt] ;
         if ( on == '.' )
         {
            if ( i == 3 )  // too many '.'s
               return e4invalidTcpAddress ;
            cnt++ ;
            break ;
         }
         if ( on == ' ' )
            continue ;
         if ( val > 100 )  // failure
         {
            cnt = maxLenFrom ;
            i = 0 ;
            break ;
         }
         if ( on < '0' || on > '9' )
            if ( on != ' ' )
               return e4invalidTcpAddress ;
         val = val * 10 + (on - '0') ;
      }
      address->tcpAddress[i] = val ;
      continue ;
   }

   return 0 ;
}


