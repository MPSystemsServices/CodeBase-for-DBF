/* d4data_p.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.hpp"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */



Data4 Index4::data()
{
   DEBUG4PTR(index==0, 60704L)
   return Data4(index->data) ;
}



#ifndef S4JNI  /* LY 99/07/08 */
   void Field4memo::changed()
   {
      DEBUG4VOID(field == 0, E60523)
      field->data->recordChanged = 1 ;
      #ifndef S4OFF_MEMO
         if ( field->memo != 0 )
            field->memo->isChanged =  1 ;
      #endif
   }
#endif



#ifdef __BORLANDC__
   #pragma warn -aus
   #pragma warn -par
#endif

#ifndef S4JNI  /* LY 99/07/08 */
   int Field4memo::setLen( unsigned newLen )
   {
      #ifdef S4OFF_MEMO
         return e4notMemo ;
      #else
         #ifdef S4OFF_WRITE
            return e4notWrite ;
         #else
            #ifdef E4PARM_HIGH
               if ( field == 0 ) return error4( 0, e4struct, ( E60525 ) ) ;
            #endif
            int rc ;
            char *buf = 0 ;

            if( field->memo == 0 )
               rc = -1 ;
            else
            {
               if( newLen > field->memo->lenMax )
               {
                  buf = ( char * ) u4allocFree( ( CODE4 * ) field->data->codeBase, newLen ) ;
                  memcpy( buf, field->memo->contents, field->memo->lenMax ) ;
               }
               rc = f4memoSetLen( field,newLen ) ;
               if( buf != 0 )
               {
                  memcpy( field->memo->contents, buf, field->memo->lenMax ) ;
                  u4free( buf ) ;
               }
            }
            return rc ;
         #endif
      #endif
   }



   Field4memo::Field4memo() : Field4()
   {
   }



   Field4memo::Field4memo( Data4& data, int j ) : Field4( data, (short)j )
   {
   }



   Field4memo::Field4memo( Data4& data, const char *name ) : Field4( data, name )
   {
   }



   Field4memo::Field4memo( Field4 f )
   {
      field = f.field ;
   }



   unsigned int Field4memo::read( char *outBuffer, unsigned int readLen, unsigned int startPos )
   {
      // AS 03/29/00 Added partial read/write support independent of normal mechanism
      return f4memoReadPart( field, outBuffer, readLen, startPos ) ;
   }



   int Field4memo::write( char *ptrToData, unsigned int lenToWrite )
   {
      // AS 03/29/00 Added partial read/write support independent of normal mechanism
      int rc = f4memoWritePart( field, ptrToData, lenToWrite, finalMemoLen, currentMemoPos ) ;
      currentMemoPos += lenToWrite ;
      return rc ;
   }
#endif   /* !S4JNI  LY 99/07/08 */



void Tag4::init( Data4 d, const char *name )
{
   if ( name )
      tag = d4tag( d.dat(), name ) ;
   else
   {
      tag = d4tagSelected( d.dat() ) ;

      if ( ! tag )
         tag = d4tagNext( d.dat(), 0 ) ;
   }
}



#ifndef S4OFF_REPORT
   REPORT4 * S4FUNCTION report4retrieve( Code4 &cb, char *fileName, int openFiles, char *pathname )
   {

      char *buf, *nameBuf ;
      REPORT4  *retvalue ;

      buf = (char *)u4allocFree( (CODE4 *)&cb, 2048 ) ;
      if ( !buf )
         return 0 ;

      nameBuf = (char *)u4allocFree( (CODE4 *)&cb, 512 ) ;
      if ( !nameBuf )
      {
         u4free( buf ) ;
         return 0 ;
      }
      retvalue = report4retrieve2( (CODE4 *)&cb, fileName, openFiles, pathname, buf, nameBuf  ) ;
      u4free( buf ) ;
      u4free( nameBuf ) ;

      return retvalue ;
   }



   RELATE4 * S4FUNCTION relate4retrieve( Code4 &cb, char *fileName, int openFiles, char *pathname )
   {
      char *buf, *nameBuf ;
      RELATE4 *retvalue ;

      buf = (char *)u4allocFree( (CODE4 *) &cb, 2048 ) ;
      if ( !buf )
         return 0 ;

      nameBuf = (char *)u4allocFree( (CODE4 *) &cb, 512 ) ;
      if ( !nameBuf )
      {
         u4free( buf ) ;
         return 0 ;
      }

      retvalue = relate4retrieve2( (CODE4 *) &cb, fileName, openFiles, pathname, buf, nameBuf  ) ;

      u4free( buf ) ;
      u4free( nameBuf ) ;

      return retvalue ;
   }



   int S4FUNCTION relate4save(Relate4set &relSet, char *fileName, int savePaths )
   {
      char *buf, *nameBuf ;
      int retvalue ;

      buf = (char *)u4allocFree( relSet.relate->codeBase, 2048 ) ;
      if ( !buf )
         return -1 ;

      nameBuf = (char *)u4allocFree( relSet.relate->codeBase, 512 ) ;
      if ( !nameBuf )
      {
         u4free( buf ) ;
         return -1 ;
      }

      retvalue = relate4save2( relSet.relate, fileName, savePaths, buf, nameBuf  ) ;

      u4free( buf ) ;
      u4free( nameBuf ) ;

      return retvalue ;

   }
#endif /* NOT S4OFF_REPORT */



#ifndef S4JNI  /* LY 99/07/08 */
   #ifndef S4OFF_ENFORCE_LOCK
      int Field4::lockCheck( )
      {
         DEBUG4INT(field == 0, E60201)
         if ( field->data->codeBase->accessMode!=OPEN4DENY_NONE)
           return r4success ;
         if ( field->data->codeBase->lockEnforce == FALSE || field->data->recNum <= 0L )
            return r4success ;
         // Check to see if the record is locked.
         if ( d4lockTest( field->data, d4recNo( field->data ), lock4write ) )
            return r4success ;
         return error4( field->data->codeBase, e4lock, E60201 ) ;

      }
   #endif
#endif
