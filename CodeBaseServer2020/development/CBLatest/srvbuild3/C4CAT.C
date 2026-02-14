/* c4cat.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_CATALOG

#ifdef CAT4CREATE
static FIELD4INFO catalogFields[] =
{
   { "ALIAS",      'C',  LEN4DATA_ALIAS, 0 },
   { "TYPE",       'C',  1, 0 },
   { "PATHNAME",   'C',  LEN4PATH, 0 },
   { "CREATE",     'L',  1, 0 },
   { "OPENMODE",   'N',  1, 0 },
   { "READONLY",   'L',  1, 0 },
   { "CREATOR",    'C',  LEN4USER_ID, 0 },
   { "OWNER",      'C',  LEN4USER_ID, 0 },
   { "LOG",        'N',  1, 0 },

   /* for SQL compatibility only */
   /*
      { "ID",         'N',  4, 0 },
      { "TBLPCTFREE", 'N',  5, 0 },
      { "SEGID",      'N',  5, 0 },
      { "HAS_PCTSTR", 'C',  1, 0 },
      { "HAS_FCNSTR", 'C',  1, 0 },
      { "HAS_CCNSTR", 'C',  1, 0 },
      { "HAS_UCNSTR", 'C',  1, 0 },
      { "TBL_STATUS", 'C',  1, 0 },
      { "RSS_ID",     'N',  5, 0 },
   */

   { 0, 0, 0, 0 },
} ;

static TAG4INFO catalogTags[] =
{
   /* for SQL compatibility only */
   /*  --> ALSO REMOVED D4TAG LATER IN FILE
      { "SYSTBLIDX", "UPPER(ALIAS)", ".NOT.DELETED() .AND. TYPE <> 'X'", r4unique, 0 },
   */
   { "ALIAS", "UPPER(ALIAS)", ".NOT.DELETED() .AND. ALIAS <> '                                '", r4unique, 0 },
   #ifdef S4MDX
      { "PATH", "SUBSTR(UPPER(PATHNAME),1,100)", ".NOT.DELETED()", 0, 0 },
   #else
      { "PATH", "SUBSTR(UPPER(PATHNAME),1,200)", ".NOT.DELETED()", 0, 0 },
   #endif
   { 0,0,0,0,0 },
} ;
#endif

int cat4add( CATALOG4 *cat, const char *owner, const char *alias, const char *pathName, const int ftype, const int create,
             const int openMode, const int readOnly, const int log )
{
   int rc ;
   CODE4 *c4 ;

   if ( cat == 0 || owner == 0 || alias == 0 || pathName == 0 )
      return error4( 0, e4parm_null, E93601 ) ;

   if ( cat->data == 0 )
      return error4( 0, e4parm, E93601  ) ;

   c4 = cat->data->codeBase ;

   if ( ftype != CAT4DBF && ftype != CAT4INDEX )
      return error4( c4, e4parm, E83603 ) ;

   #ifdef S4SERVER
      code4enterExclusive(c4, c4->catalogClient ) ;
   #endif

   rc = d4appendStart( cat->data, 0 ) ;
   if ( rc < 0 )
   {
      #ifdef S4SERVER
         code4exitExclusive(c4, c4->catalogClient ) ;
      #endif
      return rc ;
   }

   f4assign( cat->aliasFld, alias ) ;
   f4assign( cat->pathNameFld, pathName ) ;
   f4assign( cat->ownerFld, owner ) ;
   if ( openMode >= 0 )
      f4assignInt( cat->openModeFld, openMode ) ;
   f4assignChar( cat->readOnlyFld, readOnly ? 'T' : 'F' ) ;
   f4assignChar( cat->createFld, create ? 'T' : 'F' ) ;
   f4assignChar( cat->typeFld, ftype ) ;
   if ( log == LOG4ALWAYS )
      f4assignInt( cat->logFld, 2 ) ;
   else
      f4assignInt( cat->logFld, 1 ) ;

   rc = d4append( cat->data ) ;
   if ( rc == r4unique )  /* blank out alias field */
   {
      f4blank( cat->aliasFld ) ;
      rc = d4append( cat->data ) ;
   }

   d4unlockLow( cat->data, 1 ) ;

   #ifdef S4SERVER
      code4exitExclusive(c4, c4->catalogClient ) ;
   #endif

   if ( rc < 0 )
      return rc ;

   return 0 ;
}

#ifdef CAT4CREATE
CATALOG4 *S4FUNCTION cat4createCatalog( CODE4 *c4, const char *name )
{
   CATALOG4 *cat ;

   if ( c4 == 0 || name == 0 )
      return 0 ;

   cat = (CATALOG4 *)u4allocFree( c4, sizeof( CATALOG4 ) ) ;
   if ( cat == 0 )
      return 0 ;
   cat->data = d4create( c4, name, catalogFields, catalogTags ) ;
   if ( cat->data == 0 )
   {
      cat4close( cat ) ;
      return 0 ;
   }
   if ( cat4initFields( cat ) < 0 )
   {
      cat4close( cat ) ;
      return 0 ;
   }

   return cat ;
}
#endif

int cat4initFields( CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
      if ( cat->data == 0 )
         return error4( 0, e4parm, E93601 ) ;
   #endif

   cat->ownerFld =    d4field( cat->data, "OWNER" ) ;
   cat->aliasFld =    d4field( cat->data, "ALIAS" ) ;
   cat->pathNameFld = d4field( cat->data, "PATHNAME" ) ;
   cat->typeFld =     d4field( cat->data, "TYPE" ) ;
   cat->readOnlyFld = d4field( cat->data, "READONLY" ) ;
   cat->createFld =   d4field( cat->data, "CREATE" ) ;
   cat->openModeFld =  d4field( cat->data, "OPENMODE" ) ;
   cat->logFld =  d4field( cat->data, "LOG" ) ;

   if ( cat->ownerFld == 0 || cat->aliasFld == 0 || cat->pathNameFld == 0 ||
        cat->typeFld == 0 || cat->readOnlyFld == 0 || cat->logFld == 0 ||
        cat->createFld == 0 || cat->openModeFld == 0 )
      return error4( cat->data->codeBase, e4cat, E83601 ) ;

   return 0 ;
}

#ifndef CAT4CREATE
CATALOG4 *cat4open( CODE4 *c4, const char *name )
{
   CATALOG4 *cat ;
   int rc, oldUniqueError, oldSingleClient ;

   #ifdef E4PARM_LOW
      if ( c4 == 0 || name == 0 )
      {
         error4( 0, e4parm_null, E93601 ) ;
         return 0 ;
      }
   #endif

   cat = (CATALOG4 *)u4allocFree( c4, (long)sizeof( CATALOG4 ) ) ;
   if ( cat == 0 )
   {
      error4( c4, e4memory, E93601 ) ;
      return 0 ;
   }
   oldUniqueError = c4->errDefaultUnique ;
   c4->errDefaultUnique = r4unique ;
   oldSingleClient = c4->singleClient ;
   c4->singleClient = OPEN4DENY_NONE ;
   cat->data = d4open( c4, name ) ;
   c4->errDefaultUnique = oldUniqueError ;
   c4->singleClient = oldSingleClient ;
   if ( cat->data == 0 )
   {
      cat4close( cat ) ;
      error4( c4, e4open, E93601 ) ;
      return 0 ;
   }


   rc = cat4initFields( cat ) ;
   if ( rc < 0 )
   {
      cat4close( cat ) ;
      #ifdef E4STACK
         error4stack( c4, rc, E93601 ) ;
      #endif
      return 0 ;
   }

   cat->aliasTag = d4tag( cat->data, "ALIAS" ) ;
   if ( cat->aliasTag == 0 )
   {
      error4( 0, e4cat, E93601 ) ;
      return 0 ;
   }

   cat->pathTag = d4tag( cat->data, "PATH" ) ;
   if ( cat->pathTag == 0 )
   {
      error4( c4, e4cat, E93601 ) ;
      return 0 ;
   }

   /* also verify SQL compliant tag */
/*
   if ( d4tag( cat->data, "SYSTBLIDX" ) == 0 )
   {
      error4( c4, e4cat, E93601 ) ;
      return 0 ;
   }
*/

   #ifdef S4CLIENT
      l4remove( &c4->c4trans.trans.dataList, cat->data ) ;
   #endif

   return cat ;
}
#endif /* CAT4CREATE */

#ifdef CAT4CREATE
int S4FUNCTION cat4close( CATALOG4 *cat )
#else
int cat4close( CATALOG4 *cat )
#endif
{
   int rc ;

   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   rc = 0 ;
   if ( cat->data != 0 )
   {
      #ifdef S4CLIENT
         l4add( &cat->data->codeBase->c4trans.trans.dataList, cat->data ) ;
      #endif
      rc = d4close( cat->data ) ;
   }
   memset( cat, 0, sizeof( cat ) ) ;
   u4free( cat ) ;

   return rc ;
}

#ifndef CAT4CREATE
int cat4find( CATALOG4 *cat, const char *alias, const char ftype )
{
   int rc, aliasLen, numTrailBlanks, len ;
   char name[LEN4DATA_ALIAS + 1], pathName[LEN4PATH + 1] ;
   CODE4 *c4 ;
   SERVER4CLIENT *oldClient ;
   FIELD4 *pathFld ;

   #ifdef E4PARM_LOW
      if ( cat == 0 || alias == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   cat->valid = 0 ;
   c4 = cat->data->codeBase ;

   #ifdef E4PARM_LOW
      if ( cat->data == 0 )
         return error4( 0, e4parm, E93601 ) ;
   #endif

   memcpy( name, alias, LEN4DATA_ALIAS ) ;
   name[LEN4DATA_ALIAS] = 0 ;
   c4upper( name ) ;


   code4enterExclusive(c4, c4->catalogClient ) ;

   aliasLen = strlen( alias ) ;
   if ( aliasLen <= LEN4DATA_ALIAS )  /* alias search ok */
   {
      d4tagSelect( cat->data, cat->aliasTag ) ;
      rc = d4seek( cat->data, name ) ;
   }
   else
      rc = 1 ;   /* continue on with path seek */

   /* if failed on simple seek, try a path seek */
   if ( rc > 0 )
   {
      d4tagSelect( cat->data, cat->pathTag ) ;

      if ( u4nameCurrent( pathName, sizeof( pathName ), alias ) < 0 )
      {
         code4exitExclusive(c4, c4->catalogClient ) ;
         return error4( c4, e4cat, E93601 ) ;
      }

      numTrailBlanks = b4calcBlanks( pathName, strlen( pathName ), ' ' ) ;

      c4->currentClient = c4->catalogClient ;
      rc = d4seek( cat->data, pathName ) ;
      c4->currentClient = oldClient ;
      if ( rc > 0 )   /* failure to find is an error */
         rc = e4seek ;
      if ( rc == 0 && ( ( sizeof( pathName ) - numTrailBlanks ) > I4MAX_KEY_SIZE ) )
      {
         /* need to do a sub-search since path > max key size */
         len = strlen( pathName ) ;
         memset( pathName + len, ' ', sizeof( pathName ) - len - 1 ) ;
         pathFld = d4field( cat->data, "PATH" ) ;
         if ( pathFld == 0 )
         {
            code4exitExclusive(c4, c4->catalogClient ) ;
            return error4( c4, e4cat, E70184 ) ;
         }
         for ( ;; )
         {
            rc = c4memcmp( pathName, f4ptr( pathFld ), f4len( pathFld ) ) ;
            if ( rc == 0 )
               break ;
            if ( rc > 0 )
            {
               rc = e4seek ;
               break ;
            }
            rc = d4skip( cat->data, 1L ) ;
            if ( rc != 0 )
            {
               if ( rc > 0 )
                  rc = e4seek ;
               break ;
            }
         }
      }
   }

   if ( rc < 0 )
   {
      code4exitExclusive(c4, c4->catalogClient ) ;
      return error4( c4, (short)rc, E93601 ) ;
   }

   cat->valid = 1 ;
   for ( ;; )
   {
      if ( cat4type( cat ) == ftype )
      {
         code4exitExclusive(c4, c4->catalogClient ) ;
         return 0 ;
      }
      if ( d4skip( cat->data, 1L ) != 0 )
      {
         error4( c4, e4seek, E93601 ) ;
         break ;
      }
      if ( c4memcmp( name, cat4alias( cat ), strlen( name ) ) != 0 )
      {
         error4( c4, e4seek, E93601 ) ;
         break ;
      }
   }

   #ifdef E4ANALYZE
      if ( oldClient == 0 )
         error4( c4, e4struct, E93601 ) ;
   #endif

   code4exitExclusive(c4, c4->catalogClient ) ;

   cat->valid = 0 ;
   return -1 ;
}

int cat4openMode( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return -1 ;
   if ( f4char( cat->openModeFld ) == ' ' )  /* no explicit setting */
      return cat->data->codeBase->accessMode ;
   else
      return f4true( cat->openModeFld ) ;
}

int cat4create( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return -1 ;
   if ( f4char( cat->createFld ) == ' ' )  /* no explicit setting */
      return cat->data->codeBase->safety ;
   else
      return f4true( cat->createFld ) ;
}

int cat4log( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return -1 ;
   switch( f4int( cat->logFld ) )
   {
      case 2:
         return LOG4ALWAYS ;
      default:
         return 1 ;
   }
}

int cat4readOnly( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return -1 ;
   if ( f4char( cat->readOnlyFld ) == ' ' )  /* no explicit setting */
      return cat->data->codeBase->readOnly ;
   else
      return f4true( cat->readOnlyFld ) ;
}

char *cat4pathName( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
      {
         error4( 0, e4parm_null, E93601 ) ;
         return 0 ;
      }
   #endif

   if ( cat->valid == 0 )
      return 0 ;
   return f4str( cat->pathNameFld ) ;
}

int cat4pathNameLen( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return 0 ;
   return f4len( cat->pathNameFld ) ;
}

char * cat4owner( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
      {
         error4( 0, e4parm_null, E93601 ) ;
         return 0 ;
      }
   #endif

   if ( cat->valid == 0 )
      return 0 ;
   return f4str( cat->ownerFld ) ;
}

#ifndef S4INLINE
char * cat4alias( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
      {
         error4( 0, e4parm_null, E93601 ) ;
         return 0 ;
      }
   #endif

   if ( cat->valid == 0 )
      return 0 ;
   return f4str( cat->aliasFld ) ;
}
#endif

char cat4type( const CATALOG4 *cat )
{
   #ifdef E4PARM_LOW
      if ( cat == 0 )
         return error4( 0, e4parm_null, E93601 ) ;
   #endif

   if ( cat->valid == 0 )
      return (char)-1 ;
   if ( f4char( cat->typeFld ) == ' ' )  /* no explicit setting */
      return CAT4DBF ;
   else
      return f4char( cat->typeFld ) ;
}

int cat4avail( const CATALOG4 *cat )
{
   if ( cat == 0 )
      return 0 ;
   if ( cat->catalogStatus == 0 || cat->valid == 0 )
      return 0 ;
   return 1 ;
}

/*
#ifdef S4CLIENT
int S4FUNCTION d4password( CODE4 *c4, const char *password )
{
   int rc ;
   CAT4PASSWORD *cat4password ;
   CONNECTION4 *connection ;
   #ifdef E4PARM_HIGH
      if ( c4 == 0 || password == 0 )
         return error4( 0, e4parm_null, E92001 ) ;
   #endif

   cat4password = (CAT4PASSWORD *)u4allocEr( c4, sizeof( CAT4PASSWORD ) ) ;
   if ( cat4password == 0 )
      return error4stack( c4, e4memory, E92001 ) ;

   #ifdef CON4PASSWORD_HOOK
      cat4passwordScramble( password, cat4password->password ) ;
   #endif

   if ( cat4passwordFind( &c4->passwordList, cat4password->password ) == 1 )
   {
      u4free( cat4password ) ;
      return 0 ;
   }

   connection = c4->defaultServer->connect ;
   if ( connection == 0 )
      rc = e4connection ;
   else
   {
      rc = 0 ;
      for ( ;; )
      {
         connection4assign( connection, CON4PASSWORD, 0, 0 ) ;
         connection4send( connection ) ;
         rc = connection4receive( connection ) ;
         if ( rc < 0 )
            break ;
      }
   }

   if ( rc < 0 )
      u4free( cat4password ) ;

   l4add( &c4->passwordList, cat4password ) ;

   return rc ;
}
#endif
*/
#endif /* CAT4CREATE */

#endif /* S4OFF_CATALOG */
