/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* e4calc.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef ___TURBOC__
   #pragma hdrstop
#endif

void S4FUNCTION expr4calcMassage( EXPR4CALC *calc )
{
   EXPR4 *exp4 = calc->expr;

   if( exp4->type == r4num )
   {
      /* Must consist of a single numeric field */
      exp4->type =  r4numDoub ;
      exp4->len =  sizeof(double) ;
      exp4->info->len =  f4len( exp4->info->fieldPtr ) ;
      exp4->info->functionI =  E4FIELD_NUM_D ;
      exp4->info->function =  (S4OPERATOR *)v4functions[E4FIELD_NUM_D].functionPtr ;
   }
}



#ifdef S4CLIENT
static void code4calcCreateSendDataAliases( EXPR4CALC *calc, CONNECTION4 *connection )
{
   // AS 03/31/99 moved into seperate recursive function to send info for calcs of calcs...

   EXPR4 *exp4 = calc->expr ;

   for ( int i = 0 ; i < exp4->infoN ; i++ )   /* update FIELD4's if reqd */
   {
      if ( exp4->info[i].fieldNo != 0 )
      {
         if ( exp4->info[i].fieldPtr != 0 )
         {
            DATA4 *d4 = exp4->info[i].fieldPtr->data ;

            if ( d4->aliasSet == 1 )
            {
               short len = strlen( dfile4name( d4->dataFile ) ) + 1 ;
               short nboLen = htons5( len ) ;
               connection4addData( connection, &nboLen, sizeof( nboLen ), 0 ) ;
               connection4addData( connection, dfile4name( d4->dataFile ), len, 0 ) ;
               len = strlen( d4->alias ) + 1 ;
               nboLen = htons5( len ) ;
               connection4addData( connection, &nboLen, sizeof( nboLen ), 0 ) ;
               connection4addData( connection, d4->alias, len, 0 ) ;
            }
            else
            {
               short nboLen = htons5( 0 ) ;
               connection4addData( connection, &nboLen, sizeof( nboLen ), 0 ) ;
            }
         }
      }
      else
      {
         /* AS 03/31/99 --> if the calc includes a reference to another calc which
            includes a reference to an alias (t4rlate5.cpp), the aliases were not
            being transferred, so adding that here...
         */
         if ( exp4->info[i].functionI == E4CALC_FUNCTION )
         {
            EXPR4CALC *calcInternal = ((EXPR4CALC *)(exp4->info[i].p1) ) ;
            code4calcCreateSendDataAliases( calcInternal, connection ) ;  // recursively do ourselves
         }
      }
   }
}



int code4calcNumData4s( EXPR4CALC *calc )
{
   EXPR4 *exp4 = calc->expr ;
   int numData4 = 0 ;

   for ( int i = 0 ; i < exp4->infoN ; i++ )   /* update FIELD4's if reqd */
   {
      if ( exp4->info[i].fieldNo != 0 )
      {
         if ( exp4->info[i].fieldPtr != 0 )
            numData4++ ;
      }
      else
      {
         /* AS 03/31/99 --> if the calc includes a reference to another calc which
            includes a reference to an alias (t4rlate5.cpp), the aliases were not
            being transferred, so adding that here...
         */
         if ( exp4->info[i].functionI == E4CALC_FUNCTION )
         {
            EXPR4CALC *calcInternal = ((EXPR4CALC *)(exp4->info[i].p1) ) ;
            numData4 += code4calcNumData4s( calcInternal ) ;  // recursively do ourselves
         }
      }
   }

   return numData4 ;
}

#endif /* S4CLIENT */



EXPR4CALC *S4FUNCTION code4calcCreate( CODE4 *c4, EXPR4 *exp4, const char *name )
{
   #ifdef S4CLIENT
      CONNECTION4 *connection ;
      CONNECTION4CALC_CREATE_INFO_IN *infoIn ;
      DATA4 *data ;
      int rc ;
   #endif
   EXPR4CALC *calcPtr ;

   #ifdef E4PARM_HIGH
      if ( c4 == 0 || exp4 == 0 || name == 0 )
      {
         error4( 0, e4parm_null, E90920 ) ;
         return 0 ;
      }
   #endif

   calcPtr = (EXPR4CALC *)mem4createAllocZero( c4, &c4->calcMemory, 5, sizeof(EXPR4CALC), 5, 0 ) ;
   if ( calcPtr == 0 )
      return 0 ;

   #ifdef S4SERVER
      l4add( &c4->currentClient->calcList, calcPtr ) ;
   #else
      l4add( &c4->calcList, calcPtr ) ;
   #endif
   calcPtr->expr = exp4 ;
   u4ncpy( calcPtr->name, name, sizeof(calcPtr->name) ) ;
   c4upper( calcPtr->name ) ;

   expr4calcMassage( calcPtr ) ;

   #ifdef S4CLIENT
      if ( calcPtr != 0 )   /* need to register calc on server and verify it is ok */
      {
         /* AS 09/14/98 need to send any aliases over for any data4's involved
            in the calc with aliases.  This is because normally the server does
            not get the clients aliases. */

         data = exp4->data ;
         connection = data->dataFile->connection ;
         rc = connection4assign( connection, CON4CALC_CREATE, data4clientId( data ), data4serverId( data ) ) ;
         if ( rc < 0 )
            return 0 ;
         connection4addData( connection, 0, sizeof( CONNECTION4CALC_CREATE_INFO_IN ), (void **)&infoIn ) ;
         u4ncpy( infoIn->calcName, name, sizeof( calcPtr->name ) ) ;
         short len = strlen( exp4->source ) + 1 ;
         short nboLen = htons5( len ) ;  /* network byte order */
         connection4addData( connection, &nboLen, sizeof( nboLen ), 0 ) ;
         connection4addData( connection, exp4->source, len, 0 ) ;

         short numData4 = code4calcNumData4s( calcPtr ) ;

         short nboNumData4 = htons5( numData4 ) ;
         connection4addData( connection, &nboNumData4, sizeof( nboNumData4 ), 0 ) ;

         code4calcCreateSendDataAliases( calcPtr, connection ) ;

         connection4sendMessage( connection ) ;
         rc = connection4receiveMessage( connection ) ;
         if ( rc < 0 )
            return 0 ;

         rc = connection4status( connection ) ;
         if ( rc < 0 )
         {
            connection4error( connection, c4, rc, E90920 ) ;
            return 0 ;
         }
      }
   #endif

   return calcPtr ;
}

EXPR4CALC *S4FUNCTION expr4calcLookup( CODE4 *c4, DATA4 *d4, const char *name, const unsigned int nameLenIn )
{
   EXPR4CALC *calcOn ;
   char buf[sizeof(calcOn->name)] ;
   unsigned int nameLen ;

   nameLen = nameLenIn ;

   if ( nameLen >= sizeof(calcOn->name) )
      nameLen = sizeof(calcOn->name)-1 ;
   u4ncpy( buf, name, nameLen+1 ) ;
   c4upper( buf ) ;
   for( calcOn = 0 ;; )
   {
      #ifdef S4SERVER
         calcOn = (EXPR4CALC *)l4next( &c4->currentClient->calcList, calcOn ) ;
      #else
         calcOn = (EXPR4CALC *)l4next( &c4->calcList, calcOn ) ;
      #endif
      if ( calcOn == 0 )
         return 0 ;
      if ( c4strcmp( calcOn->name, buf) == 0 )
      {
         /* AS 04/30/99 t4relxb - can even fail in stand-alone, say if the data files
            are all closed and then re-opened...*/
         #ifndef S4CLIENT
            /* AS 09/03/98 need to regenerate the calc in case the context
               has changed (eg. relation module new data4's) */
            EXPR4 *saveExpr = calcOn->expr ;
            // AS 05/06/99 --> for codereporter, d4 may be null, use expr->d4 instead...
            calcOn->expr = expr4parse( ( d4 == 0 ) ? saveExpr->data : d4, saveExpr->source ) ;
            if ( saveExpr != 0 )
               expr4free( saveExpr ) ;

            /* AS 02/23/99 need to massage the new calc as well */
            if ( calcOn->expr == 0 )
               return 0 ;
            /* AS 04/30/99 need to reset curResultPos before massage (similar to expr4calcModify)...*/
            calcOn->curResultPos = 0 ;
            expr4calcMassage( calcOn ) ;
         #endif
         return calcOn ;
      }
   }
}

int S4FUNCTION expr4calcResultPos( EXPR4CALC *calcPtr, const int newResultPos )
{
   E4INFO *info ;
   int i, offset = newResultPos - calcPtr->curResultPos ;
   if ( offset == 0 )
      return 0 ;

   calcPtr->curResultPos = newResultPos ;

   info = calcPtr->expr->info ;
   for( i = calcPtr->expr->infoN; --i >= 0; )
   {
      info[i].resultPos += offset ;
      // AS May 31/04 - There is a chance the the result will be that we go past the end of the internal work buffer...
      // ensure we have enough room
      if ( info[i].resultPos + info[i].len > (int)calcPtr->expr->exprBufLen )
      {
         CODE4 *codeBase = calcPtr->expr->codeBase ;
         if ( u4allocAgain( codeBase, &calcPtr->expr->exprWorkBuf, &calcPtr->expr->exprBufLen, info[i].resultPos + info[i].len ) == e4memory )
            return error4stack( codeBase, e4memory, E90901 ) ;
      }
   }

   return 0 ;
}

#ifdef S4WINTEL
/* Expression source must be updated to refect the fact that a calculation had a name change */
/* Caller must ensure names are trimmed in front and back & are upper case */
int S4FUNCTION expr4calcNameChange( EXPR4 **exprOn, const char *oldName, const char *newName )
{
   EXPR4 *newExpr ;
   char bufMem[50] ;
   char *buf ;
   unsigned pos, bufLen ;
   int oldNameLen, ptrLen, newNameLen, didAlloc, bufPos, didChange ;
   const char *ptr ;

   ptr = expr4source( *exprOn ) ;
   oldNameLen = strlen( oldName ) ;
   ptrLen = strlen( ptr ) ;
   bufLen = sizeof( bufMem ) ;
   buf = bufMem ;
   didAlloc = bufPos = didChange = 0 ;

   for( pos = 0; ptr[pos]; pos++)
   {
      buf[bufPos++] = ptr[pos] ;
      if( (unsigned)bufPos == bufLen )
      {
         if( didAlloc )
         {
            u4allocAgain( (*exprOn)->codeBase, &buf, &bufLen, bufLen+50 ) ;
            if( buf == 0 )
               return -1 ;
         }
         else
         {
            bufLen += 50 ;
            buf = (char *) u4allocEr( (*exprOn)->codeBase, bufLen + 50 ) ;
            if( buf == 0 )
               return -1 ;
            memcpy( buf, bufMem, sizeof(bufMem) ) ;
            didAlloc = 1 ;
         }
      }

      if( ((unsigned) ptrLen - pos) < (unsigned) oldNameLen )
         continue ;
      // AS Dec 13/05 vs 5.0 fixes
      #if defined( S4WINDOWS_VS5_PLUS ) || defined( S4WINCE )
         if( _memicmp( (void *)(ptr+pos), (void *)oldName, oldNameLen ) != 0 )
      #else
         if( memicmp( (void *)(ptr+pos), (void *)oldName, oldNameLen ) != 0 )
      #endif
            continue ;
      if( u4nameChar( ptr[pos+oldNameLen] )  )
         continue ;
      if( pos > 0 )
         if( u4nameChar(ptr[pos-1]) )
            continue ;

      didChange = 1 ;
      newNameLen = strlen(newName) ;
      if( bufLen <= (unsigned) (bufPos + newNameLen) )
      {
         if( didAlloc )
         {
            u4allocAgain( (*exprOn)->codeBase, &buf, &bufLen, bufLen+ newNameLen + 50 ) ;
            if( buf == 0 )
               return -1 ;
         }
         else
         {
            bufLen += newNameLen + 50 ;
            buf = (char *)u4allocEr( (*exprOn)->codeBase, bufLen ) ;
            if( buf == 0 )
               return -1 ;
            memcpy( buf, bufMem, sizeof(bufMem) ) ;
            didAlloc = 1 ;
         }
      }

      memcpy( buf+(--bufPos), newName, newNameLen ) ;
      bufPos += newNameLen ;
      pos += oldNameLen-1 ;
   }

   if( didChange )
   {
      buf[bufPos] = 0 ;
      newExpr = expr4parseLow( (*exprOn)->data, buf, 0 ) ;
      if ( newExpr )
      {
         if ( *exprOn != 0 )
            expr4free( *exprOn ) ;
         *exprOn = newExpr ;
      }
      if( didAlloc )
         u4free( buf ) ;
      return 0 ;
   }

   return 1 ;
}
#endif  /* S4WINTEL */
