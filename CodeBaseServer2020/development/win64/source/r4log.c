/* r4log.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4CLIENT

static int dataList4isIn( DATA4LIST *, const RELATE4 * ) ;
static int log4swapEntries( L4LOGICAL *, const int, const int ) ;

int e4isConstant( E4INFO *infoPtr )
{
   int pos ;

   // AS 04/20/00 changed, changed the E4 values available
   switch( infoPtr->functionI )
   {
      case E4DOUBLE:
      case E4STRING:
      case E4TRUE:
      case E4TRUE+1:
      case E4FALSE:
      case E4FALSE+1:
         return 1 ;
      case E4STOD:
      case E4CTOD:
         for ( pos = infoPtr->numEntries - 1 ; pos >= 0 ; pos -- )
         {
            if ( (infoPtr-pos)->fieldPtr != 0 || (infoPtr-pos)->functionI >= E4CALC_FUNCTION )
               return 0 ;
         }
         return 1 ;
      default:
         break ;
   }

   return 0 ;
}



#ifdef P4ARGS_USED
   #pragma argsused
#endif
int e4isTag( E4INFO_REPORT *reportPtr, EXPR4 *expr, E4INFO *infoPtr, DATA4 *data )
{
   /* returns true if there is a tag that matches the desired condition type,
      AND if there is no filter on that tag */
   /*
      02/10/98 AS
      -----------
      CodeBase is also not able to optimize on tags in the new FoxPro fields
      formats.  This is because we only have simple field support, so, eg:
      "INT_FIELD = 6"  INT_FIELD can be converted to a double as an expression
      check, but the constant '6' cannot be converted to an int which would
      be able to be looked up in the index file.  Possibly in the future we
      will add support for these field types, for now any index tags in the
      new field type returning e4isTag failure here.  Types are:

      Allowed:
      -------
      r4double
      r4charBin
      r4memoBin

      Disallowed:
      ----------
      r4int
      r4currency
      r4dateTime

      AS 04/26/00 Found inconsistency ... the relate module does not use collating sequence, but if a tag is in a collating
      sequence then it was being used anyway (eg. with bitmap optimization it was using the tag possibly but if it later
      decided not to use it, the results could differ because it would use machine collation)


   */
   #ifndef S4INDEX_OFF
      TAG4 *tagOn ;
      int isSame, i ;
      E4INFO *infoOn, *tagInfo ;
      #ifdef S4FOX
         char doContinue ;
      #endif

      for( tagOn = 0;; )
      {
         tagOn = d4tagNext( data, tagOn ) ;
         if ( tagOn == 0 )
            break ;

         // AS 04/26/00 - problem was using general tags in relation but the relate module normally uses machine collation
         // so results could differ depending on if a tag was available.  Disabled in general, but can re-enable to old
         // way by setting useGeneralTagsInRelate
         #ifdef S4FOX
            // if USE... is defined, always use no matter what, else dependent on CODE4 setting
            #if !defined( S4USE_GENERAL_TAGS_IN_RELATE )
               if ( data->codeBase->useGeneralTagsInRelate == 0 )
                  if ( tagOn->tagFile->collateName != collate4none && tagOn->tagFile->collateName != collate4machine )
                  {
                     // can't use because we are disabling their use and it is not machine or none
                     continue ;
                  }
            #endif
         #endif

         /* see above function header for reasons for code here */
         #ifdef S4FOX
            doContinue = 0 ;
            switch( tfile4type( tagOn->tagFile ) )
            {
               case r4currency:
               case r4int:
               case r4dateTime:
                  doContinue = 1 ;
                  break ;
               default:
                  break ;
            }
            if ( doContinue )
               continue ;
         #endif

         expr4context( tagOn->tagFile->expr, data ) ;  // tag must be set to correspond to the data file being used

         if ( tagOn->tagFile->filter == 0 && ( t4unique( tagOn ) != r4uniqueContinue ) )  /* if unique a filter, than cannot bitmap optimize */
         {
            tagInfo = tagOn->tagFile->expr->info + tagOn->tagFile->expr->infoN -1 ;
            if ( tagInfo->numEntries == infoPtr->numEntries )
            {
               isSame =  1 ;
               infoOn = infoPtr ;
               for( i = 0; i < infoPtr->numEntries && isSame; i++, infoOn--, tagInfo-- )
               {
                  /* verify the general info structure */
                  if ((infoOn->len != tagInfo->len) || (infoOn->numEntries != tagInfo->numEntries) || (infoOn->numParms != tagInfo->numParms))
                  {
                     isSame = 0 ;
                     break ;
                  }

                  /* general match, so ensure a field match if appropriate */
                  if ( infoOn->fieldPtr == 0 )
                  {
                     if ( tagInfo->fieldPtr != 0 )
                     {
                        isSame = 0 ;
                        break ;
                     }
                  }
                  else
                  {
                     if ( tagInfo->fieldPtr == 0 )
                     {
                        isSame = 0 ;
                        break ;
                     }
                     /* AS 03/30/99 -- ensure that the DATA4 pointer from the field matches
                        that from the expression.  t4rlate5.c - problem was that if the same
                        DATA4 used twice and the slave was referred to in a relate query
                        it may have used the slave's tag (same TAG4FILE) instead of its
                        own (i.e. no tag should be used off of a slaves DATA4) */
                     if ( infoOn->fieldPtr->data != data )
                     {
                        #ifdef E4DEBUG
                           // AS make sure it does not discount data4's because of relation server cloning (i.e. same aliases)
                           if ( strcmp( infoOn->fieldPtr->data->alias, data->alias ) == 0 )
                              return error4( data->codeBase, e4result, E96001 ) ;
                        #endif
                        isSame = 0 ;
                        break ;
                     }
                     short compareLen = sizeof( tagInfo->fieldPtr->name ) + sizeof( tagInfo->fieldPtr->len )
                                      + sizeof( tagInfo->fieldPtr->dec ) + sizeof( tagInfo->fieldPtr->type )
                                      + sizeof( tagInfo->fieldPtr->offset ) ;
                     if ( c4memcmp( infoOn->fieldPtr, tagInfo->fieldPtr, compareLen ) || ( infoOn->fieldPtr->data->dataFile != tagInfo->fieldPtr->data->dataFile ) )
                     {
                        isSame = 0 ;
                        break ;
                     }
                  }

                  switch( infoOn->functionI )
                  {
                     case E4DOUBLE:
                     case E4STRING:
                     case E4CTOD:
                     case E4DTOC:
                     case E4DTOC+1:
                        /* Compare Constant */
                        if( c4memcmp( tagOn->tagFile->expr->constants + tagInfo->i1, expr->constants + infoOn->i1, (unsigned int)tagInfo->len ) != 0 )
                           isSame = 0 ;
                        break ;

                     default:
                        isSame =  (infoOn->i1 == tagInfo->i1) ;
                        break ;
                  }
                  if( infoOn->functionI != tagInfo->functionI )
                     if( infoOn->functionI > E4LAST_FIELD || tagInfo->functionI > E4LAST_FIELD )
                        isSame = 0 ;
               }
               if( isSame )
               {
                  reportPtr->tag = tagOn->tagFile ;
                  /* AS 6/4/98 changes.60 fix #133 */
                  t4versionCheck( tagOn, 0, 0 ) ;
                  return 1 ;
               }
            }
         }
      }
   #endif
   return 0 ;
}



static int dataList4add( DATA4LIST *listIn, CODE4 *codeBase, RELATE4 *newPointer )
{
   if ( error4code( codeBase ) < 0 )
      return -1 ;
   if ( newPointer == 0 )
      return 0 ;
   if ( dataList4isIn( listIn, newPointer ) )
      return 0 ;
   if( listIn->pointersTot <= listIn->pointersUsed )
   {
      listIn->pointersTot += 5 ;
      if ( u4allocAgain( codeBase, (char **)&listIn->pointers, &listIn->memAllocated, listIn->pointersTot * sizeof(RELATE4 *)) < 0 )
         return -1 ;
   }
   listIn->pointers[listIn->pointersUsed++] = newPointer ;
   return 0 ;
}



static int dataList4expandFromDbTree( DATA4LIST *listIn, CODE4 *codeBase )
{
   int i ;
   RELATE4 *relateParent ;

   // AS if listIn is invalid, return an error - was happening in intuit case on server
   if ( listIn == 0 )
      return error4( codeBase, e4relate, E96001 ) ;

   for( i = listIn->pointersUsed - 1 ; i >= 0 ; i-- )
   {
      relateParent = listIn->pointers[i]->master ;
      while( relateParent != 0 )
      {
         int rc = dataList4add( listIn, codeBase, relateParent ) ;
         if ( rc < 0 )
            return rc ;
         relateParent = relateParent->master ;
      }
   }

   if ( error4code( codeBase ) < 0 )
      return error4code( codeBase ) ;

   return 0 ;
}



static int dataList4isIn( DATA4LIST *listIn, const RELATE4 *newPointer )
{
   int i ;
   for( i = 0 ; i < listIn->pointersTot ; i++ )
   {
      if ( listIn->pointers[i] == newPointer )
         return 1 ;
   }

   return 0 ;
}



static int dataList4readRecords( DATA4LIST *dList )
{
   RELATE4 *cur ;
   int i, rc ;

   if ( dList == 0 )
      return 0 ;

   for( i = dList->pointersUsed-1 ; i >= 0 ; i-- )
   {
      cur = dList->pointers[i] ;
      rc = relate4readIn( cur ) ;
      if ( rc  == relate4filterRecord || rc == r4terminate )
         return rc ;
      if ( rc < 0 )
         return -1 ;
   }
   return 0 ;
}



static int dataList4remove( DATA4LIST *thisList, DATA4LIST *removeList )
{
   int i ;

   #ifdef E4PARM_LOW
      if ( thisList == 0 || removeList == 0 )
         return error4( 0, e4parm_null, E96001 ) ;
   #endif

   for( i = 0; i < thisList->pointersUsed; i++ )
      if( dataList4isIn( removeList, thisList->pointers[i]) )
         thisList->pointers[i--] = thisList->pointers[--thisList->pointersUsed] ;

   return 0 ;
}



static int log4addToList( L4LOGICAL *log, E4INFO *infoPtr, DATA4LIST *listIn )
{
   int numParms, i ;

   if ( infoPtr->functionI <= E4LAST_FIELD )
   {
      int rc = dataList4add( listIn, log->codeBase, relate4lookupRelate( (RELATE4 *)&log->relation->relate, f4data(infoPtr->fieldPtr)) ) ;
      if ( rc < 0 )
         return rc ;
   }

   if ( infoPtr->numEntries == 1 )
      return 0 ;

   numParms = infoPtr->numParms ;
   infoPtr-- ;

   for ( i = 0; i < numParms; i++ )
   {
      int rc = log4addToList( log, infoPtr, listIn ) ;
      if ( rc < 0 )
         return rc ;
      infoPtr -= infoPtr->numEntries ;
   }
   if ( error4code( log->codeBase ) < 0 )
      return error4code( log->codeBase ) ;
   return 0 ;
}



int log4buildDatabaseLists( L4LOGICAL *log )
{
   int lastPos, pos, i ;
   E4INFO *infoLast ;

   log->infoReport = (E4INFO_REPORT *)u4allocEr( log->codeBase, (long)sizeof(E4INFO_REPORT) * log->expr->infoN ) ;
   if ( log->infoReport == 0 )
      return e4memory ;

   lastPos = log->expr->infoN - 1 ;
   infoLast = (E4INFO *)log->expr->info + lastPos ;

   if ( infoLast->functionI == E4AND )
   {
      pos = lastPos - 1 ;

      for ( i = 0; i < infoLast->numParms; i++ )
      {
         if ( log->infoReport[pos].relateDataList == 0 )
         {
            log->infoReport[pos].relateDataList = (DATA4LIST *)mem4createAllocZero( log->codeBase,
                   &log->codeBase->dataListMemory, 5, sizeof(DATA4LIST), 5, 0 ) ;
            if ( log->infoReport[pos].relateDataList == 0 )
               return e4memory ;
         }
         int rc = log4addToList( log, log->expr->info+pos, log->infoReport[pos].relateDataList ) ;
         if ( rc < 0 )
            return rc ;
         pos -= log->expr->info[pos].numEntries ;
      }
   }
   else
   {
      if ( log->infoReport[lastPos].relateDataList == 0 )
      {
         log->infoReport[lastPos].relateDataList = (DATA4LIST *)mem4createAllocZero( log->codeBase,
            &log->codeBase->dataListMemory, 5, sizeof( DATA4LIST ), 5, 0 ) ;
         if ( log->infoReport[lastPos].relateDataList == 0 )
            return e4memory ;
      }
      log4addToList( log, infoLast, log->infoReport[lastPos].relateDataList ) ;
   }

   if ( error4code( log->codeBase ) < 0 )
      return error4code( log->codeBase ) ;
   return 0 ;
}



int log4bitmapDo( L4LOGICAL *log )
{
   if ( error4code( log->codeBase ) < 0 )
      return e4codeBase ;

   int rc = log4buildDatabaseLists( log ) ;
   if ( rc < 0 )
      return rc ;

   #ifndef S4INDEX_OFF
      rc = bitmap4evaluate( log, log->expr->infoN - 1 ) ;
      if ( rc < 0 )
         return rc ;
   #endif

   if ( error4code( log->codeBase ) < 0 )
      return error4code( log->codeBase ) ;

   return 0 ;
}



int log4determineEvaluationOrder( L4LOGICAL *log )
{
   /* Expand Lists due to Database Tree */
   int i, pos, numLeft, curSmallestPos, curSmallestNum, curPos ;
   int numCompare, lastPos = log->expr->infoN -1 ;
   E4INFO *infoLast, *infoPtr ;
   E4INFO_REPORT *reportLast, *report, *curReport ;

   infoLast = (E4INFO *)log->expr->info + lastPos ;
   reportLast = log->infoReport + lastPos ;

   if ( infoLast->functionI != E4AND )
      return dataList4expandFromDbTree( reportLast->relateDataList, log->codeBase ) ;

   infoPtr = infoLast-1 ;
   report = reportLast-1 ;
   for ( i = 0; i < infoLast->numParms; i++ )
   {
      int rc = dataList4expandFromDbTree( report->relateDataList, log->codeBase ) ;
      if ( rc < 0 )
         return rc ;

      report -= infoPtr->numEntries ;
      infoPtr -= infoPtr->numEntries ;
   }

   /* Change the evaluation orders by repeatedly determining the
      listIn with the smallest number of entries and puting it at the end.
      The idea is that we want the conditions which causes the fewest
      additional database records to be read in, to be evaluated first.
   */
   pos = lastPos - 1 ;  /* Position currently being made into the fewest. */

   for( numLeft = infoLast->numParms; numLeft > 1; numLeft-- )
   {
      report = log->infoReport + pos ;
      infoPtr = (E4INFO *)log->expr->info + pos ;

      /* Now determine which is the entry with the fewest data files. */
      curSmallestPos = pos ;
      curSmallestNum = report->relateDataList->pointersUsed ;

      curPos = pos - infoPtr->numEntries ;
      for( numCompare = numLeft-1; numCompare > 0; numCompare-- )
      {
         curReport = log->infoReport + curPos ;

         if ( curReport->relateDataList->pointersUsed < curSmallestNum )
         {
            curSmallestNum = curReport->relateDataList->pointersUsed ;
            curSmallestPos = curPos ;
         }

         curPos -= log->expr->info[curPos].numEntries ;
      }

      if( pos != curSmallestPos )
          if ( log4swapEntries( log, pos, curSmallestPos ) < 0 )
             return -1 ;

      /* The next step is to remove the data listIn for the first evaluated
         condition from the data listIn of the rest of the conditions. */
      curPos = pos - infoPtr->numEntries ;
      for( i = numLeft-1; i > 0; i-- )
      {
         curReport = log->infoReport + curPos ;
         dataList4remove( curReport->relateDataList, report->relateDataList ) ;
         curPos -= log->expr->info[curPos].numEntries ;
      }

      pos -= infoPtr->numEntries ;
   }
   if ( error4code( log->codeBase ) < 0 )
      return -1 ;
   return 0 ;
}



static int log4swapEntries( L4LOGICAL *log, const int a, const int b )
{
   int largeEntries, smallEntries ;
   char   *saveBuf ;
   E4INFO  *aPtr, *bPtr, *small1, *large1, *middle1;
   E4INFO_REPORT *small2, *large2, *middle2 ;
   int  smallPos, largePos, middlePos, middleEntries, movePositions ;

   if ( error4code( log->codeBase ) < 0 )
      return -1 ;

   aPtr = log->expr->info + a ;
   bPtr = log->expr->info + b ;

   if ( aPtr->numEntries > bPtr->numEntries )
   {
      small1= bPtr ;
      large1= aPtr ;
      smallPos = b ;
      largePos = a ;
   }
   else
   {
      small1= aPtr ;
      large1= bPtr ;
      smallPos = a ;
      largePos = b ;
   }

   /* make copies of large and small entries because the info may be later
      lost as swaps take place... */
   largeEntries = large1->numEntries ;
   smallEntries = small1->numEntries ;
   saveBuf = (char *)u4allocFree( log->codeBase, (long)sizeof(E4INFO) * largeEntries ) ;
   if ( saveBuf == 0 )
      return error4( log->codeBase, e4memory, E86001 ) ;

   movePositions = largeEntries - smallEntries ;
   if ( smallPos < largePos )
   {
      middlePos = smallPos + 1 ;
      middleEntries = largePos - smallPos - largeEntries ;
   }
   else
   {
      middlePos = largePos + 1 ;
      middleEntries = smallPos - largePos - smallEntries ;
      movePositions = -movePositions ;
   }
   middle1= log->expr->info + middlePos ;

   c4memcpy( saveBuf, (void *)(large1- largeEntries + 1), sizeof(E4INFO) * largeEntries ) ;
   if ( largePos > smallPos )  /* want to move small to end of large pos... */
   {
      c4memcpy( (void *)(large1- smallEntries + 1 ), (void *)(small1- smallEntries + 1),
                  sizeof(E4INFO) *smallEntries ) ;
      c4memmove( (void *)(middle1+ movePositions), middle1, sizeof(E4INFO) * middleEntries ) ;
      c4memcpy( (void *)(small1- smallEntries + 1), saveBuf, sizeof(E4INFO) * largeEntries ) ;
   }
   else  /* want to move small to start of large pos... */
   {
      c4memcpy( (void *)(large1- largeEntries + 1 ), (void *)(small1- smallEntries + 1),
                  sizeof(E4INFO) *smallEntries ) ;
      c4memmove( (void *)(middle1+ movePositions), middle1, sizeof(E4INFO) * middleEntries ) ;
      c4memcpy( (void *)(small1- largeEntries + 1), saveBuf, sizeof(E4INFO) * largeEntries ) ;
   }

   large2  = log->infoReport + largePos ;
   small2  = log->infoReport + smallPos ;
   middle2 = log->infoReport + middlePos ;

   c4memcpy( saveBuf, (void *)(large2 - largeEntries + 1), sizeof(E4INFO_REPORT) * largeEntries ) ;
   if ( largePos > smallPos )  /* want to move small to end of large pos... */
   {
      c4memcpy( (void *)(large2 - smallEntries + 1), (void *)(small2 - smallEntries + 1),
                  sizeof(E4INFO_REPORT) *smallEntries ) ;
      c4memmove( middle2 + movePositions, middle2, sizeof(E4INFO_REPORT) * middleEntries ) ;
      c4memcpy( (void *)(small2 - smallEntries + 1), saveBuf, sizeof(E4INFO_REPORT) * largeEntries ) ;
   }
   else  /* want to move small to start of large pos... */
   {
      c4memcpy( (void *)(large2 - largeEntries + 1), (void *)(small2 - smallEntries + 1),
                  sizeof(E4INFO_REPORT) *smallEntries ) ;
      c4memmove( middle2 + movePositions, middle2, sizeof(E4INFO_REPORT) * middleEntries ) ;
      c4memcpy( (void *)(small2 - largeEntries + 1), saveBuf, sizeof(E4INFO_REPORT) * largeEntries ) ;
   }

   u4free( saveBuf ) ;

   return 0 ;
}



int log4true( L4LOGICAL *log )
{
   /* Must read in records, as appropriate, to evaluate the different parts of */
   /* the expression. */
   int curPos, rc, i, *resultPtr ;
   E4INFO *infoPtr ;
   E4INFO_REPORT *infoReportPtr ;
   int nParms = 1 ;

   // AS Aug 6/02 - avoid gpf if log->expr is null  - indicates the relate4 has gone bad...
   if ( log->expr == 0 )
      return error4( 0, e4relate, E96002 ) ;

   curPos = log->expr->infoN - 1 ;

   if( log->expr->info[curPos].functionI == E4AND )
   {
      nParms = log->expr->info[curPos].numParms ;
      curPos-- ;
   }

   /* Go through each of the & sub-expressions and evaluate them, first */
   /* reading in the appropriate database records for the sub-expression. */

   if ( expr4context( log->expr, log->expr->data ) < 0 )
      return -1 ;

   for( i = 0; i < nParms; i++ )
   {
      infoPtr = log->expr->info + curPos ;
      infoReportPtr = log->infoReport + curPos ;

      rc = dataList4readRecords( infoReportPtr->relateDataList ) ;
      if ( rc == relate4filterRecord )
         return 0 ;
      if ( rc == r4terminate )
         return rc ;
      if ( rc < 0 )
         return -1 ;

      if ( log->expr->info[curPos].numParms < 2 )
      {
         if ( expr4execute( log->expr, curPos, (void **)&resultPtr ) < 0 )
            return -1 ;
         if ( *resultPtr == 0 )
            return 0 ;
      }
      else
      {
         #ifdef S4TEST
         #ifdef S4DEBUG
            /* in debug case, if tag, the result must always be true if we get here... */
            if ( log->codeBase->bitmap_disable == 0 && !log->relation->bitmapsFreed )   /* then do check */
            {
               if ( expr4execute( log->expr, curPos, (void **)&resultPtr ) < 0 )
                  return -1 ;
               if ( ( (infoReportPtr-1)->tag || (infoReportPtr-2)->tag ) )
                  if ( *resultPtr == 0 )
                     return error4( log->codeBase, e4info, E96002 ) ;
            }
         #endif
         #endif

         if ( ( (infoReportPtr-1)->tag == 0 && (infoReportPtr-2)->tag == 0 ) || log->relation->bitmapsFreed )
         {
            if ( expr4execute( log->expr, curPos, (void **)&resultPtr ) < 0 )
               return -1 ;
            if ( *resultPtr == 0 )
               return 0 ;
         }
      }
      curPos -= infoPtr->numEntries ;
   }
   if ( error4code( log->codeBase ) < 0 )
      return -1 ;
   return 1 ;
}

#endif /* S4CLIENT */
