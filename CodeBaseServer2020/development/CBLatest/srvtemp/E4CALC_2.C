/* e4calc_2.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
   #ifdef ___TURBOC__
      #pragma hdrstop
   #endif
#endif

#ifndef S4SERVER

int S4FUNCTION expr4calcRemove( EXPR4CALC *calc, int removeAll )
{
   LIST4 saveCalcList, tempCalcList ;
   EXPR4CALC *calcOn ;
   CODE4 *c4base = calc->expr->codeBase ;
   int transferedOne ;
   EXPR4 *expr ;

   c4memcpy( &saveCalcList, &c4base->calcList, sizeof(saveCalcList) ) ;
   c4memcpy( &tempCalcList, &c4base->calcList, sizeof(saveCalcList) ) ;
   c4memset( &c4base->calcList, 0, sizeof(c4base->calcList) ) ;

   l4remove( &tempCalcList, calc ) ;

   /* Check other calculations */
   for( transferedOne = 1 ; transferedOne ; )
   {
      transferedOne = 0 ;
      for( calcOn = (EXPR4CALC *)l4first(&tempCalcList) ; calcOn ; )
      {
         EXPR4CALC *calcNext = (EXPR4CALC *)l4next( &tempCalcList, calcOn ) ;

         c4base->errExpr = 0 ;
         expr = expr4parseLow( calcOn->expr->data, expr4source(calcOn->expr), 0 ) ;
         c4base->errExpr = 1 ;
         if( expr )
         {
            expr4free( expr ) ;
            l4remove( &tempCalcList, calcOn ) ;
            l4add( &c4base->calcList, calcOn ) ;
            transferedOne = 1 ;
         }

         calcOn = calcNext ;
      }
   }

   if( removeAll == 0 )
   {
      if( tempCalcList.nLink > 0 )
      {
         l4add( &tempCalcList, calc ) ;
         for( ;; )
         {
            calcOn = (EXPR4CALC *)l4pop( &tempCalcList ) ;
            if ( calcOn == 0 )
               return 1 ;
            l4add( &c4base->calcList, calcOn ) ;
         }
      }
      l4add( &c4base->calcList, calc ) ;
      return 0 ;
   }

   /* Remove calculations */
   l4add( &tempCalcList, calc ) ;

   for( ;; )
   {
      calcOn = (EXPR4CALC *)l4pop( &tempCalcList) ;
      if ( calcOn == 0 )
         return 0 ;
      l4add( &c4base->calcList, calcOn ) ;
      expr4calcDelete( c4base, calcOn ) ;
   }
}

int S4FUNCTION expr4calcModify( EXPR4CALC *calc, char *exprSource )
{
   EXPR4 *expr ;
   EXPR4CALC *nextCalc, *calcNext ;
   CODE4 *codeBase ;
   LIST4 saveCalcList, tempCalcList ;
   EXPR4CALC *calcOn ;
   int transferedOne ;

   codeBase = calc->expr->codeBase ;

   c4memcpy( &saveCalcList, &codeBase->calcList, sizeof(saveCalcList) ) ;
   c4memcpy( &tempCalcList, &codeBase->calcList, sizeof(saveCalcList) ) ;
   c4memset( &codeBase->calcList, 0, sizeof(codeBase->calcList) ) ;

   l4remove( &tempCalcList, calc ) ;

   /* Check other calculations */
   for( transferedOne = 1 ; transferedOne ; )
   {
      transferedOne = 0 ;
      for( calcOn = (EXPR4CALC *)l4first(&tempCalcList) ; calcOn ; )
      {
         calcNext = (EXPR4CALC *)l4next( &tempCalcList, calcOn ) ;

         codeBase->errExpr = 0 ;
         expr = expr4parseLow( calcOn->expr->data, expr4source( calcOn->expr ), 0 ) ;
         codeBase->errExpr = 1 ;
         if( expr )
         {
            expr4free( expr ) ;
            l4remove( &tempCalcList, calcOn ) ;
            l4add( &codeBase->calcList, calcOn ) ;
            transferedOne = 1 ;
         }

         calcOn = calcNext ;
      }
   }

   expr = expr4parseLow( calc->expr->data, exprSource, 0 ) ;

   l4add( &codeBase->calcList, calc ) ;

   if( tempCalcList.nLink > 0 )
      for( ;; )
      {
         calcOn = (EXPR4CALC *)l4pop( &tempCalcList ) ;
         if ( calcOn == 0 )
            break ;
         l4add( &codeBase->calcList, calcOn ) ;
      }

   if( expr == 0 )
      return -1 ;

   if ( calc->expr != 0 )
      expr4free( calc->expr ) ;
   calc->expr = expr ;
   expr4calcMassage( calc ) ;

   nextCalc = (EXPR4CALC *)l4first( &codeBase->calcList ) ;
   while( nextCalc )
   {
      expr = expr4parseLow( nextCalc->expr->data, expr4source( nextCalc->expr ), 0 ) ;
      if( expr == 0 )
         return -1 ;

      if ( nextCalc->expr != 0 )
         expr4free( nextCalc->expr ) ;
      nextCalc->expr = expr ;
      nextCalc->curResultPos = 0 ;
      expr4calcMassage( nextCalc ) ;
      nextCalc = (EXPR4CALC *)l4next( &codeBase->calcList, nextCalc ) ;
   }

        return 0 ;
}

#endif /* S4SERVER */
