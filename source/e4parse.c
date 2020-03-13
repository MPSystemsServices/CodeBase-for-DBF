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

/* e4parse.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* Restrictions - STR can only have a constant 2nd & 3rd parameters
                  SUBSTR can only have a constant 2nd & 3rd parameters
                  LEFT can only have a constant 2nd parameter
                  RIGHT can only have a constant 2nd paramater, and the
                    first paramater must be a field or constant only.
                  IIF must have a predictable type
                  TRIM, LTRIM & ALLTRIM return unpredictable lengths.
                       Its result
                       Ex. TRIM(L_NAME) + TRIM(F_NAME) is OK
                       SUBSTR( TRIM(L_NAME), 3, 2 ) is not OK
                  Memo field's evaluate to a maximum length.  Anything over
                  this maximum gets truncated.
                  PADL and PADR functions must have a constant 2nd (length) paramater
                  ASCEND & DESCEND produce machine-byte collations or the result.
                  Therefore, ASCEND( STRING_FIELD ), if you choose a collation for
                  the index, the collation is ignored for this expression type.


   Known inconsistencies -
                  DTOC( x, 1 ) where x is a date value is supposed to operate
                    the same as DTOS( x ).  In actual fact, you can obtain the
                    same effect by using anything as the 2nd paramater (i.e.
                    doesn't have to be a '1').
                  TTOC( x, 1 ) - we only support this function with the optional
                    '1' paramater (used primarily for indexing).  Also, it is only
                    supported with r4date and r4dateTime types (not r4dateTimeMilli).
                    Similar to DTOC, any paramater can be used in place of the '1'
                  The contain ($) operator does not work correctly when
                    applied with a TRIM().  eg: "TRIM( 'BOB ' ) $ 'BOBJ'"
                    returns failure.
                  ALLTRIM() and TRIM() functions do not always work as
                    expected in combination with the comparison functions.
                    For example:
                       "ALLTRIM( ' george ' ) > 'george' ) returns TRUE
                       instead of FALSE (i.e. they should be equal).
                  Fox 3.0 compatible files do not work with the new field
                    types if an index file includes a table name qualifier
                    (eg. "data->intField" where data is the data file
                     being indexed on.)  To get around this, do not use
                     the qualifer in index files (eg. index on "intField".)
                    Since CodeBase disallows other data-file qualifiers within
                    index files, this is not a major limitation.
                  String comparison of varying length appears to work different
                    than FoxPro.  For example, expr "AAAA" = "AAA" and expr
                    "AAA" = "AAAA" both return 'TRUE' in CodeBase, but only
                    the 2nd returns 'TRUE' in FoxPro.
*/

#include "d4all.h"
#ifdef __TURBOC__
   #pragma hdrstop
#endif

#ifndef S4PALM
   #include <ctype.h>
#endif

static int e4lookupInFunctionArray( const unsigned char *str, const int strLen, const int startI, const int endI ) ;

static void s4scanInit( S4SCAN *, const unsigned char * ) ;
static char s4scanPop( S4SCAN * ) ; /* Returns current char and goes to the next */
static int s4scanSearch( S4SCAN *, const char ) ; /* Returns # of characters scanned */
static int s4scanRange( S4SCAN *, const int, const int ) ;
static unsigned char s4scanChar( S4SCAN *s4 ) ;
static void e4functionPop( EXPR4 * ) ;

static int s4stackCur( S4STACK * ) ;
static int s4stackPop( S4STACK * ) ;
static int s4stackPushInt( S4STACK *, const int ) ;
static int s4stackPushStr( S4STACK *, const void *, const int ) ;

/* e4massage
   -  Check the type returns to ensure that functions get the correct type
      result.  Use 'E4FUNCTIONS.code' to change the function where possible
      so that the correct function is used.
   -  Make sure that field parameters are put on the stack for the concatentate
      operators.
   -  Fill in the function pointers.
   -  Change (FIELD4 *) pointers in 'p1' to (char *) pointers.
   -  Where the result stack is used, make sure the correct values are filled
      into the E4INFO entires in order to adjust for the lengths needed.
   -  Check the length returns to make sure that 'codeBase->exprBufLen' is large enough
      to handle executing the expression.
   -  Calculate the length of the final result.
   -  Enforce restrictions to TRIM, STR and IIF
   -  Make sure an extra max. length character is added for e4upper() & e4trim()
*/

/* e4lookup, searches 'v4functions' for an operator or function.

       str - the function name
       strLen - If 'strLen' is greater than or equal to zero it contains the
                 exact number of characters in 'str' to lookup.  Otherwise,
                 as many as needed, provided an ending null is not reached,
                 are compared.
*/

// AS Oct 10/01 - e4lookup() must be exported always since it is available externally and reqd. for build with def file
int S4FUNCTION e4lookup( const unsigned char *str, const int strLen, const int startI, const int endI )
{
   #ifdef S4OFF_REPORT
      return error4( 0, e4notSupported, E90906 ) ;
   #else
      char uStr[9] ;  /* Maximum # of function name characters plus one. */
      int i ;

      #ifdef E4PARM_LOW
         if ( str == 0 || endI < startI )
            return error4( 0, e4parm, E90906 ) ;
      #endif

      u4ncpy( uStr, (char *)str, sizeof( uStr ) ) ;
      c4upper( uStr ) ;

      for( i=startI; i<= endI; i++)
      {
         if ( v4functions[i].code < 0 )
            break ;
         if ( v4functions[i].name == 0 )
            continue ;
         #ifdef E4ANALYZE
            if ( v4functions[i].nameLen >= (char)sizeof( uStr ) )
               return error4( 0, e4result, E90906 ) ;
         #endif

         if ( v4functions[i].name[0] == uStr[0] )
            if( strLen == v4functions[i].nameLen || strLen < 0 )
               if ( strncmp( uStr, v4functions[i].name, (size_t)v4functions[i].nameLen ) == 0 )
                  return i ;
      }
      return -1 ;
   #endif
}


static int e4massage( E4PARSE *p4 )
{
   int parmPos, iParm, isOk, codeOn ;
   int typeShouldBe, len, i, doneTrimMemoOrCalc ;
   int position[E4MAX_STACK_ENTRIES+1] ;
   long length[E4MAX_STACK_ENTRIES] ;
   int types[E4MAX_STACK_ENTRIES] ;
   int numEntries[E4MAX_STACK_ENTRIES] ;
   E4INFO *pointers[E4MAX_STACK_ENTRIES] ;
   unsigned storedKeyLen ;
   #ifdef S4DATA_ALIGN
      int rem ;
   #endif

   #ifdef E4PARM_LOW
      if ( p4 == 0 )
         return error4( 0, e4parm_null, E90901 ) ;
   #endif

   #ifdef E4ANALYZE
      memset( types, 255, sizeof( types ) ) ;
      memset( pointers, 255, sizeof( pointers ) ) ;
      memset( length, 255, sizeof( length ) ) ;
      memset( numEntries, 255, sizeof( numEntries ) ) ;
      memset( position, 255, sizeof( position ) ) ;
   #else
      #ifdef E4MISC
         memset( types, 0, sizeof( types ) ) ;
         memset( pointers, 0, sizeof( pointers ) ) ;
         memset( length, 0, sizeof( length ) ) ;
         memset( numEntries, 0, sizeof( numEntries ) ) ;
         memset( position, 0, sizeof( position ) ) ;
      #endif
   #endif

   CODE4 *codeBase = p4->codeBase ;
   int numParms = doneTrimMemoOrCalc = 0 ;
   long bufLenNeeded = 0 ;

   position[0] = 0 ; /* The first parameter can be placed at position 0 */

   for ( int iInfo = 0; iInfo < p4->expr.infoN; iInfo++ )
   {
      E4INFO *info = p4->expr.info + iInfo ;

      /* Check the parameters types */
      codeOn = v4functions[info->functionI].code ;
      if ( v4functions[info->functionI].numParms != (char)info->numParms )
         if ( v4functions[info->functionI].numParms > 0 )
         {
            if( codeBase->errExpr )
               return error4describe( codeBase, e4numParms, E90901, p4->expr.source, 0, 0 ) ;
            return e4numParms ;
         }

      for( ;; )
      {
         if ( codeOn != v4functions[info->functionI].code )
         {
            if( codeBase->errExpr )
               return error4describe( codeBase, e4typeSub, E90901, p4->expr.source, 0, 0 ) ;
            return e4typeSub ;
         }

         isOk = 1 ;

         for( iParm = 0; iParm < info->numParms; iParm++ )
         {
            if ( (int)v4functions[info->functionI].numParms < 0 )
               typeShouldBe = v4functions[info->functionI].type[0] ;
            else
               typeShouldBe = v4functions[info->functionI].type[iParm] ;

            parmPos = numParms - info->numParms + iParm ;

            if ( types[parmPos] != typeShouldBe )
            {
               if ( types[parmPos] == r4date && typeShouldBe == r4dateDoub )
               {
                  pointers[parmPos]->functionI = E4FIELD_DATE_D ;
                  length[parmPos] = sizeof(double) ;
                  continue ;
               }
               if ( types[parmPos] == r4num && typeShouldBe == r4numDoub )
               {
                  pointers[parmPos]->functionI = E4FIELD_NUM_D ;
                  length[parmPos] = sizeof(double) ;
                  continue ;
               }
               if ( types[parmPos] == r4int && typeShouldBe == r4numDoub )
               {
                  pointers[parmPos]->functionI = E4FIELD_INT_D ;
                  length[parmPos] = sizeof(double) ;
                  continue ;
               }
               // AS July 27/01 - field value is stored in intel ordering always, so need a special function to extract
               // the value in local double ordering format
               // if ( types[parmPos] == r4double && typeShouldBe == r4numDoub )
               // {
               //    pointers[parmPos]->functionI = E4FIELD_DOUB ;
               //    length[parmPos] = sizeof(double) ;
               //    continue ;
               // }
               if ( types[parmPos] == r4currency && typeShouldBe == r4numDoub )
               {
                  pointers[parmPos]->functionI = E4FIELD_CUR_D ;
                  length[parmPos] = sizeof(double) ;
                  continue ;
               }
               // AS Jul 21/05 - Support for new field type binary float
               // if ( types[parmPos] == r4floatBin && typeShouldBe == r4numDoub )
               // {
               //    pointers[parmPos]->functionI = E4FIELD_BINFLOAT_D ;
               //    length[parmPos] = sizeof(double) ;
               //    continue ;
               // }
               // if ( types[parmPos] == r4num && typeShouldBe == r4floatBin )
               // {
               //    pointers[parmPos]->functionI = E4FIELD_BINFLOAT_D ;
               //    length[parmPos] = sizeof(float) ;
               //    continue ;
               // }
               if ( types[parmPos] == r4numDoub && typeShouldBe == r4floatBin && pointers[parmPos]->functionI == E4DOUBLE)
               {
                  pointers[parmPos]->functionI = E4FLOAT ;
                  length[parmPos] = sizeof(float) ;
                  continue ;
               }
               if ( types[parmPos] == r4int && typeShouldBe == r4floatBin )
               {
                  pointers[parmPos]->functionI = E4FIELD_INT_F ;
                  length[parmPos] = sizeof(float) ;
                  continue ;
               }
               info->functionI++ ;
               isOk = 0 ;
               break ;
            }
         }
         if ( isOk )
            break ;
      }

      switch( info->functionI )
      {
         case E4DESCEND_STR:
         case E4ASCEND_STR:
         // AS May 11/06 - support for ascend in general collation
         // in this case, the ASCEND'd strings get converted to their long form as part of the ascend, and then everything works as per normal.
         case E4DESCEND_NUM:
         case E4DESCEND_DATE_D:
         case E4DESCEND_LOG:
         case E4ASCEND_NUM:
         case E4ASCEND_DATE:
         case E4ASCEND_DATE_D:
         case E4ASCEND_LOG:
         case E4DESCEND+4:
         case E4DESCEND+5:
         case E4DESCEND+6:
         case E4DESCEND+7:
         case E4DESCEND+8:
         case E4DESCEND+9:
         case E4DESCEND+10:
         case E4DESCEND+11:
         case E4DESCEND+12:
         case E4DESCEND+13:
         case E4DESCEND+14:
         case E4DESCEND+15:
         case E4DESCEND+16:
         case E4ASCEND+1:
         case E4ASCEND+2:
         case E4ASCEND+7:
         case E4ASCEND+8:
         case E4ASCEND+9:
         case E4ASCEND+10:
         case E4ASCEND+11:
         case E4ASCEND+12:
         case E4ASCEND+13:
         case E4ASCEND+14:
         case E4ASCEND+15:
         case E4ASCEND+16:
         case E4ASCEND+17:
            // AS May 10/06 - if necessary, apply the collating sequence to this part of the string...we need to track the tag in that case
            if ( p4->expr.tagPtr )  /* max size is key size */
               info->tagPtr = p4->expr.tagPtr ;
            break ;
         case E4CONCATENATE:
         case E4CONCATENATE+1:
         case E4CONCAT_TWO:
         case E4TRIM:
         case E4LTRIM:
         case E4ALLTRIM:
         case E4PADL:
         case E4PADR:
         case E4UPPER:
         case E4SUBSTR:
         case E4LEFT:
         case E4RIGHT:
         // AS July 25/02 new function
         case E4SPACE:
         #ifndef S4MEMO_OFF
            case E4FIELD_MEMO:
         #endif
            /* AS 12/04/98 -- add check to ensure that all the cases above are == to # of ascend
               and descend cases in e4functi.c */
            assert5( E4NUM_DESCEND_FUNCTIONS == 17 && E4NUM_ASCEND_FUNCTIONS == 18 ) ;
            for( iParm = 1; iParm <= info->numParms; iParm++ )
            {
               E4INFO *info_parm = pointers[numParms-iParm] ;
               if ( info_parm->functionI == E4FIELD_STR )  /* Make sure the parameter is put on the stack. */
                  info_parm->functionI = E4FIELD_STR_CAT ;
               if ( info_parm->functionI == E4FIELD_WSTR )   /* Make sure the parameter is put on the stack. */
                  info_parm->functionI = E4FIELD_STR_CAT + 1 ;
               if ( info->functionI == E4CONCATENATE  &&  doneTrimMemoOrCalc )
                  info->functionI = E4CONCAT_TRIM ;
            }
            break ;
         // AS 09/20/00 new function
         case E4DATETIME:
            // i1 stores the numParms...
            // info->i1 = (*infoI1) ;
            break ;
         // AS Jan 6/05 - Added support for a new KEYEQUAL function (useful with general collations for example to make a-accent == a for partial match)
         case E4KEYEQUAL:
            if ( p4->expr.tagPtr )  /* max size is key size */
               info->tagPtr = p4->expr.tagPtr ;
            break ;
         default:
            break ;
      }

      numParms -= info->numParms ;
      if ( numParms < 0 )
         if( codeBase->errExpr )
            return error4( codeBase, e4result, E90901 ) ;

      types[numParms] = v4functions[info->functionI].returnType ;

      if ( info->functionI == E4CALC_FUNCTION )
      {
         EXPR4CALC *calc = ((EXPR4CALC *) info->p1) ;
         types[numParms] = expr4type( calc->expr ) ;
      }
      switch( types[numParms] )
      {
         case r5wstr:
            switch( info->functionI )
            {
               case E4FIELD_WSTR:
                  length[numParms] = f4len( info->fieldPtr ) ;
                  break ;
               case E4CONCATENATE+1:
                  info->i1 = (int) (length[numParms]) ;
                  length[numParms] += length[numParms+1] ;
                  break ;
               default:
                  return error4( codeBase, e4result, E90901 ) ;
            }
            break ;
         case r5wstrLen:
            switch( info->functionI )
            {
               case E4FIELD_WSTR_LEN:
                  /* length - subtract 2 so that we lose the length setting */
                  length[numParms] = f4len( info->fieldPtr ) - sizeof( unsigned short ) ;
                  break ;
               default:
                  info->i1 = (int) (length[numParms]) ;
                  length[numParms] += length[numParms+1] ;
                  break ;
            }
            break ;
         case r4str:
            switch( info->functionI )
            {
               case E4FIELD_STR:
               case E4FIELD_STR_CAT:
                  length[numParms] = f4len( info->fieldPtr ) ;
                  break ;
               #ifndef S4MEMO_OFF
                  case E4FIELD_MEMO:
                     if ( p4->expr.tagPtr )  /* max size is key size */
                     {
                        /* for - ole-db memos never there, ensure limited to size of supporeted key size */
                        p4->expr.hasMemoField = 1 ;
                        unsigned short sizeMemoExpr = code4memSizeMemoExprGet(codeBase) ;

                        // AS Oct 18/05 - can relax this constraint...if the memo field is used for intermediary purposes this
                        // can cause loss of accuracy.  We catch the index max key length at index creation time.
                        if ( p4->expr.infoN == 1 )  // means simple index on memo field, in that case we enforce this limit
                        {
                           #ifdef S4FOX
                              /* for fox, extra byte for potential null field */
                              length[numParms] = I4MAX_KEY_SIZE_COMPATIBLE - info->fieldPtr->null ;
                           #else
                              length[numParms] = I4MAX_KEY_SIZE_COMPATIBLE ;
                           #endif
                           if ( (long)sizeMemoExpr < length[numParms] )
                              length[numParms] = sizeMemoExpr ;
                        }
                        else
                           length[numParms] = sizeMemoExpr ;
                     }
                     else
                        length[numParms] = code4memSizeMemoExprGet(codeBase) ;
                     doneTrimMemoOrCalc = 1 ;
                     break ;
               #endif  /* S4MEMO_OFF */
               case E4CONCATENATE:
               case E4CONCAT_TWO:
               case E4CONCAT_TRIM:
                  info->i1 = (int) (length[numParms]) ;
                  length[numParms] += length[numParms+1] ;
                  break ;
               case E4IIF:
                  // if ( length[numParms+1] != length[numParms+2] )
                  //    if( codeBase->errExpr )
                  //       return error4describe( codeBase, e4lengthErr, E90901, p4->expr.source, 0, 0 ) ;
                  // AS 11/15/99 -- allow IIF to have varying lengths...
                  info->i1 = length[numParms+1] ;
                  info->i2 = length[numParms+2] ;
                  /* LY 00/01/26 : max() not supported by all compilers */
                  length[numParms] = ( length[numParms+1] > length[numParms+2] ?
                     length[numParms+1] : length[numParms+2] ) ;
                  break ;
               case E4DTOS:
               case E4DTOS+1:
                  length[numParms] = sizeof(double) ;
                  break ;
               // AS Apr 22/04 - TTOC support
               case E4TTOC:
                  length[numParms] = sizeof(double) ;
                  info->i1 = p4->constants.pos ;
                  len = 14 ;
                  length[numParms] = len ;
                  break ;
               case E4TTOC+1:
                  length[numParms] = sizeof(double) ;
                  info->i1 = p4->constants.pos ;
                  len = 14 ;
                  length[numParms] = len ;
                  break ;
               case E4TTOC+2:
                  length[numParms] = sizeof(double) ;
                  info->i1 = p4->constants.pos ;
                  len = 14 ;
                  length[numParms] = len ;
                  break ;
               case E4DTOC:
               case E4DTOC+1:
               case E4CTOD:
                  length[numParms] = sizeof(double) ;
                  info->i1 = p4->constants.pos ;
                  len = c4strlen( code4dateFormat( codeBase ) ) ;
                  s4stackPushStr( &p4->constants, code4dateFormat( codeBase ), len + 1 ) ;
                  if ( info->functionI == E4DTOC || info->functionI == E4DTOC+1 )
                     length[numParms] = len ;
                  break ;
               case E4CHR:
                  length[numParms] = sizeof(char) ;
                  break ;
               case E4DEL:
                  length[numParms] = sizeof(char) ;
                  #ifndef S4CLIENT
                  if ( p4->expr.tagPtr )  /* data4 independent, so point to datafile */
                     info->p1 = (char *)&p4->expr.dataFile->record ;
                  else  /* data4 dependent, so just point to record */
                  #endif
                     info->p1 = (char *)&p4->expr.data->record ;
                  break ;
               case E4CALC_FUNCTION:
                  doneTrimMemoOrCalc = 1 ;
                  length[numParms] = expr4len( ((EXPR4CALC *) info->p1)->expr ) ;
                  break ;
               case E4RIGHT:
                  if ( info->i1 > (int)(length[numParms]) )
                     info->i1 = (int)(length[numParms]) ;
                  if ( info->i1 < 0 )
                     info->i1 = 0 ;
                  if ( info->len > (int)(length[numParms]) )
                     info->len = (int)(length[numParms]) ;
                  length[numParms] = info->len ;
                  break ;
               // AS July 25/02 new function
               case E4SPACE:
                  length[numParms] = info->len ;
                  break ;
               case E4SUBSTR:
               case E4LEFT:
                  if ( info->i1 > (int)(length[numParms]) )
                     info->i1 = (int)(length[numParms]) ;
                  if ( info->i1 < 0 )
                     info->i1 = 0 ;
                  length[numParms] -= info->i1 ;
                  if ( info->len > (int)(length[numParms]) )
                     info->len = (int)(length[numParms]) ;
                  length[numParms] = info->len ;
                  break ;
               case E4TIME:
                  length[numParms] = sizeof(double) ;
                  break ;
               case E4TRIM:
               case E4LTRIM:
               case E4ALLTRIM:
                  doneTrimMemoOrCalc = 1 ;
                  p4->expr.hasTrim = 1 ;
                  break ;
               case E4PADL:
               case E4PADR:
                  // if ( info->i1 > (int)(length[numParms]) )
                  //   info->i1 = (int)(length[numParms]) ;
                  //if ( info->i1 < 0 )
                  //   info->i1 = 0 ;
                  //length[numParms] = info->i1 ;
                  //if ( info->len > (int)(length[numParms]) )
                  //   info->len = (int)(length[numParms]) ;
                  // info->i1 will contain the number of extra blanks to add.  This is == to
                  // info->len - the length of last paramater
                  info->i1 = info->len - length[numParms] ;
                  if ( info->i1 < 0 )  // means we are really doing a trip, no need to memset to zero
                     info->i1 = 0 ;
                  length[numParms] = info->len ;
                  break ;
               case E4L2BIN:
                  length[numParms] = 4 ;  // converts numeric to a 4 byte long int value
               case E4UPPER:
                  break ;
               case E4DESCEND:
               case E4DESCEND+1:
               case E4DESCEND+2:
               case E4DESCEND+3:
               case E4DESCEND+4:
               case E4DESCEND+5:
               case E4DESCEND+6:
               case E4DESCEND+7:
               case E4DESCEND+8:
               case E4DESCEND+9:
               case E4DESCEND+10:
               case E4DESCEND+11:
               case E4DESCEND+12:
               case E4DESCEND+13:
               case E4DESCEND+14:
               case E4DESCEND+15:
               case E4DESCEND+16:
               case E4ASCEND:
               case E4ASCEND+1:
               case E4ASCEND+2:
               case E4ASCEND+3:
               case E4ASCEND+4:
               case E4ASCEND+5:
               case E4ASCEND+6:
               case E4ASCEND+7:
               case E4ASCEND+8:
               case E4ASCEND+9:
               case E4ASCEND+10:
               case E4ASCEND+11:
               case E4ASCEND+12:
               case E4ASCEND+13:
               case E4ASCEND+14:
               case E4ASCEND+15:
               case E4ASCEND+16:
               case E4ASCEND+17:
                  /* AS 12/04/98 -- add check to ensure that all the cases above are == to # of ascend
                     and descend cases in e4functi.c */
                  assert5( E4NUM_DESCEND_FUNCTIONS == 17 && E4NUM_ASCEND_FUNCTIONS == 18 ) ;
                  #ifdef S4FOX
                     if ( info->tagPtr )  /* max size is key size */
                        info->tagPtr->hasAscendOrDescend = 1 ;
                  #endif
                  /* AS 01/15/98 0 types[numParms] is always return type r4str, we are interested in the base type... */
                  switch( v4functions[info->functionI].type[0] )
                  {
                     case r4dateDoub:
                     case r4numDoub:
                     case r4dateTime:
                     case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
                        length[numParms] = sizeof( double ) ;
                        break ;
                     case r4log:
                        length[numParms] = sizeof( char ) ;
                        break ;
                     case r4int:
                     #ifdef S5USE_EXTENDED_TYPES
                        case r5ui4:
                     #endif
                        length[numParms] = sizeof( long ) ;
                        break ;
                     #ifdef S5USE_EXTENDED_TYPES
                        case r5i8:
                           length[numParms] = 8 ;
                           break ;
                        case r5dbDate:
                        case r5dbTime:
                           length[numParms] = 6 ;
                           break ;
                        case r5dbTimeStamp:
                           length[numParms] = 16 ;
                           break ;
                        case r5i2:
                        case r5ui2:
                           length[numParms] = sizeof( long ) ;  // these types stored as 4 byte in index
                           break ;
                     #endif
                     // AS May 11/06 - support for ascend in general collation
                     #ifdef S4FOX
                        case r4str:
                           {
                              if ( info->tagPtr )  /* max size is key size */
                              {
                                 switch( info->functionI )
                                 {
                                    case E4DESCEND_STR:
                                    case E4ASCEND_STR:
                                       // info->tagPtr->hasAscendOrDescend = 1 ;
                                       if ( info->tagPtr->collateName != collate4none )   // needs to be initialized to do this piece...
                                       {
                                          COLLATE4 *collate = collation4get( info->tagPtr->collateName ) ;
                                          if ( collate->collateType != collate4machineByteOrder )
                                             length[numParms] = (info-1)->len * (1+collate->keySizeCharPerCharAdd) ;
                                       }
                                       break ;
                                       /*
                                    case E4DESCEND_NUM:
                                    case E4DESCEND_DATE_D:
                                    case E4DESCEND_LOG:
                                    case E4ASCEND_NUM:
                                    case E4ASCEND_DATE:
                                    case E4ASCEND_DATE_D:
                                    case E4ASCEND_LOG:
                                       info->tagPtr->hasAscendOrDescend = 1 ;
                                       break ;
                                       */
                                 }
                              }
                           }
                        #endif
                     default:
                        break ;
                  }
                  break ;
               default:
                  length[numParms] = info->len ;
            }
            break ;
         case r4num:
            length[numParms] = f4len( info->fieldPtr ) ;
            break ;
         case r4currency:
            length[numParms] = sizeof(double) ;
            break ;
         case r4dateTime:
         case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
            length[numParms] = sizeof(double) ;
            break ;
         case r4int:
         #ifdef S5USE_EXTENDED_TYPES
            case r5ui4:
         #endif
            length[numParms] = sizeof(S4LONG) ;
            break ;
         #ifdef S5USE_EXTENDED_TYPES
            case r5i8:
               length[numParms] = 8 ;
               break ;
            case r5dbDate:
            case r5dbTime:
               length[numParms] = 6 ;
               break ;
            case r5dbTimeStamp:
               length[numParms] = 16 ;
               break ;
            case r5i2:
            case r5ui2:
               length[numParms] = sizeof( long ) ;  // these types stored as 4 byte in index
               break ;
         #endif
         case r4numDoub:
         case r4dateDoub:
            length[numParms] = sizeof(double) ;
            if ( info->functionI == E4CTOD )
            {
               info->i1 = p4->constants.pos ;
               s4stackPushStr( &p4->constants, code4dateFormat( codeBase ), c4strlen( code4dateFormat( codeBase ) ) + 1 ) ;
            }
            if ( info->functionI == E4RECCOUNT )
               info->p1 = (char *)p4->expr.dataFile ;
            if ( info->functionI == E4RECNO )
               info->p1 = 0 ;  /* updated for c/s support, just use expr data4 */
            break ;
         // AS Jul 21/05 - Support for new field type binary float
         case r4floatBin:
            length[numParms] = sizeof( float ) ;  // these types stored as float in index
            break ;
         case r4date:
            length[numParms] = sizeof(double) ;
            break ;
         case r4log:
            // AS 04/20/00 was sometimes examining memory not initialized...
            switch( info->functionI )
            {
               case E4FIELD_LOG:
                  break ;
               case E4DELETED:
//               case E4NOT_DELETED:
                  #ifndef S4CLIENT
                     if ( p4->expr.tagPtr )  /* data4 independent, so point to datafile */
                        info->p1 = (char *)&p4->expr.dataFile->record ;
                     else  /* data4 dependent, so just point to record */
                  #endif
                     info->p1 = (char *)&p4->expr.data->record ;
                  break ;
               // AS Jun 27/03 - Added EMPTY() function - for now for r4str types only
               case E4EMPTY:
               case E4EMPTY+1:
               case E4EMPTY+2:
               case E4EMPTY+3:
               case E4EMPTY+4:
               case E4TRUE:
               case E4TRUE+1:
               case E4FALSE:
               case E4FALSE+1:
               case E4NOT:
               case E4NOT+1:
               case E4OR:
               case E4AND:
               case E4CALC_FUNCTION:
                  // these functions do not use i1, so don't need initialization below
                  break ;
               default:
                  // the purpose here is to calculate the i1/p1 for those functions which use this.
                  // i.e.e4equal(), e4greater(), e4iifStr(), e4less(), e4notEqual()
                  // i.e. i1 is required for functions with non-logical paramaters
                  {
                     #ifdef E4ANALYZE
                        // ensure not using uninitialized length
                        assert5( length[numParms] != -1 ) ;
                        assert5( length[numParms+1] != -1 ) ;
                     #endif

                     info->i1 = (int)(length[numParms+1]) ;
                     int lengthStatus = 1 ;
                     if ( length[numParms] < length[numParms+1] )
                     {
                        info->i1 = (int)(length[numParms]) ;
                        lengthStatus = -1 ;
                     }
                     if ( length[numParms] == length[numParms+1] )
                        lengthStatus = 0 ;

                     if ( info->functionI == E4GREATER )
                     {
                        if ( lengthStatus > 0 )
                           #ifdef S4WIN64 /* LY 00/09/20 */
                              info->p1 = (char *)((LONGLONG)1) ;
                           #else
                              info->p1 = (char *)1L ;
                           #endif
                        else
                           info->p1 = (char *)0L ;
                     }
                     if ( info->functionI == E4LESS )
                     {
                        if ( lengthStatus < 0 )
                           #ifdef S4WIN64 /* LY 00/09/20 */
                              info->p1 = (char *)((LONGLONG)1) ;
                           #else
                              info->p1 = (char *)1L ;
                           #endif
                        else
                           info->p1 = (char *)0L ;
                     }
                  }
                  break ;
            }
            length[numParms] = sizeof(int) ;
            break ;
         default:
            if ( codeBase->errExpr )
               return error4( codeBase, e4result, E90901 ) ;
            else
               return e4result ;
      }

      /* make sure there is enough key space allocated for the type,
         in case a partial evaluation occurs */
      #ifdef S4CLIENT
         storedKeyLen = (unsigned)(length[numParms]) ;
      #else
         switch( types[numParms] )
         {
            #ifdef S4FOX
               case r4num:
               case r4date:
               case r4numDoub:
                  storedKeyLen = sizeof( double ) ;
                  break ;
            #endif  /*  ifdef S4FOX      */
            #ifdef S4CLIPPER
               case r4num:  /* numeric field return, must fix length problem */
                  storedKeyLen = f4len( info->fieldPtr ) ;
                  break ;
               case r4numDoub:
                  storedKeyLen = codeBase->numericStrLen ;
                  break ;
            #endif  /*  ifdef S4CLIPPER  */
            #ifdef S4NDX
               case r4num:
               case r4date:
                  storedKeyLen = sizeof( double ) ;
                  break ;
            #endif  /*  ifdef S4NDX  */
            #ifdef S4MDX
               case r4num:
                  storedKeyLen = (int)sizeof( C4BCD ) ;
                  break ;
               case r4numDoub:
                  storedKeyLen = (int)sizeof( C4BCD ) ;
                  break ;
               case r4date:
               case r4dateDoub:
                  storedKeyLen = sizeof( double ) ;
                  break ;
            #endif  /* S4MDX */
            default:
               storedKeyLen = (unsigned)(length[numParms]) ;
         }

         #ifdef S4FOX
            storedKeyLen++ ;    /* null entry will increase length by one */
         #endif
      #endif

      u4allocAgain( codeBase, &codeBase->storedKey, &codeBase->storedKeyLen, (unsigned)storedKeyLen + 1 ) ;

      if ( error4code( codeBase ) < 0 )
         return -1 ;
      #ifdef S4DATA_ALIGN
         int delta = 0 ;
         switch(types[numParms])
         {
            case r4numDoub:
            case r4dateDoub:
            case r4num:
            case r4date:
            {
               if (rem=position[numParms]%sizeof(double))
                  delta=sizeof(double)-rem;
               break ;
            }
            case r4log:
            {
               if (rem=position[numParms]%sizeof(int))
                  delta=sizeof(double)-rem;
               break ;
            }
            case r4int:
            {
               if (rem=position[numParms]%sizeof(S4LONG))
                  delta=sizeof(double)-rem;
               break ;
            }
          }
         info->resultPos=position[numParms]+delta;
      #else
         info->resultPos = position[numParms] ;
      #endif

      bufLenNeeded = length[numParms] ;
      if ( info->functionI == E4CALC_FUNCTION )
         bufLenNeeded = ((EXPR4CALC *)info->p1)->expr->lenEval ;
      if( (types[numParms] == (int)r4num || types[numParms] == (int)r4date || types[numParms] == (int)r4int) && (size_t)length[numParms] < sizeof(double) ) // CS 2000/12/01
      {
         position[numParms+1] = info->resultPos + sizeof(double) ;
         // AS Apr 12/07 - In this case, the bufLenNeeded is also large...i.e. we need to have at least 8 bytes to do a double conversion, but the field is < 8 bytes...
         if ( bufLenNeeded < sizeof( double ) )
            bufLenNeeded = sizeof( double ) ;
      }
      else
      {
         position[numParms+1] = info->resultPos + (unsigned)length[numParms] ;
      }

      if( bufLenNeeded > INT_MAX )
         return error4( codeBase, e4overflow, E90901 ) ;

      if ( info->resultPos + bufLenNeeded > (long)p4->expr.lenEval )
         p4->expr.lenEval = info->resultPos + (unsigned)bufLenNeeded ;

      info->len = (int)(length[numParms]) ;
      info->numEntries = 1 ;

      for( i = 0; i < info->numParms; i++ )
         info->numEntries += numEntries[numParms+i] ;

      numEntries[numParms] = info->numEntries ;
      pointers[numParms] = info ;

      numParms++ ;
      if ( numParms >= E4MAX_STACK_ENTRIES )
         if( codeBase->errExpr )
            return error4( codeBase, e4overflow, E90901 ) ;
   }

   if ( numParms != 1 )
   {
      if( codeBase->errExpr )
         error4( codeBase, e4result, E90901 ) ;
      return -1 ;
   }

   for( i = 0; i < p4->expr.infoN; i++ )
   {
      E4INFO *info = p4->expr.info + i ;
      info->function = (S4OPERATOR *)v4functions[info->functionI].functionPtr ;
   }

   p4->expr.lenEval += 1 ;

   if ( p4->expr.exprBufLen < (unsigned)p4->expr.lenEval )
   {
      if ( u4allocAgain( codeBase, &p4->expr.exprWorkBuf, &p4->expr.exprBufLen, p4->expr.lenEval ) == e4memory )
         return error4stack( codeBase, e4memory, E90901 ) ;
   }

   p4->expr.len = (int)(length[0]) ;
   p4->expr.type = types[0] ;

   return 0 ;
}



int e4addConstant( E4PARSE *p4, const int iFunctions, const void *consPtr, const unsigned consLen )
{
   E4INFO *info ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 )
         return error4( 0, e4parm_null, E90902 ) ;
   #endif

   info = e4functionAdd( &p4->expr, iFunctions ) ;
   if ( info == 0 )
      return -1 ;
   info->i1 = p4->constants.pos ;
   info->len = consLen ;
   return s4stackPushStr( &p4->constants, consPtr, (int)consLen ) ;
}



E4INFO *e4functionAdd( EXPR4 *expr, const int iFunctions )
{
   E4INFO *info ;

   #ifdef E4PARM_LOW
      if ( expr == 0 )
      {
         error4( 0, e4parm_null, E90903 ) ;
         return 0 ;
      }
   #endif

   if ( (unsigned)((expr->infoN+1)*sizeof(E4INFO)) > expr->exprBufLen )
      if ( u4allocAgain( expr->codeBase, &expr->exprWorkBuf, &expr->exprBufLen, sizeof(E4INFO) * (expr->infoN+10) ) == e4memory )
         return 0 ;

   info = (E4INFO *)expr->exprWorkBuf + expr->infoN++ ;

   info->functionI = iFunctions ;
   info->numParms = v4functions[iFunctions].numParms ;
   if ( info->numParms < 0 )
      info->numParms = 2 ;
   info->function = (S4OPERATOR *)v4functions[iFunctions].functionPtr ;
   return info ;
}



static void e4functionPop( EXPR4 *expr )
{
   expr->infoN-- ;
}



static void expr4parseFree( E4PARSE *parse, Bool5 freeWorkBuf )
{
   // free up constants if they are allocated...
   if ( parse->constants.doExtend == EXTEND4ALLOCATED )
   {
      u4free( parse->constants.ptr ) ;
      parse->constants.ptr = 0 ;
      parse->constants.len = 0 ;
      parse->constants.doExtend = EXTEND4AVAIL ;
   }
   if ( freeWorkBuf && parse->expr.exprWorkBuf )
   {
      u4free( parse->expr.exprWorkBuf ) ;
      parse->expr.exprWorkBuf = 0 ;
   }
}



EXPR4 *S4FUNCTION expr4parseLow( DATA4 *d4, const char *exprPtr, TAG4FILE *tagPtr )
{
   E4PARSE parse ;
   char ops[128] ;
   char constants[512] ;
   char *src ;
   int rc, infoLen, posConstants ;
   EXPR4 *express4 ;

   #ifdef E4PARM_HIGH
      if ( d4 == 0 || exprPtr == 0 )
      {
         error4( 0, e4parm_null, E90904 ) ;
         return 0 ;
      }
   #endif

   if ( error4code( d4->codeBase ) < 0 )
      return 0 ;

   c4memset( (void *)&parse, 0, sizeof(E4PARSE) ) ;
   c4memset( ops, 0, sizeof(ops));

/*   if ( parse.expr.exprBufLen > 0 )
      memset( parse.expr.exprWorkBuf, 0, parse.expr.exprBufLen ) ; */

   parse.expr.tagPtr = tagPtr ;
   // #ifdef S4FOX
   //    parse.expr.vfpInfo = tagPtr ? &tagPtr->vfpInfo : 0 ;
   // #endif
   parse.expr.data   = d4 ;
   parse.expr.source = (char *)exprPtr ;
   parse.codeBase   = d4->codeBase ;
   parse.expr.codeBase = d4->codeBase ;

   parse.op.ptr = ops ;
   parse.op.len = sizeof(ops) ;
   parse.op.codeBase = d4->codeBase ;

   parse.constants.ptr = constants ;
   parse.constants.len = sizeof(constants) ;
   parse.constants.codeBase = d4->codeBase ;
   parse.constants.doExtend = EXTEND4AVAIL ;  // AS 12/30/99 allow for extension of constants...
   parse.op.doExtend = EXTEND4AVAIL ;  // CS 2000/05/08

   s4scanInit( &parse.scan, (unsigned char *)exprPtr ) ;

   rc = expr4parseExpr( &parse ) ;
   if ( rc < 0 )
   {
      expr4parseFree( &parse, 1 ) ;
      return 0 ;
   }
   if ( s4stackCur( &parse.op ) != E4NO_FUNCTION )
   {
      expr4parseFree( &parse, 1 ) ;
      if( parse.codeBase->errExpr )
         error4describe( parse.codeBase, e4complete, E90904, exprPtr, 0, 0 ) ;
      return 0 ;
   }

   parse.expr.info = (E4INFO *)parse.expr.exprWorkBuf ;
   parse.expr.dataFile = d4->dataFile ;
   if ( e4massage( &parse ) < 0 )
   {
      expr4parseFree( &parse, 1 ) ;
      return 0 ;
   }

   infoLen = parse.expr.infoN * sizeof(E4INFO) ;
   posConstants = sizeof(EXPR4) + infoLen ;

   express4 = (EXPR4 *)u4allocFree( d4->codeBase, (long)posConstants + parse.constants.len + parse.scan.len + 1L ) ;
   if ( express4 == 0 )
   {
      expr4parseFree( &parse, 1 ) ;
      if ( d4->codeBase->errExpr )
         error4( d4->codeBase, e4memory, E90904 ) ;
      return 0 ;
   }

   c4memcpy( (void *)express4, (void *)&parse.expr, sizeof(EXPR4) ) ;

   express4->data = d4 ;
   express4->dataFile = d4->dataFile ;
   express4->info = (E4INFO *)( express4 + 1 ) ;
   express4->constants = (char *)express4 + posConstants ;
   src = express4->constants + parse.constants.len ;
   express4->source = src ;

   c4memcpy( (void *)express4->info, parse.expr.exprWorkBuf, (unsigned int)infoLen ) ;

   // AS Aug 15/02 - use parse.constants.ptr, not constants since we may have reallocated
   // this value
   // c4memcpy( express4->constants, constants, parse.constants.len ) ;
   c4memcpy( express4->constants, parse.constants.ptr, parse.constants.len ) ;
   c4strcpy( src, parse.scan.len + 1L, exprPtr ) ;  // AS Dec 13/05 vs 5.0 fixes

   #ifdef S4CLIPPER
      express4->keyLen = parse.expr.keyLen ;
      express4->keyDec = parse.expr.keyDec ;
   #endif

   expr4parseFree( &parse, 0 ) ;
   return express4 ;
}


/*    Looks at the input string and returns and puts a character code on the
   result stack corresponding to the next operator.  The operators all operate
   on two operands.  Ex. +,-,*,/, >=, <, .AND., ...

      If the operator is ambiguous, return the arithmatic choice.

   Returns -2 (Done), 0, -1 (Error)
*/




int e4getOperator( E4PARSE *p4, int *opReturn)
{
   char ch ;
   int op ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 || opReturn == 0 )
         return error4( 0, e4parm_null, E90505 ) ;
   #endif

   s4scanRange( &p4->scan, 1, ' ' ) ;
   ch = s4scanChar(&p4->scan) ;
   if ( ch==0 || ch==')' || ch==',' )
   {
      *opReturn = E4DONE ;
      return 0 ;
   }

   op  = e4lookupInFunctionArray( p4->scan.ptr+p4->scan.pos, -1, E4FIRST_OPERATOR, E4LAST_OPERATOR ) ;
   if ( op < 0 )
      if( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4unrecOperator, E90905, (char *)p4->scan.ptr, 0, 0 ) ;

   p4->scan.pos += v4functions[op].nameLen ;
   *opReturn = op ;

   return 0 ;
}



static int e4lookupInFunctionArray( const unsigned char *functionName, const int functionNameLen, const int startE4functionIndex, const int endE4functionIndex )
{
   /* e4lookupInFunctionArray, searches 'v4functions' for an operator or function.
          functionName - the function name
          functionNameLen - If 'functionNameLen' is greater than or equal to zero it contains the
                    exact number of characters in 'functionName' to lookup.  Otherwise,
                    as many as needed, provided an ending null is not reached,
                    are compared.
      returns the index into the v4functions array which points to the first entry which
              function name is that input (in some cases, multiple entries in the array
              are used for the same function name, so a further search on the array
              for the appropriate entry for the given function name may be required)
      returns '-1' if the lookup fails
      returns other negative values on severe error only
   */

   char functionNameUpperCaseTrimmed[9] ;  /* Maximum # of function name characters plus one. */
   int functionIndex ;

   #ifdef E4PARM_LOW
      if ( functionName == 0 || endE4functionIndex < startE4functionIndex )
         return error4( 0, e4parm, E90906 ) ;
   #endif

   u4ncpy( functionNameUpperCaseTrimmed, (char *)functionName, sizeof( functionNameUpperCaseTrimmed ) ) ;
   c4upper( functionNameUpperCaseTrimmed ) ;

   for ( functionIndex = startE4functionIndex ; functionIndex <= endE4functionIndex ; functionIndex++ )
   {
      /* if we have reached the last element of the array we have failed in the lookup */
      if ( v4functions[functionIndex].code == E4TERMINATOR )
         return -1 ;

      /* if this entry in the function array has the same name as the previous function in
         the array, then just skip to the next entry because obviously there is no name match
      */
      if ( v4functions[functionIndex].name == 0 )
         continue ;

      #ifdef E4ANALYZE
         /* the v4function array contains a function name larger than is valid - this should never
            occur because we don't have any functions with more than 8 characters - severe error
         */
         if ( v4functions[functionIndex].nameLen >= (char)sizeof( functionNameUpperCaseTrimmed ) )
            return error4( 0, e4result, E90906 ) ;
      #endif

      /* if the first letter matches, then do a detailed search (optimization) */
      if ( v4functions[functionIndex].name[0] == functionNameUpperCaseTrimmed[0] )
      {
         /* verify that the length of the name is the same or that we are ignoring the length of the name */
         if ( functionNameLen == v4functions[functionIndex].nameLen || functionNameLen < 0 )
         {
            if ( c4strncmp( functionNameUpperCaseTrimmed, v4functions[functionIndex].name, (size_t)v4functions[functionIndex].nameLen ) == 0 )
            {
               /* the name matches, so return the index number of the array entry which matched*/
               return functionIndex ;
            }
         }
      }
   }

   /* we get here if the start function index is > end function index - in theory this
      can never happen - it is checked in E4PARM_LOW, so error out if here
   */
   /* AS 04/27/99 --> can get here if passed endE4functionIndex, in which case should return -1 - not found... */
   return -1 ;
   // return error4( 0, e4result, E90906 ) ;
}



static int opToExpr( E4PARSE *p4 )
{
   E4INFO *info ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 )
         return error4( 0, e4parm_null, E90907 ) ;
   #endif

   info = e4functionAdd( &p4->expr, s4stackPop(&p4->op) ) ;
   if ( info == 0 )
      return -1 ;

   for( ; s4stackCur(&p4->op) == E4ANOTHER_PARM ; )
   {
      s4stackPop(&p4->op) ;
      info->numParms++ ;
   }

   return 0 ;
}



int expr4parseExpr( E4PARSE *p4 )
{
   /*
        Parses an expression consisting of value [[operator value] ...]
      The expression is ended by a ')', a ',' or a '\0'.
      Operators are only popped until a '(', a ',' or the start of the stack.
      Left to right evaluation for operators of equal priority.

         An ambiguous operator is one which can be interpreted differently
      depending on its operands.  However, its operands depend on the
      priority of the operators and the evaluation order. Fortunately, the
      priority of an ambigous operator is constant regardless of its
      interpretation.  Consequently, the evaluation order is determined first.
      Then ambiguous operators can be exactly determined.

      Ambigous operators:+, -,  >, <, <=, >=, =, <>, #

      Return

          0  Normal
         -1  Error
   */

   int rc, opValue, opOnStack ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 )
         return error4( 0, e4parm_null, E90908 ) ;
   #endif

   // AS May 24/07 - for clipper set a default keylen (for numerics)
   // this is due to a recent change in the relation where we sort using the key method instead of the binary method
   #ifdef S4CLIPPER
      p4->expr.keyLen = p4->codeBase->numericStrLen ;
      p4->expr.keyDec = p4->codeBase->decimals ;
   #endif



   rc = expr4parseValue( p4 ) ;
   if ( rc < 0 )
      return error4stack( p4->codeBase, (short)rc, E90908 ) ;

   for( ;; )
   {
      rc = e4getOperator( p4, &opValue ) ;
      if ( rc < 0 )
         return error4stack( p4->codeBase, (short)rc, E90908 ) ;
      if ( opValue == E4DONE )  /* Done */
      {
         while( s4stackCur(&p4->op) != E4L_BRACKET && s4stackCur(&p4->op) != E4COMMA
                && s4stackCur(&p4->op) != E4NO_FUNCTION )
         {
            rc = opToExpr( p4 ) ;
            if ( rc < 0 )
               return error4stack( p4->codeBase, (short)rc, E90908 ) ;
         }
         return 0 ;
      }

      /* Everything with a higher or equal priority than 'opValue' must be
         executed first. (equal because of left to right evaluation order)
         Consequently, all high priority operators are sent to the result
         stack.
      */
      while ( s4stackCur( &p4->op ) >= 0 )
      {
         opOnStack = s4stackCur(&p4->op ) ;
         if ( v4functions[opValue].priority <= v4functions[opOnStack].priority )
         {
            if ( opValue == opOnStack && (int)v4functions[opValue].numParms < 0 )
            {
               /* If repeated AND or OR operator, combine them into one with an
                  extra paramter.  This makes the relate module optimization
                  algorithms easier. */
               s4stackPop( &p4->op ) ;
               s4stackPushInt( &p4->op, E4ANOTHER_PARM ) ;
               break ;
            }
            else
            {
               rc = opToExpr( p4 ) ;
               if ( rc < 0 )
                  return error4stack( p4->codeBase, (short)rc, E90908 ) ;
            }
         }
         else
            break ;
      }

      s4stackPushInt( &p4->op, opValue ) ;

      rc = expr4parseValue( p4 ) ;
      if ( rc < 0 )
         return error4stack( p4->codeBase, (short)rc, E90908 ) ;
   }
}



static int expr4parseStrFunction( E4PARSE *p4, int *functionNumber, int numParms, int *infoI1, int *infoLen )
{
   // used to parse the E4STR function...
   // returns >= 0 numParms if success, or < 0 if error

   // STR( numeric ) or STR( Widestring field )  are both valid...

   *infoLen = 10 ;
   E4INFO *info = (E4INFO *) p4->expr.exprWorkBuf + p4->expr.infoN -1 ;

   short isWide = 0 ;
   if ( info->fieldPtr != NULL )
   {
      if ( f4typeInternal( info->fieldPtr ) == r5wstr ||  f4typeInternal( info->fieldPtr ) == r5wstrLen )
         isWide = 1 ;
   }

   if ( isWide ) // wide string to character  .. STR( widestring ) function...
   {
      // AS Aug 28/03 - The output length is half the input length (From widestring to multi-byte)
      (*infoLen) = f4len( info->fieldPtr ) / 2 ;
      (*functionNumber)++ ;
   }
   else
   {
      // STR (numeric) function
      if ( numParms == 3 )
      {
         if ( info->functionI != E4DOUBLE )
         {
            if( p4->codeBase->errExpr ) // AS Dec 12/03 - fixed error message type
               return error4describe( p4->codeBase, e4typeSub, E90909, p4->expr.source, 0, 0 ) ;
            return e4notConstant ;
         }

         #ifdef S4DATA_ALIGN
            double doubVal ;
            memcpy( (void *)&doubVal, (p4->constants.ptr + info->i1), sizeof(double) ) ;
            *infoI1 = (int)doubVal ;
         #else
            *infoI1 = (int)*(double *) (p4->constants.ptr + info->i1) ;
         #endif

         e4functionPop( &p4->expr ) ;
         numParms-- ;
      }

      if ( numParms == 2  )
      {
         info = (E4INFO *) p4->expr.exprWorkBuf + p4->expr.infoN - 1 ; /* may have changed due to nParms == 3 ) */

         if ( info->functionI != E4DOUBLE )
         {
            if( p4->codeBase->errExpr ) // AS Dec 12/03 - fixed error message type
               return error4describe( p4->codeBase, e4typeSub, E90909, p4->expr.source, 0, 0 ) ;
            return e4notConstant ;
         }

         #ifdef S4DATA_ALIGN
            double doubVal ;
            memcpy( (void *)&doubVal, (p4->constants.ptr + info->i1), sizeof(double) ) ;
            *infoLen = (int) doubVal ;
         #else
            *infoLen = (int) *(double *) (p4->constants.ptr + info->i1) ;
         #endif

         e4functionPop( &p4->expr ) ;
         numParms-- ;
      }

      // AS Aug 28/03 - these infoLen adjustments only apply to the numerical version of the function
      if ( *infoLen < 0 )
         *infoLen = 10 ;

      if ( *infoLen <= (*infoI1) + 1 )
         *infoI1 = *infoLen - 2 ;

      if ( *infoI1 < 0 )
         *infoI1 = 0 ;

   }
   return numParms ;
}



/*
   AS Dec 12/03 - fixed function - 3rd paramater is the number of digits not a filler character
   StrZero( .04, 5, 3 ) truncates to "0.040"
   parses the same as expr4parseStr
*/
/*
static int expr4parseStrZeroFunction( E4PARSE *p4, int *functionNumber, int numParms, int *infoI1, int *infoLen )
{
   // used to parse the E4STRZERO function...
   // returns >= 0 numParms if success, or < 0 if error


   E4INFO *info = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN -1 ;

   if ( numParms == 3 )
   {
      if ( info->functionI != E4DOUBLE )
      {
         if( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }

      #ifdef S4DATA_ALIGN
         double doubVal ;
         memcpy( (void *)&doubVal, (p4->constants.ptr + info->i1), sizeof(double) ) ;
         *infoI1 = (int)doubVal ;
      #else
         *infoI1 = (int)*(double *) (p4->constants.ptr + info->i1) ;
      #endif

      if ( *infoI1 < 0 || * infoI1 > 9 )  // invalid value
      {
         if( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }

      e4functionPop( &p4->expr ) ;
      numParms-- ;
      info = (E4INFO *) p4->expr.exprWorkBuf + p4->expr.infoN - 1 ; // may have changed due to nParms == 3 )
   }
   else
      *infoI1 = 0 ;  // default to use '0' for filler character

   if ( info->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      double doubVal ;
      memcpy( (void *)&doubVal, (p4->constants.ptr + info->i1), sizeof(double) ) ;
      *infoLen = (int)doubVal ;
   #else
      *infoLen = (int)*(double *) (p4->constants.ptr + info->i1) ;
   #endif

   e4functionPop( &p4->expr ) ;
   numParms-- ;

   if ( *infoLen < 0 )
      *infoLen = 0 ;

   return numParms ;
}
*/



static int expr4parseRightFunction( E4PARSE *p4, int numParms, int *infoI1, int *infoLen )
{
   // used to parse the E4RIGHT function...

   int rVal ;
   #ifdef S4DATA_ALIGN
      double doubVal ;  /* LY 99/5/24 */
   #endif
   // firstParamaterInfo points to the length value (i.e. in RIGHT( "ABC", 2), points to '2')
   E4INFO *firstParamaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN -1 ;

   // secondParamaterInfo points to the constant/field value (i.e. in RIGHT( "ABC", 2), points to 'ABC')
   E4INFO *secondParamaterInfo = firstParamaterInfo - 1 ;   /* get the constant/field len */

   if ( firstParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, ( p4->constants.ptr + firstParamaterInfo->i1 ), sizeof(double) ) ;
      (*infoI1) = (int)doubVal ;
   #else
      (*infoI1) = (int)*(double *)( p4->constants.ptr + firstParamaterInfo->i1 ) ;
   #endif

   if ( secondParamaterInfo->fieldPtr != 0 )   /* is a field */
      rVal = f4len( secondParamaterInfo->fieldPtr ) - (*infoI1) ;
   else  /* assume a string constant */
   {
      // AS Dec 15/04 - support for subexpressions...
      if ( secondParamaterInfo->functionI != E4STRING )
      {
         // in this case it is variable...just store the offset amount...
         rVal = (*infoI1) ;
         // ((E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN)->varLength = 1 ;
         // return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      }
      else
         rVal = secondParamaterInfo->len - (*infoI1) ;
   }
   *infoLen = *infoI1 ;
   (*infoI1) = rVal ;
   if ( (*infoLen) < 0 )
     *infoLen = 0 ;

   e4functionPop( &p4->expr ) ;
   numParms-- ;
   return numParms ;
}



// AS July 25/02 new function
static int expr4parseSpaceFunction( E4PARSE *p4, int *infoLen )
{
   // used to parse the E4RIGHT function...

   // int rVal ;
   #ifdef S4DATA_ALIGN
      double doubVal ;
   #endif
   // firstParamaterInfo points to the numerical paramater (# of spaces)
   E4INFO *firstParamaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN -1 ;

   if ( firstParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, ( p4->constants.ptr + firstParamaterInfo->i1 ), sizeof(double) ) ;
      (*infoLen) = (int)doubVal ;
   #else
      (*infoLen) = (int)*(double *)( p4->constants.ptr + firstParamaterInfo->i1 ) ;
   #endif

   return 0 ;
}



static int expr4parsePadFunction( E4PARSE *p4, int numParms, int *infoI1, int *infoLen )
{
   // used to parse the E4PADL and E4PADR functions...

   // firstParamaterInfo will point to the 1st (left) paramater
   E4INFO *firstParamaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN -1 ;
   #ifdef S4DATA_ALIGN
      double doubVal ;  /* LY 99/5/24 */
   #endif

   if ( firstParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, ( p4->constants.ptr + firstParamaterInfo->i1 ), sizeof(double) ) ;
      (*infoLen) = (int)doubVal ;
   #else
      (*infoLen) = (int)( *(double *)( p4->constants.ptr + firstParamaterInfo->i1 ) ) ;
   #endif

   e4functionPop( &p4->expr ) ;
   numParms-- ;

   return numParms ;
}



static int expr4parseSubStr2parmFunction( E4PARSE *p4, int numParms, int *infoI1, int *infoLen )
{
   // used to parse the SubStr function when 2 paramaters

   int rVal ;
   E4INFO *firstParamaterParamaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN - 1 ;
   E4INFO *secondParamaterInfo = firstParamaterParamaterInfo - 1 ;   /* get the constant/field len */
   #ifdef S4DATA_ALIGN
      double doubVal ;  /* LY 99/5/24 */
   #endif

   if ( firstParamaterParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, ( p4->constants.ptr + firstParamaterParamaterInfo->i1 ), sizeof( double ) ) ;
      (*infoI1) = (int)doubVal ;
   #else
      (*infoI1) = (int)*(double *)( p4->constants.ptr + firstParamaterParamaterInfo->i1 ) ;
   #endif

   (*infoI1)-- ;
   if ( secondParamaterInfo->fieldPtr != 0 )   /* is a field */
      rVal = f4len( secondParamaterInfo->fieldPtr ) - (*infoI1) ;
   else  /* assume constant */
      rVal = secondParamaterInfo->len - (*infoI1) ;
   *infoLen = rVal ;
   if ( (*infoLen) < 0 )
     *infoLen = 0 ;
   e4functionPop( &p4->expr ) ;
   numParms-- ;

   return numParms ;
}



static int expr4parseSubStr3parmOrLeftFunction( E4PARSE *p4, int numParms, int *infoLen )
{
   // used to parse the SubStr function when 3 paramaters or to parse the left function

   E4INFO *firstParamaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN -1 ;
   #ifdef S4DATA_ALIGN
      double doubVal ;  /* LY 99/5/24 */
   #endif

   if ( firstParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, ( p4->constants.ptr + firstParamaterInfo->i1 ), sizeof(double) ) ;
      (*infoLen) = (int)doubVal ;
   #else
      (*infoLen) = (int)( *(double *)( p4->constants.ptr + firstParamaterInfo->i1 ) ) ;
   #endif

   e4functionPop( &p4->expr ) ;
   numParms-- ;

   return numParms ;
}



static int expr4parseDateTimeParmFunction( E4PARSE *p4, int *functionNumber, int numParms, int *infoI1 )
{
   // we trim off the time portion and put it into i1, then leave the date portion to be parsed as per normal.

   // ensure all parm types are double...
   int timeVal = 0 ;

   if ( numParms >= 7 ) // milliseconds
   {
      E4INFO *paramaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN - 1 ;
      if ( paramaterInfo->functionI != E4DOUBLE )
      {
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }
      double doubVal ;
      c4memcpy( (void *)&doubVal, (p4->constants.ptr + paramaterInfo->i1), sizeof(double) ) ;
      timeVal = (int)doubVal ;
      e4functionPop( &p4->expr ) ;
      (*functionNumber)++ ;   // we need to use the 2nd datetime function (to produce a datetimemilli).
   }

   if ( numParms >= 6 ) // seconds
   {
      E4INFO *paramaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN - 1 ;
      if ( paramaterInfo->functionI != E4DOUBLE )
      {
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }
      double doubVal ;
      c4memcpy( (void *)&doubVal, (p4->constants.ptr + paramaterInfo->i1), sizeof(double) ) ;
      timeVal += 1000 * (int)doubVal ;
      e4functionPop( &p4->expr ) ;
   }

   if ( numParms >= 5 ) // minutes
   {
      E4INFO *paramaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN - 1 ;
      if ( paramaterInfo->functionI != E4DOUBLE )
      {
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }
      double doubVal ;
      c4memcpy( (void *)&doubVal, (p4->constants.ptr + paramaterInfo->i1), sizeof(double) ) ;
      timeVal += 60000 * (int)doubVal ;
      e4functionPop( &p4->expr ) ;
   }

   if ( numParms >= 4 ) // hours
   {
      E4INFO *paramaterInfo = (E4INFO *)p4->expr.exprWorkBuf + p4->expr.infoN - 1 ;
      if ( paramaterInfo->functionI != E4DOUBLE )
      {
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
         return e4notConstant ;
      }
      double doubVal ;
      c4memcpy( (void *)&doubVal, (p4->constants.ptr + paramaterInfo->i1), sizeof(double) ) ;
      timeVal += 3600000 * (int)doubVal ;
      e4functionPop( &p4->expr ) ;
   }

   (*infoI1) = (int)timeVal ;

   numParms = 3 ;
   return numParms ;
}



static int expr4parseSubStr2parmPart2Function( E4PARSE *p4, int numParms, int *infoI1 )
{
   E4INFO *firstParamaterInfo = (E4INFO *) p4->expr.exprWorkBuf + p4->expr.infoN -1 ;
   #ifdef S4DATA_ALIGN
      double doubVal ;  /* LY 99/5/24 */
   #endif

   if ( firstParamaterInfo->functionI != E4DOUBLE )
   {
      if ( p4->codeBase->errExpr )
         error4describe( p4->codeBase, e4notConstant, E90909, p4->expr.source, 0, 0 ) ;
      return e4notConstant ;
   }

   #ifdef S4DATA_ALIGN
      memcpy( (void *)&doubVal, (p4->constants.ptr + firstParamaterInfo->i1), sizeof(double) ) ;
      (*infoI1) = (int) doubVal ;
   #else
      (*infoI1) = (int) *(double *) (p4->constants.ptr + firstParamaterInfo->i1) ;
   #endif

   (*infoI1)-- ;
   e4functionPop( &p4->expr ) ;
   numParms-- ;

   return numParms ;
}



static int expr4parseParamaters( E4PARSE *p4 )
{
   /*
      Used to parse paramaters to a function.  Basically, look for each paramater and parse
      each one.

      returns the number of paramaters found, or < 0 if error
   */

   /* keep parsing until no more paramaters */
   char ch ;
   int rc, numParms = 0 ;

   for( ;; )
   {

      ch = s4scanChar( &p4->scan ) ;

      if ( ch == 0 )
      {
         /* we reached the end of string without a closing bracked on the paramater function
            list.  This is an error */
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4rightMissing, E90909, (char *)p4->scan.ptr, 0, 0 ) ;
         return e4rightMissing ;
      }

      if ( ch == ')')
      {
         /* we have reached the end of the paramater list, so just return */
         p4->scan.pos++ ;
         return numParms ;
      }

      /* parse the paramater */
      rc = expr4parseExpr( p4 ) ;
      if ( rc < 0 )
         return error4stack( p4->codeBase, (short)rc, E90909 ) ;

      numParms++ ;

      /* */
      while( s4scanChar( &p4->scan ) <= ' ' && s4scanChar( &p4->scan ) >='\1')
         p4->scan.pos++ ;

      if ( s4scanChar( &p4->scan ) == ')')
      {
         p4->scan.pos++ ;
         break ;
      }
      if ( s4scanChar( &p4->scan ) != ',')
      {
         if( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4commaExpected, E90909, (char *)p4->scan.ptr, 0, 0 ) ;
         return e4commaExpected ;
      }
      p4->scan.pos++ ;
   }

   return numParms ;
}



static int expr4parseLookup( E4PARSE *p4, const char *functionName, const int functionNameLen, EXPR4CALC **calc, void **newOrTotalPtr )
{
   /* looks up the function name to get the v4function index.  If it not found in
      the v4functions[] array, we look through the calc expressions to see if it
      is a calc name instead.

      calc is assigned a pointer to the EXPR4CALC if the parse reveals that the return is
      a E4CALC_FUNCTION or a E4TOTAL.  newOrTotalPtr is assigned the calc if EXPR4CALC
      or the total if E4TOTAL.

      RETURNS

      if > 0 returns an index into v4functions[] array where the function name was found

      E4CALC_FUNCTION means a valid calc was found

      E4TOTAL if a valid total was found

      e4unrecFunction means the function is invalid

      other < 0 means severe error

      ERRORS

      generates an error if errExpr is true and no function or calc is found which
      matches the input.

      otherwise just returns the appropriate error code.

   */

   /* first check out the function array */
   int functionIndex = e4lookupInFunctionArray( (unsigned char *)functionName, functionNameLen, E4FIRST_FUNCTION, 0x7FFF ) ;
   if ( functionIndex >= 0 )  /* function found */
      return functionIndex ;

   /* not found in function array, check out the calcs and totals */

   *calc = expr4calcLookup( p4->codeBase, p4->expr.data, functionName, functionNameLen ) ;

   if ( *calc == 0 )
   {
      /* not found in the calcs, so return error or unrecFunction */
      if( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4unrecFunction, E90909, (char *)p4->scan.ptr, 0, 0 ) ;
      return e4unrecFunction ;
   }

   #ifndef S4SERVER
      if ( (*calc)->total != 0 ) /* is a total, not a calc */
      {
         *newOrTotalPtr = (*calc)->total ;
         return E4TOTAL ;
      }
   #endif

   *newOrTotalPtr = *calc ;
   return E4CALC_FUNCTION ;
}



int expr4parseFunction( E4PARSE *p4, const char *functionName, const int functionNameLen )
{
   /*
      This function assumes that we are at the parse point where we know that we are
      parsing a function.

      p4 is the current parse

      functionName is the start of the function name is to be parsed

      functionNameLen is the length of the function name
   */

   int functionNumber, numParms, infoI1, infoLen ;
   E4INFO *info ;
   void *newOrTotalPtr = 0 ;
   EXPR4CALC *calc = 0 ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 || functionName == 0 || functionNameLen < 0 )
         return error4( 0, e4parm, E90909 ) ;
   #endif

   infoI1 = infoLen = 0 ;

   if ( error4code( p4->codeBase ) < 0 )
      return e4codeBase ;

   /* get the function number */
   functionNumber = expr4parseLookup( p4, functionName, functionNameLen, &calc, &newOrTotalPtr ) ;
   if ( functionNumber < 0 )
      return functionNumber ;

   s4stackPushInt( &p4->op, E4L_BRACKET ) ;
   p4->scan.pos++ ;

   /* parse the function's paramaters before the function itself since the function paramaters
      may be required as part of the parsing (eg. constant character or numeric values)
   */
   numParms = expr4parseParamaters( p4 ) ;
   if ( numParms < 0 ) /* error */
      return numParms ;

   s4stackPop( &p4->op ) ;  /* pop the left bracket */

   // AS July 25/02 new function
   if ( functionNumber == E4SPACE )
      expr4parseSpaceFunction( p4, &infoLen ) ;

   /*
      AS Dec 12/03 - fixed function - 3rd paramater is the number of digits not a filler character
      parses the same as expr4parseStr
   */
   if ( functionNumber == E4STR || functionNumber == E4STRZERO )
      numParms = expr4parseStrFunction( p4, &functionNumber, numParms, &infoI1, &infoLen ) ;

//   if ( functionNumber == E4STRZERO )
//      numParms = expr4parseStrZeroFunction( p4, &functionNumber, numParms, &infoI1, &infoLen ) ;

   if ( numParms == 2  &&  functionNumber == E4RIGHT )
      numParms = expr4parseRightFunction( p4, numParms, &infoI1, &infoLen ) ;

   if ( numParms == 2 && ( functionNumber == E4PADL || functionNumber == E4PADR ) )
      numParms = expr4parsePadFunction( p4, numParms, &infoI1, &infoLen ) ;

   if ( numParms == 2  &&  functionNumber == E4SUBSTR )   /* case where no 3rd paramater on substr */
      numParms = expr4parseSubStr2parmFunction( p4, numParms, &infoI1, &infoLen ) ;

   if ( numParms == 3  &&  functionNumber == E4SUBSTR || numParms == 2  &&  functionNumber == E4LEFT )
      numParms = expr4parseSubStr3parmOrLeftFunction( p4, numParms, &infoLen ) ;

   if ( numParms == 2  &&  functionNumber == E4SUBSTR )
      numParms = expr4parseSubStr2parmPart2Function( p4, numParms, &infoI1 ) ;

   if ( numParms >= 3 && numParms <= 7 && functionNumber == E4DATETIME )
      numParms = expr4parseDateTimeParmFunction( p4, &functionNumber, numParms, &infoI1 ) ;

   if ( error4code( p4->codeBase ) < 0 || numParms < 0 )
      return -1 ;

   if ( numParms != v4functions[functionNumber].numParms && (int)v4functions[functionNumber].numParms >= 0 )
   {
      // AS Apr 22/04 - TTOC support
      if( ( ( functionNumber == E4DTOC ) || ( functionNumber == E4TTOC ) ) && numParms == 2 )
      {
         e4functionPop( &p4->expr ) ;
         numParms-- ;
         if ( functionNumber == E4DTOC )
            functionNumber = E4DTOS ;
      }
      else
      {
         if( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4numParms, E80906, v4functions[functionNumber].name, (char *)p4->scan.ptr, 0 ) ;
         return e4numParms ;
      }
   }

   info = e4functionAdd( &p4->expr, functionNumber ) ;
   if ( info == 0 )
      return -1 ;

   info->i1  = infoI1 ;
   info->len = infoLen ;

   info->numParms = numParms ;
   if ( functionNumber == E4CALC_FUNCTION || functionNumber == E4TOTAL )
      info->p1 = (char *)newOrTotalPtr ;
   return 0 ;
}



static int expr4parseValueRightBracket( E4PARSE *p4 )
{
   int rc ;

   p4->scan.pos++ ;

   s4stackPushInt( &p4->op, E4L_BRACKET) ;
   rc = expr4parseExpr( p4 ) ;
   if ( rc < 0 )
      return error4stack( c4, (short)rc, E90910 ) ;

   while ( s4scanChar( &p4->scan ) <= ' ' &&
      s4scanChar( &p4->scan ) != 0)   p4->scan.pos++ ;

   if ( s4scanChar( &p4->scan ) != ')' )
   {
      if ( p4->codeBase->errExpr )
         return error4describe( p4->codeBase, e4rightMissing, E90910, (char *)p4->scan.ptr, 0, 0 ) ;
      return e4rightMissing ;
   }
   p4->scan.pos++ ;
   s4stackPop( &p4->op ) ;
   return 0 ;
}



static int expr4parseValueLogical( E4PARSE *p4, int iFunctions )
{
   /*
      parses a logical value.

      returns 0 if success, < 0 if error
   */
   int rc ;


   p4->scan.pos += v4functions[iFunctions].nameLen ;

   //if ( strcmp( v4functions[iFunctions].name, ".NOT." ) == 0 )
   if ( iFunctions == E4NOT || iFunctions == E4NOT+1 )
   {
      rc = expr4parseValue( p4 ) ;   /* One operand operation */
      if ( rc < 0 )
         return error4stack( p4->codeBase, (short)rc, E90910 ) ;
      s4stackPushInt( &p4->op, iFunctions ) ;
      return 0 ;
   }

   if ( e4functionAdd( &p4->expr, iFunctions ) == 0 )
      return -1 ;
   return 0 ;
}



static int expr4parseValueString( E4PARSE *p4 )
{
   /*
      continues parsing given that the current position is the start of a string

      returns 0 if success
      returns < 0 in case of error
   */

   char searchChar ;
   const unsigned char *startPtr ;
   int len ;

   if ( s4scanChar( &p4->scan ) == '[' )
      searchChar = ']' ;
   else
      searchChar = s4scanChar( &p4->scan ) ;

   p4->scan.pos++ ;
   startPtr = p4->scan.ptr + p4->scan.pos ;

   len = s4scanSearch( &p4->scan, searchChar ) ;
   if ( s4scanChar( &p4->scan ) != searchChar )
   {
      // AS Mar 23/04 - It is actually an error in this case as well.  Having an unterminated string is incompatible with other products.
      // if ( len < 0 )
      {
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4unterminated, E90910, (char *)p4->scan.ptr, 0, 0 ) ;
         return e4unterminated ;
      }
   }

   p4->scan.pos++ ;

   if ( e4addConstant( p4, E4STRING, startPtr, (unsigned int)len ) < 0 )
      return -1 ;
   return 0 ;
}



static int expr4parseValueReal( E4PARSE *p4 )
{
   /*
      attempts to parse the current position as a real value

      returns
        0 if success
        r4database if in fact was not a real value, probably a database name starting with a
          number (eg. 12data->field )
   */

   const unsigned char *startPtr = p4->scan.ptr + p4->scan.pos ;
   int len ;
   int savePos = p4->scan.pos ;
   double d ;

   p4->scan.pos++ ;
   len = 1 ;

   while( ((s4scanChar( &p4->scan ) >= '0') && (s4scanChar( &p4->scan ) <= '9')) || (s4scanChar( &p4->scan ) == '.') )
   {
      if ( s4scanChar( &p4->scan ) == '.' )
      {
         if ( c4strnicmp( (char *)p4->scan.ptr + p4->scan.pos, ".AND.", 5) == 0 ||
              c4strnicmp( (char *)p4->scan.ptr + p4->scan.pos, ".OR.", 4) == 0 ||
              c4strnicmp( (char *)p4->scan.ptr + p4->scan.pos, ".NOT.", 5) == 0 )
            break ;
         /* if the next value is a character, then we have a database
            with a number as its name/alias.  (i.e. 111.afld), since
            numerics are invalid to being a field name, MUST be a
            number if a numeric after the decimal point... */
         /* AS 03/03/97 fix to numerics, fix #70 in changes.60 */
         p4->scan.pos++ ;
         if ( c4toupper( s4scanChar( &p4->scan ) ) >= 'A' && c4toupper( s4scanChar( &p4->scan ) ) <= 'Z' )
            break ;
         else
            p4->scan.pos-- ;   /* retract ++ which was just done to see next character */
      }
      len++ ;
      p4->scan.pos++ ;
   }

   /* check to see if maybe actually a database name starting with a numeric... */
   if ( c4toupper( s4scanChar( &p4->scan ) ) >= 'A' && c4toupper( s4scanChar( &p4->scan ) ) <= 'Z' )
   {
      p4->scan.pos = savePos ;
      return r4database ;
   }

   d = c4atod( (char *)startPtr, len ) ;
   if ( e4addConstant( p4, E4DOUBLE, &d, sizeof( d ) ) < 0 )
      return -1 ;
   return 0 ;
}



static DATA4 *getData4fromName( E4PARSE *p4, const char *name, int len )
{
   DATA4 *basePtr ;
   char bName[LEN4DATA_ALIAS+1] ;

   if ( len > LEN4DATA_ALIAS )
      len = LEN4DATA_ALIAS ;

   c4memmove( bName, name, (size_t)len ) ;
   bName[len] = '\0' ;

   basePtr = tran4dataName( code4trans( p4->codeBase ), bName, 0L, 1 ) ;
   if ( basePtr == 0 )
   {
      if ( p4->codeBase->errExpr )
         error4describe( p4->codeBase, e4dataName, E90910, name, (char *)p4->scan.ptr, (char *) 0 ) ;
      return 0 ;
   }

   return basePtr ;
}



static int getV4functionsIndexFromFieldType( CODE4 *c4, int fieldType )
{
   /* simple mapping from the input field type to the v4functions[] index value which
      means a simple field type
   */
   switch( fieldType )
   {
      case r4num:
      case r4float:
         return E4FIELD_NUM_S ;
      case r5wstr:
         return E4FIELD_WSTR ;
      case r5wstrLen:
         return E4FIELD_WSTR_LEN ;
      case r4str:
         return E4FIELD_STR ;
      case r4date:
         return E4FIELD_DATE_S ;
      case r4log:
         return E4FIELD_LOG ;
      case r4memo:
      #ifdef S4FOX
         case r4gen:
      #endif
      #ifdef S4MDX
         case r4bin:
      #endif
         #ifdef S4MEMO_OFF
            return error4( c4, e4notMemo, E90910 ) ;
         #else
            return E4FIELD_MEMO ;
         #endif
      /* visual Fox 3.0 new field types - also used for ole-db */
      case r4currency:
         return E4FIELD_CUR ;
      case r4dateTime:
         return E4FIELD_DTTIME ;
      case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetime
         return E4FIELD_DTTIME_MILLI ;
      #ifndef S4MDX
         // same as 'binary' type in mdx, don't use here...
         case r4double:
            return E4FIELD_DOUB ;
      #endif
      // AS Jul 20/05 - Support for new field type binary float
      case r4floatBin:
         return E4FIELD_BINFLOAT ;
      case r4int:
         return E4FIELD_INT ;
      #ifdef S5USE_EXTENDED_TYPES
         case r5i8:
            return E4FIELD_I8 ;
         case r5dbDate:
            return E4FIELD_DBDATE ;
         case r5dbTime:
            return E4FIELD_DBTIME ;
         case r5dbTimeStamp:
            return E4FIELD_DBTIMESTAMP ;
         case r5i2:
            return E4FIELD_SHORT ;
         case r5ui2:
            return E4FIELD_UNS_SHORT ;
         case r5ui4:
            return E4FIELD_UNS_INT ;
      #endif
      default:
         if ( c4->errExpr )
            return error4( c4, e4typeSub, E80901 ) ;
         return -1 ;
   }
}



static int expr4parseField( E4PARSE *p4, const char *inputName, int len )
{
   /* first we need to see if we have a base table prefix to the field name (eg. D1->field) */

   const unsigned char *startPtr = (const unsigned char *)inputName ;
   DATA4 *basePtr = 0 ;

   #ifdef S4FOX
      /* for fox, '.' is the same as -> - means a data table name */
      if ( s4scanChar( &p4->scan ) == '.' )
      {
         // AS 02/25/00 problem with expression ".NOT.log1.AND.log2", is looking for
         // "log1.AND" as a database.  Resolve this by if the basePtr is NULL, then
         // just continue on and assume that it is not a pointer...
         int oldErrCode = p4->codeBase->errExpr ;
         p4->codeBase->errExpr = 0 ;
         basePtr = getData4fromName( p4, inputName, len ) ;
         p4->codeBase->errExpr = oldErrCode ;
      }
   #endif

   if ( s4scanChar( &p4->scan ) == '-' )
   {
      if ( p4->scan.ptr[p4->scan.pos+1] == '>')
      {
         /* means a data table name */
         basePtr = getData4fromName( p4, inputName, len ) ;
         p4->scan.pos++ ;
      }
   }

   CODE4 *c4 = p4->codeBase ;

   if ( basePtr != 0 )
   {
      if ( p4->expr.tagPtr )  /* data4 independent, so point to datafile */
         if ( basePtr != (DATA4 *)p4->expr.data )   /* allow if it points to itself */
            return error4( c4, e4tagExpr, E80909 ) ;

      p4->scan.pos++ ;

      startPtr = p4->scan.ptr + p4->scan.pos ;
      for ( len = 0 ; u4nameChar( s4scanChar( &p4->scan ) ) ; len++ )
      {
         p4->scan.pos++ ;
      }
   }
   else
   {
      /* a base DATA4 was not included, assume the current DATA4 for the expression */
      basePtr = (DATA4 *)p4->expr.data ;
   }

   // AS Oct 27/03 - long field names support
   if ( len > 10 )
   {
      // AS Nov 24/03 - Long field name support only available in fox
      #ifdef S4CLIENT_OR_FOX
         if ( basePtr->dataFile->longFieldNamesSupported == 0 )
      #endif
      {
         /* a field name > 10 characters was given.  This is invalid. */
         if ( p4->codeBase->errExpr )
            return error4describe( p4->codeBase, e4unrecValue, E90910, (char *)p4->scan.ptr, 0, 0 ) ;
         else
            len = 0 ;  /* LY 2002/04/18 : avoid overwriting memory in c4memmove */
      }
   }

   char fieldName[11], *fieldNamePtr ;
   #ifdef S4CLIENT_OR_FOX
      char *longFieldName ;
      if ( basePtr->dataFile->longFieldNamesSupported == 0 )
   #endif
         fieldNamePtr = fieldName ;
   #ifdef S4CLIENT_OR_FOX
      else
      {
         longFieldName = (char *)u4alloc( len+1 ) ;
         if ( longFieldName == 0 )
            return error4( c4, e4fieldName, E90910 ) ;
         fieldNamePtr = longFieldName ;
      }
   #endif
   c4memmove( fieldNamePtr, startPtr, (size_t)len ) ;
   fieldNamePtr[len] = 0 ;
   FIELD4 *fieldPtr = 0 ;

   if ( !c4->errExpr )
   {
      int tempErrFieldName = c4->errFieldName;
      c4->errFieldName = 0;
      fieldPtr = d4field( basePtr, fieldNamePtr ) ;
      c4->errFieldName = tempErrFieldName;
   }
   else
      fieldPtr = d4field( basePtr, fieldNamePtr ) ;

   #ifdef S4CLIENT_OR_FOX
      if ( basePtr->dataFile->longFieldNamesSupported != 0 )
         u4free( longFieldName ) ;
   #endif

   if ( fieldPtr == 0 )
   {
      if ( c4->errExpr == 1 && c4->errFieldName == 0 )  /* generate an error since it wasn't by d4field */
         return error4( c4, e4fieldName, E90910 ) ;
      return -1 ;
   }

   #ifdef S4CLIPPER
      p4->expr.keyLen = fieldPtr->len ;
      p4->expr.keyDec = fieldPtr->dec ;
   #endif

   int iFunction = getV4functionsIndexFromFieldType( c4, fieldPtr->type ) ;
   if ( iFunction < 0 )
      return iFunction ;

   E4INFO *info = e4functionAdd( &p4->expr, iFunction ) ;
   if ( info == 0 )
      return -1 ;
   info->fieldNo = f4number( fieldPtr ) ;
   info->fieldPtr = fieldPtr ;

   if ( fieldPtr->data == (DATA4 *)p4->expr.data )  /* data is local */
      info->localData = 1 ;

   #ifndef S4CLIENT
      if ( p4->expr.tagPtr )
         info->p1 = (char *)&basePtr->dataFile->record ;
      else
   #endif
         info->p1 = (char *)&basePtr->record ;

   info->i1 = fieldPtr->offset ;

   return 0 ;
}



static int expr4parseValueFunctionOrField( E4PARSE *p4 )
{
   CODE4 *c4 = p4->codeBase ;
   DATA4 *basePtr = 0 ;
   int len ;

   const unsigned char *startPtr = p4->scan.ptr + p4->scan.pos ;

   for( len = 0 ; u4nameChar( s4scanChar( &p4->scan ) ) ; len++ )
      p4->scan.pos++ ;

   // AS 10/06/99 --> expression 'L .AND. M' L logical not working because space was removed here.
   // recover from this if no bracket...
   int savePos = p4->scan.pos ;
   s4scanRange( &p4->scan, (char)0, ' ' ) ;

   if ( s4scanChar( &p4->scan ) == '(' )
      return expr4parseFunction( p4, (char *)startPtr, len ) ;

   // AS 10/06/99 --> recover from this if no bracket...
   p4->scan.pos = savePos ;
   return expr4parseField( p4, (char *)startPtr, len ) ;
}



int expr4parseValue( E4PARSE *p4 )
{
   char ch ;
   int iFunctions, rc ;

   #ifdef E4PARM_LOW
      if ( p4 == 0 )
         return error4( 0, e4parm_null, E90910 ) ;
   #endif

   if ( error4code( p4->codeBase ) < 0 )
      return e4codeBase ;

   s4scanRange( &p4->scan, ' ', ' ' ) ;

   ch = s4scanChar( &p4->scan ) ;

   switch ( ch )
   {
      case '(':
         /* expression */
         return expr4parseValueRightBracket( p4 ) ;
      case '.':
      case '!':   // AS 07/07/00 - added support for !...
         /* logical */
         iFunctions = e4lookupInFunctionArray( p4->scan.ptr + p4->scan.pos, -1, E4FIRST_LOG, E4LAST_LOG ) ;
         if ( iFunctions >= 0 )
            return expr4parseValueLogical( p4, iFunctions ) ;
         /* otherwise, is actually not a logical, break out of loop and continue search (eg.
            coule be a real ".123" */
         break ;
      case '\'':
      case '\"':
      case '[':
         /* string */
         return expr4parseValueString( p4 ) ;
      default:  /* remaining cases too complex, analyze below */
         break ;
   }

   if ( ((ch >='0') && (ch <='9')) || (ch == '-') || (ch == '+') || (ch == '.') ) /* real */
   {
      rc = expr4parseValueReal( p4 ) ;
      if ( rc != r4database )
         return rc ;
      /* r4database value means that it was not actually a real, so continue with evaluation
         but taking the value as a name */
   }

   if ( u4nameChar( s4scanChar( &p4->scan ) ) )  /* function or field */
      return expr4parseValueFunctionOrField( p4 ) ;

   if ( p4->codeBase->errExpr )
      return error4describe( p4->codeBase, e4unrecValue, E90910, (char *)p4->scan.ptr, 0, 0 ) ;

   return e4unrecValue ;
}



static int s4stackPop( S4STACK *s4 )
{
   int retValue ;

   retValue = s4stackCur(s4) ;

   if ( s4->pos >= sizeof(int) )
      s4->pos -= sizeof(int) ;
   return retValue ;
}



static int s4stackCur( S4STACK *s4 )
{
   int pos, curData ;

   if ( s4->pos < sizeof(int) )
      return E4NO_FUNCTION ;
   pos = s4->pos - sizeof(int) ;
   c4memcpy( (void *)&curData, s4->ptr+pos, sizeof(int) ) ;
   return curData ;
}



static int s4stackPushInt( S4STACK *s4, const int i )
{
   return s4stackPushStr( s4, &i, sizeof(i)) ;
}



static int s4stackPushStr( S4STACK *s4, const void *p, const int len )
{
   char *oldPtr ;
   CODE4 *c4 ;

   c4 = s4->codeBase ;

   if ( error4code( c4 ) < 0 )
      return -1 ;

   if ( s4->pos+len > s4->len )
   {
      oldPtr = s4->ptr ;
      if ( s4->doExtend == EXTEND4OFF )  // extending constant buffer not available...
         s4->ptr = 0 ;
      else
         s4->ptr = (char *)u4allocFree( c4, (long)s4->len + 256L ) ;
      if ( s4->ptr == 0 )
      {
         s4->ptr = oldPtr ;
         if ( c4->errExpr )
            return error4( c4, e4memory, E90911 ) ;
         return e4memory ;
      }
      c4memcpy( s4->ptr, oldPtr, s4->len ) ;
      if ( s4->doExtend == EXTEND4ALLOCATED )  // means memory previously allocated, so free now...
         u4free( oldPtr ) ;
      s4->doExtend = EXTEND4ALLOCATED ;  // notify that we have allocated the memory...
      s4->len += 256 ;

      return s4stackPushStr( s4, p, len ) ;
   }
   else
   {
      c4memcpy( s4->ptr+s4->pos, p, (unsigned int)len ) ;
      s4->pos += len ;
   }
   return 0 ;
}



static unsigned char s4scanChar( S4SCAN *s4 )
{
   if ( s4->pos >= s4->len )
      return 0 ;
   return s4->ptr[s4->pos] ;
}



static void s4scanInit( S4SCAN *s4, const unsigned char *p )
{
   s4->ptr = p ;
   s4->pos = 0 ;
   s4->len = c4strlen( (char *)p ) ;
}



static int s4scanRange( S4SCAN *s4, const int startChar, const int endChar )
{
   int count ;

   for ( count = 0; s4->pos < s4->len; s4->pos++, count++ )
      if ( s4->ptr[s4->pos] < startChar || s4->ptr[s4->pos] > endChar )
         return count ;
   return count ;
}



static int s4scanSearch( S4SCAN *s4, const char searchChar )
{
   int count ;

   for ( count = 0; s4->pos < s4->len; s4->pos++, count++ )
      if ( s4->ptr[s4->pos] == searchChar )
         return count ;
   return count ;
}



#ifdef S4VB_DOS
   EXPR4 *expr4parse_v( DATA4 *d4, char *expr )
   {
      return expr4parseLow( d4, c4str(expr), 0 ) ;
   }
#endif
