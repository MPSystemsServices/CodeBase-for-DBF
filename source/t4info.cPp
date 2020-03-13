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

/* t4info.cpp/cxx (c)Copyright Sequiter Software Inc., 1988-1998.  All rights reserved. */

#include "d4all.hpp"
#ifdef __TURBOC__
   #pragma hdrstop
#endif  /* __TUROBC__ */

Data4 Expr4::data()const
{
   DEBUG4PTR( expr == 0, E60602 )
   #ifdef D4DLL_CPP
      return Data4( expr->d4 ) ;
   #else
      return Data4( expr->data ) ;
   #endif
}

Tag4info::Tag4info( Code4 &code )
{
   size = 0 ;
   codeBase = &code ;
   length = 0 ;
   tag = NULL ;
   expr=filt=0;

}

Tag4info::Tag4info( Index4 i )
{
   DEBUG4VOID(i.index == 0, E60983)
   size = 0 ;
   #ifdef D4DLL_CPP
      codeBase = (Code4 *) i.index->d4->c4 ;
   #else
      codeBase = (Code4 *) i.index->data->codeBase ;
   #endif
   length = 0 ;
   tag = NULL ;
   expr=filt=0;
   addIndex_tags( i.index ) ;
}

Tag4info::Tag4info( Data4 d )
{
   DEBUG4VOID(d.data == 0, E60983)
   #ifdef D4DLL_CPP
      INDEX4 *index = (INDEX4*) l4next( d4indexList( d.data ), NULL ) ;
   #else
      INDEX4 *index = (INDEX4*) l4next( &(d.data->indexes), NULL ) ;
   #endif

   size = 0 ;
   #ifdef D4DLL_CPP
      codeBase = (Code4 *) d.data->c4 ;
   #else
      codeBase = (Code4 *) d.data->codeBase ;
   #endif
   length = 0 ;
   tag = NULL ;
   expr=filt=0;

   #ifdef D4DLL_CPP
      for(; index != NULL ; index = (INDEX4*) l4next( d4indexList( d.data ), index ) )
   #else
      for(; index != NULL ; index = (INDEX4*) l4next( &(d.data->indexes), index ) )
   #endif
       addIndex_tags( index ) ;
}

int Tag4info::addIndex_tags( INDEX4 *index )
{
   TAG4INFO *tagInfo ;
   int i = 0 ;

   tagInfo = i4tagInfo( index ) ;
   DEBUG4INT(tagInfo == 0, E60984)
   while( tagInfo[i].name != 0 )
   {
      add( getName( tagInfo, i ), getExpr( tagInfo, i ), getFilter( tagInfo, i ),
           getUniqueKey( tagInfo, i ), getDescendKey( tagInfo, i ) ) ;
      i++ ;
   }
   u4free(tagInfo);
   return 0 ;
}

int Tag4info::add( Tag4 tagIn )
{
   TAG4INFO *tagInfo ;
   int i = 0 ;
   char *name ;

   DEBUG4INT(tagIn.tag == 0, E60991)
   #ifdef D4DLL_CPP
      tagInfo = t4tagInfo( tagIn.tag ) ;
   #else
      tagInfo = i4tagInfo( tagIn.tag->index ) ;
   #endif
   DEBUG4INT(tagInfo == 0, E60991)
   name = t4alias( tagIn.tag ) ;
   while( strcmp( tagInfo[i].name, name ) != 0 ) i++ ;

   add( getName( tagInfo, i ), getExpr( tagInfo, i ), getFilter( tagInfo, i ),
        getUniqueKey( tagInfo, i ), getDescendKey( tagInfo, i ) ) ;

   return 0 ;
}

int Tag4info::add( const char *name, const char *expre, const char *filter,
                          short uniq, unsigned short desc )
{
   DEBUG4INT(codeBase == 0, E60991)
   /* LY 99/07/08 : replaced Str4ten stuff */
   char st_name[11] ;
   size_t blankLen ;
   memset( st_name, 0, 11 ) ;
   #ifdef S4WINCE
      blankLen = c4strspn( name, " " ) ;
   #else
      blankLen = strspn( name, " " ) ;
   #endif
   /* LY Oct 8/04 : should be <= 10 */
   /* LY Feb 27/04 : replaced strlen(name) with 10 */
   memcpy( st_name, name, (blankLen > 0? blankLen-1: (strlen( name ) > 10 ? 10 : strlen( name )) ) ) ;
   c4upper( st_name ) ;
   #ifdef E4DEBUG
      if( name == 0 || expre == 0 )
         return codeBase->error( e4parm, E60991 ) ;
   #endif

   if( u4allocAgain( codeBase, (char **)&tag, &length,
                                 (size+2) * sizeof( TAG4INFO ))!=0 )
      return -1 ;

   tag[size].name = (char *)u4allocEr( codeBase, 11 ) ;
   if( tag[size].name )
      u4ncpy( tag[size].name, st_name, 11 ) ;

   unsigned len = strlen(expre) + 1 ;
   //tag[size].expression = (char *)u4allocEr( codeBase, len ) ;
   expr= (char *)u4allocEr( codeBase, len ) ;

   if( expr != NULL )
      u4ncpy( expr, expre, len ) ;
   tag[size].expression=expr;

   //if( tag[size].expression != NULL )
   //  u4ncpy( tag[size].expression, expre, len ) ;

   if( filter )
   {
      filt = (char *)u4allocEr( codeBase, len = strlen( filter )+1 ) ;
      //tag[size].filter = (char *)u4allocEr( codeBase, len = strlen( filter )+1 ) ;
      if( filt != NULL )
      //if( tag[size].filter != NULL )
         u4ncpy( filt, filter, len ) ;
         //u4ncpy( tag[size].filter, filter, len ) ;
      tag[size].filter=filt;
   }

   #ifdef D4DLL_CPP
      if( code4errorCode( codeBase, -5 ) < 0 )
   #else
      if( codeBase->errorCode < 0 )
   #endif
   {
      u4free( tag[size].name ) ;
      u4free( expr ) ;
      //u4free( tag[size].expression ) ;
      u4free( filt ) ;
      //u4free( tag[size].filter ) ;
      memset( tag+size, 0, sizeof(TAG4INFO) ) ;
      return -1 ;
   }

   tag[size].unique = uniq ;
   tag[size].descending = desc ;
   size++ ;
   return 0 ;
}

char *Tag4info::getName( TAG4INFO *tagInfo, int tagPos )
{
   DEBUG4PTR(tagInfo == 0, E60985)
   if( tagInfo[tagPos].name != 0 )
      return tagInfo[tagPos].name ;
   else
      return 0 ;
}

const char *Tag4info::getExpr( TAG4INFO *tagInfo, int tagPos )
{
   DEBUG4PTR(tagInfo == 0, E60986)
   if( tagInfo[tagPos].name != 0 )
      return tagInfo[tagPos].expression ;
   else
      return 0 ;
}

const char *Tag4info::getFilter( TAG4INFO *tagInfo, int tagPos )
{
   DEBUG4PTR(tagInfo == 0, E60987)
   if( tagInfo[tagPos].name != 0 )
      return tagInfo[tagPos].filter ;
   else
      return 0 ;
}

short Tag4info::getUniqueKey( TAG4INFO *tagInfo, int tagPos )
{
   #ifdef E4PARM_HIGH
      if (tagInfo == 0)
         return (short)error4( 0, e4parmNull, E60988 );
   #endif

   if( tagInfo[tagPos].name != 0 )
      return tagInfo[tagPos].unique ;
   else
      return 0 ;
}

unsigned short Tag4info::getDescendKey( TAG4INFO *tagInfo, int tagPos )
{
   #ifdef E4PARM_HIGH
      if (tagInfo == 0)
         return (short)error4( 0, e4parmNull, E60989 );
   #endif

   if( tagInfo[tagPos].name != 0 )
      return tagInfo[tagPos].descending ;
   else
      return 0 ;
}

void Tag4info::free( )
{
   for( ; size > 0 ; )
   {
      size -- ;
      u4free( tag[size].name ) ;
      expr =(char *)tag[size].expression;
      u4free( expr ) ;
      tag[size].expression=0;
      filt =(char *)tag[size].filter;
      u4free( filt ) ;
      tag[size].filter=0;
   }
   u4free( tag ) ;
   tag = 0 ;
   size = 0 ;
   length = 0 ;
}

Tag4info::~Tag4info()
{
   free( ) ;
}

int Tag4info::del( int index )
{
   DEBUG4INT(codeBase == 0, E60982)
   if( index >= size || index < 0 )
      return codeBase->error( e4parm, E60982 ) ;

   //CJ - 05/11/99 - fix to make sure the expr and filter are poinitin to
   // the soon to be deleted TAG4INFO.
   u4free( tag[index].name ) ;
   expr = (char *)tag[index].expression;
   tag[index].expression = 0 ;
   u4free( expr ) ;

   filt = (char *)tag[index].filter ;
   tag[index].filter=0;
   u4free( filt ) ;

   memcpy( tag+index, tag+index+1, sizeof(TAG4INFO) * (size-index) ) ;
   size-- ;
   return 0 ;
}

#ifndef S4JNI  /* LY 99/07/08 */
int Tag4info::del( const char *name )
{
   DEBUG4INT(codeBase == 0, E60982)
   Str4ten st_tag( name ) ;
   st_tag.upper( ) ;
   st_tag.trim( ) ;
   for( int i = 0 ; i < size ; i++ )
   {
      if( memcmp( tag[i].name, st_tag.ptr( ), st_tag.len() ) == 0 )
      {
          del( i ) ;
          return 0 ;
      }
   }
   codeBase->error( e4parm, E60982, name ) ;
   return -1 ;
}
#endif

