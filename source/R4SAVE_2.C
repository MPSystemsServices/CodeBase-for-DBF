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

/* r4save_2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_REPORT

#ifdef S4CR2  /* required for building crep2.exe*/
   #define S4CONV_REP
#endif

extern int file_version;
extern LIST4 name_list;
extern char *lookahead_buf;
extern int lookedahead_calc;

int save4long( FILE4SEQ_WRITE *seq, long *lval );
int save4short( FILE4SEQ_WRITE *seq, short *sval );
int ret4long( FILE4SEQ_READ *seq, long *lval );
int ret4short( FILE4SEQ_READ *seq, short *sval );

/************************************************************
 *
 * Function:
 *
 *  PARAMETERS:
 *    DATA4* data - data file
 *    char* index_name - name of the index file to be checked for
 *    char* index_lookup - buffer used in the function
 *    char* current - buffer used in the function
 *
 *  DESCRIPTION:this function performs the same operation as d4index(), but
 *   checks for an index file both with and without path names
 *
 *  RETURNS: 1 - on finding a matching index file, 0 - on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */

/* this function performs the same operation as d4index(), but checks for an
   index file both with and without path names
 */
int S4FUNCTION r4index_lookup_foo( DATA4 *data, char *index_name,
                                   char *index_lookup, char *current )
{
   INDEX4 *index_on ;
   int ttype;


   if ( data == 0 || index_name == 0 )
      #ifdef S4DEBUG
         e4severe( e4parm, E4_D4INDEX ) ;
      #else
         return 0 ;
      #endif

   #ifndef S4CLIENT
      ttype = report4index_type();
   #else
      ttype = code4indexFormat( data->codeBase ) ;
   #endif
   u4namePiece( index_lookup, 257, index_name, 1, 0 ) ;
   #ifndef S4CASE_SEN
      c4upper( index_lookup ) ;
   #endif

   if( r4ntx == ttype )
   {
      if( d4tag( data, index_lookup ) )
         return 1;
      u4namePiece( index_lookup, 257, index_name, 0, 0 ) ;
      if( d4tag( data, index_lookup ) )
         return 1;
   }
   else
   {
      index_on = (INDEX4 *)l4first( &data->indexes );
      while( index_on )
      {
         #ifndef S4CLIENT
            #ifdef N4OTHER
               u4namePiece( current, 257, index_on->file.name, 1, 0 ) ;
            #else
               u4namePiece( current, 257, index_on->indexFile->file.name, 1, 0 ) ;
            #endif
         #else
            u4namePiece( current, 257, i4fileName( index_on ), 1, 0 ) ;
         #endif
         #ifndef S4CASE_SEN
            c4upper( current ) ;
         #endif
         if ( !strcmp( current, index_lookup ) )    /* check out data->alias? */
            return 1;
         index_on = (INDEX4 *) l4next( &data->indexes, index_on) ;
      }

      u4namePiece( index_lookup, 257, index_name, 0, 0 ) ;
      #ifndef S4CASE_SEN
         c4upper( index_lookup ) ;
      #endif

      index_on = (INDEX4 *)l4first( &data->indexes );
      while( index_on )
      {
         #ifndef S4CLIENT
            #ifdef N4OTHER
               u4namePiece( current, 257, index_on->file.name, 0, 0 ) ;
            #else
               u4namePiece( current, 257, index_on->indexFile->file.name, 0, 0 ) ;
            #endif
         #else
            u4namePiece( current, 257, i4fileName( index_on ), 0, 0 ) ;
         #endif
         #ifndef S4CASE_SEN
            c4upper( current ) ;
         #endif
         if ( !strcmp( current, index_lookup ) )    /* check out data->alias? */
            return 1;
         index_on = (INDEX4 *) l4next( &data->indexes, index_on) ;
      }

   }

   return( 0 );
}

/* wrapper for above function */
int S4FUNCTION r4index_lookup( DATA4 *data, char *index_name )
{
   char *index_lookup, *current;
   int retvalue;

   index_lookup = (char *)u4allocFree( data->codeBase, 258 );
   if( !index_lookup )
      return 0;

   current = (char*)u4allocFree( data->codeBase, 258 );
   if( !current )
   {
      u4free( index_lookup );
      return 0;
   }

   retvalue = r4index_lookup_foo( data, index_name, index_lookup, current );

   u4free( index_lookup );
   u4free( current );
   return retvalue;
}

/************************************************************
 *
 * Function: r4open_data_foo()
 *
 *  PARAMETERS:
 *    char* file_name - full name of the file, potentially including path
 *    char* alias - alias to set for the data file
 *    RELATE4* relate - the relate associated with the current report
 *    CODE4* codeBase - CODE4 structure for the app
 *    char* fnbuf - a buffer used by the function
 *
 *  DESCRIPTION: opens a data file, if the file is already open in the relate
 *   appropriate aliasing is performed to open the file a second time
 *
 *  RETURNS: a DATA4 pointer on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
// AS Dec 13/05 - improvements for function deprecation
#define FNBUF_LEN 256
static DATA4 *r4open_data_foo( char *file_name, char *alias, RELATE4 *relate, CODE4 *codeBase, char *fnbuf )
{
   DATA4   *old, *nnew;
   RELATE4 *relate_on;
   char    abuf[LEN4DATA_ALIAS + 1];

   if( !file_name || !alias )
      return NULL;

   nnew = old = NULL;
   /* if no alias or no relate do a simple d4open() */
   if( alias[0] == '\0' || !relate )
   {
      nnew = d4open( codeBase, file_name );
      return nnew;
   }

   #ifndef S4CASE_SEN
      c4upper( file_name );
      c4upper( alias );
   #endif

   /* check to see if the file is already open */
   relate_on = relate;
   while( relate_on )
   {
      // AS Dec 13/05 - under Windows strcpy is becoming deprecated...
      #ifndef S4CLIENT
         c4strcpy( fnbuf, FNBUF_LEN, relate_on->data->dataFile->file.name );
      #else
         c4strcpy( fnbuf, FNBUF_LEN, d4fileName( relate_on->data ) ) ;
      #endif
      #ifndef S4CASE_SEN
         c4upper( fnbuf );
      #endif
      // AS Dec 13/05 - improvements for function deprecation
      c4strcpy( abuf, sizeof( abuf ), d4alias( relate_on->data ) );
      #ifndef S4CASE_SEN
         c4upper( abuf );
      #endif
      if( strcmp( fnbuf, file_name ) == 0 && strcmp(abuf, alias) == 0 )
         old = relate_on->data;

      relate4next( &relate_on );
   }

   /* if the file is not already open do a d4open() and set the alias */
   if( old == NULL )
   {
      nnew = d4open( codeBase, file_name );
      if( nnew && alias[0] != '\0' )
         d4aliasSet( nnew, alias );
      return nnew;
   }

   /* save the existing files alias, reset it to xxxxxxx open the new file,
      set the new files alias, then restore the old files alias  */
   // AS Dec 13/05 - under Windows strcpy is becoming deprecated...
   c4strcpy( abuf, sizeof( abuf ), d4alias(old) );
   d4aliasSet( old, "XXXXXXXXXX" );
   nnew = d4open( codeBase, file_name );
   if( nnew )
      d4aliasSet( nnew, alias );
   d4aliasSet( old, abuf );

   return nnew;
}

/* wrapper for above function */
DATA4 *r4open_data( char *file_name, char *alias, RELATE4 *relate, CODE4 *codeBase )
{
   DATA4 *retvalue;
   char *fnbuf;

   // AS Dec 13/05 - improvements for function deprecation
   fnbuf = (char *)u4allocFree( codeBase, FNBUF_LEN );
   if( !fnbuf )
      return NULL;

   retvalue = r4open_data_foo( file_name, alias, relate, codeBase, fnbuf );

   u4free( fnbuf );
   return retvalue;
}


/* frees the list of N4CHANGE structures */
void report4free_name_list()
{
   PN4CHANGE nchange;

   while( (nchange = (PN4CHANGE)l4pop( &name_list )) != NULL )
   {
      if( nchange->old_name )
         u4free( nchange->old_name );

      if( nchange->new_name )
         u4free( nchange->new_name );

      u4free( nchange );
   }
}

/* simple memcompare function that first capitalizes the buffers being compared */
int r4memicmp_foo( char *str1, char *str2, int len, char *buffer )
{
   memset( buffer, 0, len + 1 );
   memcpy( buffer, str1, len );
   #ifndef S4CASE_SEN
      c4upper( buffer );
      c4upper( str2 );
   #endif
   return c4memcmp( str1, str2, len );
}

/* wrapper for above function */
int r4memicmp( char *str1, char *str2, int len )
{
   char *buffer;
   int retvalue;

   buffer = (char *)u4alloc( len+2 );
   if( !buffer )
      return 1;

   retvalue = r4memicmp_foo( str1, str2, len, buffer );
   u4free( buffer );
   return retvalue;
}

/************************************************************
 *
 * Function: report4nchange()
 *
 *  PARAMETERS:
 *    CODE4* c4 - apps code4 struct
 *    char** psrc - pointer to a pointer to the source string
 *    int can_alloc - flag specifying whether or not to allocate additional
 *     memory as needed
 *    int s_size - size of the source string
 *
 *  DESCRIPTION: in CR when opening a report file, if a data or index file
 *   cannot be found the user is prompted to enter a new path and/or name.
 *   If the name of a data file has changed the name must also be changed in
 *   all the expressions within the report.  To do this a list of N4CHANGE
 *   structures is maintained.  N4CHANGE simply contains the old and new names
 *   for a data file.  This function is passed an expression source and is
 *   responsible for swapping the names.
 *
 *  RETURNS: none
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
void report4nchange( CODE4 *c4, char **psrc, int can_alloc, int s_size )
{
   char      *orig = NULL, *src = *psrc;
   PN4CHANGE nchange;
   int       pos, src_pos, orig_len;
   int       old_name_len, new_name_len;
   unsigned  src_size = s_size;

   if( strlen(src) == 0 )
      return;

   /* for ever N4CHANGE struct in the name list */
   nchange = (PN4CHANGE)l4first( &name_list );
   while( nchange )
   {
      /* if no names continue */
      if( !nchange->new_name || !nchange->old_name )
         continue;

      old_name_len = strlen(nchange->old_name);

      /* create a buffer to duplicate the expression source */
      int origLen = strlen(src) + 1 ;
      orig = (char *)u4allocFree( c4, origLen );
      if( !orig )
         return;

      /* copy the expression source */
      src_pos = 0;
      // AS Dec 13/05 - improvements for function deprecation
      c4strcpy( orig, origLen, src );
      orig_len = strlen( orig );

      /* run through the copy of the expression source */
      for( pos = 0; orig[pos]; pos++ )
      {
         /* we are copying the duplicated expression source back into the
            original buffer, with appropriate insertions */
         src[src_pos++] = orig[pos];

         /* if the expression source buffer is too small due to changes
            do a re-allocation */
         if( src_pos == (int)src_size )
         {
            if( !can_alloc )
            {
               u4free( orig );
               return;
            }
            u4allocAgain( c4, &src, &src_size, src_size+50 );
            if( !src )
               return;
         }

         /* if we're at the end of the expression source, continue */
         if( (orig_len - pos) < old_name_len )
            continue;

         /* is this piece of the expression the current old name */
         if( r4memicmp( orig+pos, nchange->old_name, old_name_len ) != 0 )
            continue;

         if( u4nameChar( orig[pos+old_name_len] ) )
            continue;

         if( pos > 0 )
            if( u4nameChar( orig[pos-1]) )
               continue;

         /* insert the new name into the expression source, re-allocating space
            if necessary */
         new_name_len = strlen( nchange->new_name );
         if( (int)src_size <= (pos+new_name_len) )
         {
            if( !can_alloc )
            {
               u4free( orig );
               return;
            }

            u4allocAgain( c4, &src, &src_size, src_size + 50 );
            if( !src )
            {
               u4free( orig );
               return;
            }
         }
         memcpy( src+(--src_pos), nchange->new_name, new_name_len );
         src_pos += new_name_len;
         pos += (old_name_len - 1);
      } /* end of loop through copy of expression source */

      /* end the string */
      src[src_pos++] = '\0';

      /* next change name */
      nchange = (PN4CHANGE)l4next( &name_list, nchange );
      /* free the source duplicate buffer */
      if( orig )
      {
         u4free( orig );
         orig = NULL;
      }
   }

   if( orig )
   {
      u4free( orig );
      orig = NULL;
   }

   /* set the pointer to the new source buffer */
   *psrc = src;
}



/************************************************************
 *
 * Function: relate4retrieve_relate_foo()
 *
 *  PARAMETERS:
 *   FILE4SEQ_READ* seq - sequential read struture for the file i/o
 *   int open_files - should the data files for the relate be opened
 *   char* spath - path to look for the data files
 *   the rest of the parameters are simply character buffers used in the fctn
 *
 *  DESCRIPTION: retrieves a relation from a file.  Note: this file and the
 *   r4save.c source file are compiled directly into the CR executable, as well
 *   as into the CodeBase DLL.  As a result certain sections of this code
 *   are specific the the executable and call functions in the executable
 *
 *  RETURNS: a RELATE4 pointer on success, NULL on failure
 *
 *  By: Raymond Cypher
 *
 *  HISTORY:
 *
 */
// AS Dec 13/05 - improvements for function deprecation
#define TEMP4NAME_BUF_LEN 512
#define STR4BUF_LEN 256
static RELATE4 *S4FUNCTION relate4retrieve_relate_foo( FILE4SEQ_READ *seq, int open_files, char *spath,
                           char *dname_buf, char *iname_buf, char *tname_buf,
                           char *str_buf, char *masterExpr_buf,
                           char *slave_expr_buf, char *tempname_buf, int file_type )
{
   RELATE4 *relate = NULL, *master = NULL;
   DATA4   *data;
   TAG4    *tag;
   INDEX4  *index;
   LIST4   indexes;
   CODE4   *c4;
   INAME4  *iname, *nname;
   EXPR4   *expr;
   char alias_buf[LEN4DATA_ALIAS + 1], *cptr, *tmp_buf;
/* removed because aliases should be allowed in client/server
#ifdef S4CLIENT
   char temp_buf[11];
#endif
*/
   short matchLen, relationType, sortType, errorAction, nLinks, code;
   int i, err_code, err_flag, repeat_flag, errorCode = 0, iindex, tname_err;
   int indexType ;
   REPORT4 *dummyreport;
   #ifdef S4CR2
      PN4CHANGE nchange;
      int rc;
   #endif


   /* reset the changed name list */
   memset( &name_list, 0, sizeof(name_list) );

   c4 = seq->file->codeBase;
   c4->errOpen = 0 ;   /* JH 11/01/99 - added so that Load Relation does not */
                       /* report an error if the database file is not in the */
                       /* same location as the relation (REL) one.           */

   /* reset the index name list */
   memset( &indexes, 0, sizeof(indexes) );
   err_code = err_flag = 0;

   while( 1 )
   {
      /* get the data file name */
      if( retrieve4string( seq, dname_buf, 256 ) < 0 )
      {
         /* the error codes are used to allow the continued retrieval of
            the relation tree, even if a part of it is not retrievable due
            to a missing data or index file */
         errorCode = 1;
         goto CLEANUP;
      }

      /* get the files alias */
      memset( alias_buf, 0, sizeof(alias_buf) );
      if( file_version >= 0x24 )
      {
         if( retrieve4string( seq, alias_buf, sizeof(alias_buf) ) < 0 )
         {
            errorCode = 1;
            goto CLEANUP;
         }
      }
      /* removed because aliases should be allowed in client/server
         #ifdef S4CLIENT
            u4namePiece( temp_buf, sizeof( temp_buf ), dname_buf, 0, 0 ) ;
            if( strcmp( alias_buf, temp_buf ) )
               strcpy( alias_buf, temp_buf ) ;
         #endif
      */

      /* retrieve the internal flags of the RELATE4 */
      ret4short( seq, &matchLen );
      ret4short( seq, &relationType );
      ret4short( seq, &sortType );
      ret4short( seq, &errorAction );

      /* retrieve the names of the index files and place them in the index list */
      ret4short( seq, &nLinks );
      for( i = 0; i < nLinks; i++ )
      {
         iname = (INAME4 *)u4allocFree( c4, sizeof(INAME4) );
         if( retrieve4string( seq, iname_buf, 256 ) < 0 )
         {
            errorCode = 1;
            goto CLEANUP;
         }
         /* LY 2001/08/31 : changed both lines from using +1, since u4nameExt() is used to add extension */
         iname->name_length = strlen(iname_buf) + 5;
         iname->index_name = (char *)u4allocFree( c4, iname->name_length );
         if ( iname->index_name == 0 )
         // AS Dec 13/05 - improvements for function deprecation
         {
            u4free( iname ) ;
            errorCode = 1;
            goto CLEANUP;
         }
         c4strcpy( iname->index_name, iname->name_length, iname_buf );
         l4add( &indexes, iname );
      }

      /* get the tag name */
      if( retrieve4string( seq, tname_buf, 256 ) < 0 )
      {
         errorCode = 1;
         goto CLEANUP;
      }

      /* retrieve the source for the master expression */
      if( retrieve4string( seq, masterExpr_buf, 1024 ) < 0 )
      {
         errorCode = 1;
         goto CLEANUP;
      }

      /* check for changed names */
      cptr = masterExpr_buf;
      report4nchange( c4, &cptr, 0, 1024 );

      /* the code indicates where the new relate should fall in the tree with
         respect to the last relate.  The code used is the same as the return
         values from relate4next() */
      ret4short( seq, &code );

      if( !err_flag && !err_code )
      {
         repeat_flag = 1;
         while( repeat_flag )
         {
            repeat_flag = 0;
            /* see if data file is already open */
            data = 0;
            u4namePiece( str_buf, 256, dname_buf, 0, 0 );
            data = code4data( c4, alias_buf );
            /* if not already open try to open */
            if( !data )
            {
               /* if not allowed to open, error */
               if( !open_files )
               {
                  error4describe( c4, e4result, 0L, E4_RESULT_LCF, dname_buf, (char *)0 );
                  err_flag = 1;
               }

               /* check for an alternate path */
               if( spath && spath[0] != '\0' )
               {
                  u4namePiece( str_buf, 256, dname_buf, 0, 1 );
                  // AS Dec 13/05 - improvements for function deprecation
                  c4strcpy( tempname_buf, TEMP4NAME_BUF_LEN, spath );
                  if( *(tempname_buf+(strlen(tempname_buf)-1) ) != '\\' )
                     c4strcat( tempname_buf, TEMP4NAME_BUF_LEN, "\\" );
                  c4strcat( tempname_buf, TEMP4NAME_BUF_LEN, str_buf );
                  /* open the data file */
                  data = r4open_data( tempname_buf, alias_buf, master, c4 );
               }
               else
               {
                  /* open the data file */
                  data = r4open_data( dname_buf, alias_buf, master, c4 );
               }

               /* if the open failed */
               if( !data )
               {
                  /* if this is for the CR executable interactively get an
                     alternate file name */
                  #ifdef S4CR2
                  nchange = (PN4CHANGE)u4allocFree( c4, sizeof(N4CHANGE) );
                  if( nchange )
                  {
                     u4namePiece( str_buf, STR4BUF_LEN, dname_buf, 0, 0 );
                     nchange->old_name = (char *)u4allocFree( c4, strlen(str_buf)+1 );
                     if( nchange->old_name )
                     {
                        // AS Dec 13/05 - improvements for function deprecation
                        c4strcpy( nchange->old_name, STR4BUF_LEN, str_buf );
                        rc = AlternateDataFile( dname_buf, 256 );
                        if( rc == 0 )
                        {
                           u4namePiece( str_buf, STR4BUF_LEN, dname_buf, 0, 0 );
                           // AS Dec 13/05 - improvements for function deprecation
                           int strLen = strlen(str_buf) + 1 ;
                           nchange->new_name = (char *)u4allocFree( c4, strLen );
                           if( nchange->new_name )
                           {
                              c4strcpy( nchange->new_name, strLen, str_buf );
                              l4add( &name_list, nchange );
                              repeat_flag  = 1;
                           }
                           else
                           {
                              u4free( nchange->old_name );
                              u4free( nchange );
                              err_flag = 1;
                           }
                        }
                        else
                           err_flag = 1;
                     }
                     else
                     {
                        u4free( nchange );
                        err_flag = 1;
                     }
                  }
                  else
                     err_flag = 1;
                  #else
                     /* if not in the executable report an error */
                     if( spath && spath[0] != '\0' )
                        error4describe( c4, e4report, 0L, E4_REP_DFILE, tempname_buf, (char *)0 );
                     else
                        error4describe( c4, e4report, 0L, E4_REP_DFILE, dname_buf, (char *)0 );
                     err_flag = 1;
                  #endif
               }
               error4set( c4, 0 );
            }
         }
      }

      if( !err_flag && !err_code )
      {
         /* loop through index file names */
         iname = (INAME4 *)l4first( &indexes );
         while( iname )
         {
            repeat_flag = 1;
            while( repeat_flag )
            {
               repeat_flag = 0;
               index = NULL;
               iindex = 0;

               u4namePiece( str_buf, 256, iname->index_name, 0, 0 );
               tname_err = c4->errTagName;
               c4->errTagName = 0;

               /* check to see if the index is already open */
               iindex = r4index_lookup( data, str_buf );
               c4->errTagName = tname_err;
               error4set( c4, 0 );

               /* if not already open */
               if( !iindex )
               {
                  /* if not allowed to open files report an error */
                  if( !open_files )
                  {
                     error4describe( c4, e4result, 0L, E4_RESULT_LCF, iname_buf, (char *)0 );
                     err_flag = 1;
                  }

                  /* by default this compile switch is defined in r4report.h
                     it forces the index file name to have the default extension
                     for the compile flag, regardless of the extension saved in
                     the report.  IF the user is using a different extension
                     he should undefine this switch */
                  #ifdef S4DEFAULT_INDEX
                  #ifdef S4CLIENT
                     indexType = code4indexFormat( c4 ) ;
                  #else
                     indexType = report4index_type() ;
                  #endif
                  switch( indexType )
                  {
                     /* LY 2001/08/31 : changed 2nd param to u4namExt from strlen(iname->index_name */
                     case r4cdx :
                        u4nameExt( iname->index_name, iname->name_length, "cdx", 1 );
                        break ;
                     case r4mdx :
                        u4nameExt( iname->index_name, iname->name_length, "mdx", 1 );
                        break ;
                     case r4ntx :
                        u4nameExt( iname->index_name, iname->name_length, "ntx", 1 );
                        break ;
                  }
                  #endif

                  /* attempt to open the index file */
                  if( spath && spath[0] != '\0' )
                  {
                     u4namePiece( str_buf, 256, iname->index_name, 0, 1 );
                     // AS Dec 13/05 - improvements for function deprecation
                     c4strcpy( tempname_buf, TEMP4NAME_BUF_LEN, spath );
                     if( *(tempname_buf+(strlen(tempname_buf)-1) ) != '\\' )
                        c4strcat( tempname_buf, TEMP4NAME_BUF_LEN, "\\" );
                     c4strcat( tempname_buf, TEMP4NAME_BUF_LEN, str_buf );
                     index = i4open( data, tempname_buf );
                  }
                  else
                     index = i4open( data, iname->index_name );

                  /* if open fails */
                  if( !index )
                  {
                     /* if in CR executable prompt for alternate */
                     #ifdef S4CR2
                     lstrcpy( str_buf, iname->index_name );
                     rc = AlternateIndexFile( str_buf, 256 );
                     if( rc == 0 )
                     {
                        repeat_flag = 1;
                        u4free( iname->index_name );
                        iname->index_name = NULL;
                        iname->index_name = (char *)u4alloc( lstrlen(str_buf)+1 );
                        if( iname->index_name )
                           lstrcpy( iname->index_name, str_buf );
                     }
                     else
                        err_flag = 1;
                     #else
                     /* report an error */
                     if( spath && spath[0] != '\0' )
                        error4describe( c4, e4report, 0L, E4_REP_IFILE, tempname_buf, (char *)0 );
                     else
                        error4describe( c4, e4report, 0L, E4_REP_IFILE, iname_buf, (char *)0 );
                     err_flag = 1;
                     #endif
                  }
                  error4set( c4, 0 );
               }
            }
            iname = (INAME4 *)l4next( &indexes, iname );
         }
      }

      if( !err_flag && !err_code )
      {
         tag = NULL;

         /* if this is the first relate do a relate4init() */
         if( !relate )
         {
            master = relate = relate4init( data );
            if( !relate )
               goto CLEANUP;

            /* set the internal flags */
            relate->matchLen = matchLen;
            relate->sortType = sortType;
            relate->relationType = relationType;
            relate->errorAction = errorAction;

            /* if a tag was specified set it */
            if( strlen(tname_buf) )
            {
               repeat_flag = 1;
               while( repeat_flag )
               {
                  repeat_flag = 0;
                  tag = NULL;
                  tag = d4tag( data, tname_buf );
                  if( tag )
                  {
                     relate->dataTag = tag;
                     d4tagSelect( data, tag );
                  }
                  #ifdef S4CR2
                  else
                  {
                     /* if the specified tag is not available and this is the
                        .exe prompt for an alternate */
                     error4set( c4, 0 );
                     rc = AlternateTagName( tname_buf, 256 );
                     if( rc == 0 )
                        repeat_flag = 1;
                  }
                  #else
                  else
                     error4set( c4, 0 );
                  #endif
               }
            }
         }
         else
         {
            /* if not the first relate do a create slave */
            /* start by getting a tag pointer from the tag name */
            if( strlen(tname_buf) )
            {
               repeat_flag = 1;
               while( repeat_flag )
               {
                  repeat_flag = 0;
                  tag = NULL;
                  tag = d4tag( data, tname_buf );
                  if( !tag )
                  #ifdef S4CR2
                  {
                     error4set( c4, 0 );
                     rc = AlternateTagName( tname_buf, 256 );
                     if( rc == 0 )
                        repeat_flag = 1;
                     else
                     {
                        goto CLEANUP;
                     }
                  }
                  #else
                  {
                     error4describe( c4, e4report, 0L, E4_REP_NOTAG, (char *)tname_buf, 0 );
                     error4set( c4, 0 );
                     goto CLEANUP;
                  }
                  #endif
               }
            }

            /* try to create the slave */
            repeat_flag = 1;
            while( repeat_flag )
            {
               repeat_flag = 0;
               expr = expr4parse( master->data, masterExpr_buf );
               if( expr )
               {
                  expr4free( expr );
                  relate = relate4createSlave( master, data, masterExpr_buf, tag );
               }
               #ifdef S4CR2
               else
               {
                  /* if create slave fales prompt for a different master expr */
                  error4set( c4, 0 );
                  rc = AlternateMasterExpression( masterExpr_buf, master, 1024 );
                  if( rc == 0 )
                     repeat_flag = 1;
                  else
                     relate = NULL;
               }
               #else
               else
               {
                  /* report an error */
                  error4describe( c4, e4report, 0L, E4_REP_NOMEXPR, 0, 0 );
                  error4set( c4, 0 );
                  relate = NULL;
               }
               #endif
            }

            if( !relate )
               goto CLEANUP;

            /* #ifdef S4CR2 */
               if(relate->matchLen < matchLen)
                  matchLen = relate->matchLen;
            /* #endif */

            /* relate->matchLen = matchLen; */
            relate->sortType = sortType;
            relate->relationType = relationType;
            relate->errorAction = errorAction;
         }
      }

      /* free the index name list */
      iname = (INAME4 *)l4first( &indexes );
      while( iname )
      {
         nname = (INAME4 *)l4next( &indexes, iname );
         l4remove( &indexes, iname );
         u4free( iname->index_name );
         u4free( iname );
         iname = nname;
      }

      /* if end of relates leave the loop */
      if( code == 2 )
         break;

      if( err_flag == 0 && err_code > 0 )
      {
         err_code += code;
         if( err_code <= 0 )
         {
            err_code = 0;
            code = 1;
         }
      }

      if( err_flag == 1 )
      {
         err_flag = 0;
         err_code = 1;
      }

      if( err_code <= 0 )
      {
         master = relate;
         while( code++ <= 0 )
            master = master->master;
      }
   }

   /* deal with the sort and query expressions */
   memset( masterExpr_buf, 0, 1024 );
   memset( slave_expr_buf, 0, 1024 );
   retrieve4string( seq, masterExpr_buf, 1024 );
   cptr = masterExpr_buf;
   report4nchange( c4, &cptr, 0, 1024 );
   retrieve4string( seq, slave_expr_buf, 1024 );
   cptr = slave_expr_buf;
   report4nchange( c4, &cptr, 0, 1024 );
   if( master )
   {
      repeat_flag = 1;
      if( strlen( slave_expr_buf) > 0 )
      while( repeat_flag )
      {
         repeat_flag = 0;
         expr = expr4parse( master->data, slave_expr_buf );
         if( !expr && !lookahead_buf && (file_type == R4REPORT_FILE) )
         {
            lookedahead_calc = TRUE;
            err_code = 0;

            /* this is a dummy variable used to call generic function which
               requires only the first two pointer addresses */
            dummyreport = (REPORT4 *)u4allocFree( seq->file->codeBase, 8 );
            dummyreport->codeBase = seq->file->codeBase;
            dummyreport->relate = relate;

            if( file_version >= 0x26 )
               lookahead_buf = (char *)u4allocFree( seq->file->codeBase, (2048 + (5 * sizeof(S4LONG))) );
            else
               lookahead_buf = (char *)u4allocFree( seq->file->codeBase, 2048 );

            tmp_buf = lookahead_buf;
            if( retrieve4string( seq, tmp_buf, 1024 ) < 0 )
               err_code = 1;
            else
            {
               tmp_buf += 1024;
               if( retrieve4string( seq, tmp_buf, 1024 ) < 0 )
                  err_code = 1;
               else
               {
                  tmp_buf += 1024;
                  /* reading the margins if req'd */
                  if( file_version >= 0x26 )
                  {
                     for( i = 0; i <= 4; i++ )
                     {
                        ret4long( seq, (long *)tmp_buf );
                        tmp_buf += sizeof(S4LONG);
                     }
                  }
               }
            }

            if( !err_code )
            {
               if( report4retrieve_calc( seq, dummyreport ) != 0 )
                  expr = expr4parse( master->data, slave_expr_buf );
               else
                  err_code = 1;
            }

            if( err_code )
            {
               lookedahead_calc = FALSE;
               u4free( lookahead_buf );
               lookahead_buf = NULL;
               u4free( dummyreport );
            }
         }
         else
            lookedahead_calc = FALSE;

         if( expr )
         {
            expr4free( expr );
            relate4sortSet( master, slave_expr_buf );
         }
         #ifdef S4CR2
         else
         {
            error4set( c4, 0 );
            rc = AlternateSortExpression( slave_expr_buf, master, 1024 );
            if( rc == 0 )
               repeat_flag = 1;
         }
         #else
         else
         {
            error4describe( c4, e4report, 0L, E4_REP_NOSORT, 0, 0 );
            error4set( c4, 0 );
         }
         #endif
      }

      repeat_flag = 1;
      if( strlen( masterExpr_buf ) > 0 )
      while( repeat_flag )
      {
         repeat_flag = 0;
         expr = expr4parse( master->data, masterExpr_buf );

         if( !expr && !lookahead_buf && (file_type == R4REPORT_FILE) )
         {
            lookedahead_calc = TRUE;
            err_code = 0;

            /* this is a dummy variable used to call generic function which
               requires only the first two pointer addresses */
            dummyreport = (REPORT4 *)u4allocFree( seq->file->codeBase, 8 );
            dummyreport->codeBase = seq->file->codeBase;
            dummyreport->relate = relate;

            if( file_version >= 0x26 )
               lookahead_buf = (char *)u4allocFree( seq->file->codeBase, (2048 + (5 * sizeof(S4LONG))) );
            else
               lookahead_buf = (char *)u4allocFree( seq->file->codeBase, 2048 );

            tmp_buf = lookahead_buf;
            if( retrieve4string( seq, tmp_buf, 1024 ) < 0 )
               err_code = 1;
            else
            {
               tmp_buf += 1024;
               if( retrieve4string( seq, tmp_buf, 1024 ) < 0 )
                  err_code = 1;
               else
               {
                  tmp_buf += 1024;
                  /* reading the margins if req'd */
                  if( file_version >= 0x26 )
                  {
                     for( i = 0; i <= 4; i++ )
                     {
                        ret4long( seq, (long *)tmp_buf );
                        tmp_buf += sizeof(S4LONG);
                     }
                  }
               }
            }

            if( !err_code )
            {
               if( report4retrieve_calc( seq, dummyreport ) != 0 )
                  expr = expr4parse( master->data, masterExpr_buf );
               else
                  err_code = 1;
            }

            if( err_code )
            {
               lookedahead_calc = FALSE;
               u4free( lookahead_buf );
               lookahead_buf = NULL;
               u4free( dummyreport );
            }
         }
/*         else                           */
/*            lookedahead_calc = FALSE;   */

         if( expr )
         {
            expr4free( expr );
            relate4querySet( master, masterExpr_buf );
         }
         #ifdef S4CR2
         else
         {
            error4set( c4, 0 );
            rc = AlternateQueryExpression( masterExpr_buf, master, 1024 );
            if( rc == 0 )
               repeat_flag = 1;
         }
         #else
         else
         {
            error4describe( c4, e4report, 0L, E4_REP_NOQUERY, 0, 0 );
            error4set( c4, 0 );
         }
         #endif
      }
   }

   if( relate )
      return( &relate->relation->relate );
   else
      return NULL;

CLEANUP:
   iname = (INAME4 *)l4first( &indexes );
   while( iname )
   {
      nname = (INAME4 *)l4next( &indexes, iname );
      l4remove( &indexes, iname );
      u4free( iname->index_name );
      u4free( iname );
      iname = nname;
   }

   if( master )
   {
      relate4free( master, open_files );
   }
   else
   {
      if( relate )
         relate4free( relate, open_files );
   }

   if( errorCode )
   {
      error4describe( c4, e4report, 0L, E4_REP_RELERR, 0, 0 );
   }
   return NULL;
}

/* wrapper for above function */
RELATE4 * S4FUNCTION   relate4retrieve_relate( FILE4SEQ_READ *seq, int open_files, char *spath, int file_type )
{
   char *dname_buf = NULL, *iname_buf = NULL, *tname_buf = NULL, *str_buf = NULL;
   char *masterExpr_buf = NULL, *slave_expr_buf = NULL, *tempname_buf = NULL;
   RELATE4 *retvalue = NULL;

   /* global vars req'd if user-defined calc objects are read in to    */
   /* satisfy the query expression */
   lookahead_buf = NULL;
   lookedahead_calc = FALSE;

   dname_buf = (char *)u4allocFree( seq->file->codeBase, 256 );
   iname_buf = (char *)u4allocFree( seq->file->codeBase, 256 );
   tname_buf = (char *)u4allocFree( seq->file->codeBase, 256 );
   str_buf = (char *)u4allocFree( seq->file->codeBase, STR4BUF_LEN );
   masterExpr_buf = (char *)u4allocFree( seq->file->codeBase, 1024 );
   slave_expr_buf = (char *)u4allocFree( seq->file->codeBase, 1024 );
   tempname_buf = (char *)u4allocFree( seq->file->codeBase, TEMP4NAME_BUF_LEN );
   if( !dname_buf || !iname_buf || !tname_buf || !str_buf ||
       !masterExpr_buf || !slave_expr_buf || !tempname_buf )
      goto R4LEAVE;

   retvalue = relate4retrieve_relate_foo( seq, open_files, spath,
                 dname_buf, iname_buf, tname_buf, str_buf, masterExpr_buf,
                 slave_expr_buf, tempname_buf, file_type );

R4LEAVE:
   if( dname_buf )
      u4free( dname_buf );
   if( iname_buf )
      u4free( iname_buf );
   if( tname_buf )
      u4free( tname_buf );
   if( str_buf )
      u4free( str_buf );
   if( masterExpr_buf )
      u4free( masterExpr_buf );
   if( slave_expr_buf )
      u4free( slave_expr_buf );
   if( tempname_buf )
      u4free( tempname_buf );

   return retvalue;

}

/* see the CodeReporter manual */
int S4FUNCTION obj4dataFieldSet( POBJ4 obj, char *fname, char ftype, int flength, int fdec )
{
   POUT4OBJ oobj;
   PREPORT4 report;

   if( !obj )
      return -1;

   report = obj->area->report;
   obj->field_type = ftype;
   obj->field_len = flength;
   obj->field_dec = fdec;
   if( fname )
      u4ncpy( obj->field_name, fname, sizeof(obj->field_name) );
   else

   oobj = (POUT4OBJ)l4first( &report->output_objs );
   while( oobj )
   {
      if( oobj->obj == obj )
      {
         if( fname == NULL || ftype == 0 || flength == 0 )
            l4remove( &report->output_objs, oobj );
         return 0;
      }
      oobj = (POUT4OBJ)l4next( &report->output_objs, oobj );
   }

   oobj = (POUT4OBJ)u4allocFree( obj->area->report->codeBase, sizeof(OUT4OBJ) );
   if( !oobj )
      return -1;
   oobj->obj = obj;
   l4add( &report->output_objs, oobj );
   return 0;
}

/* see the CodeReporter manual */
int S4FUNCTION report4dataFileSet( PREPORT4 report, char *fname )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   if( report->dfile_name )
      u4free( report->dfile_name );
   report->dfile_name = NULL;

   if( fname == NULL )
      return 0;

   report->dfile_name = (char *)u4allocEr( report->codeBase, strlen(fname)+1 );
   if( !report->dfile_name )
      return -1;

   u4ncpy( report->dfile_name, fname, strlen(fname)+1 );
   return 0;
}

/* see the CodeReporter manual */
int S4FUNCTION report4dataGroup( PREPORT4 report, PGROUP4 group )
{
   #ifdef E4PARM_HIGH
           if( report == 0 )
                   return error4( 0, e4parm_null, E95702 ) ;
   #endif

   report->output_group = group;
   return 0;
}
#endif   /* S4OFF_REPORT */
