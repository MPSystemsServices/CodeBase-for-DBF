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

// gen4.cpp (c)Copyright Sequiter Software Inc., 1999.  All rights reserved.

// Data file and header source code generation.
// Must have enough characters in flag name to uniquely specify it.

// -safety {ON|OFF}   (Safety ON/OFF for following source/header files; starts on)
// -header header_name  (header default - gen.h )
// -source source_name  (program default - gen.cpp )
// -production {ON|OFF}   (Production Tags ON/OFF for following data files;
//                         starts on)
// -create {ON|OFF}  (Add code to open function to automatically create data
//                      file and index files if they do not exist.  Default ON.)
// -data  db1.dbf
// -classname db1name [derive_name]
//                   (Class Name override; default same as data file)
//                   (Derive name default is Data4)
// -index ind1.cdx
// -index ind2.cdx
// -case {UPPER|LOWER|MIXED}
// -tagid  tag_id_prefix    (Default 'tag_')
// -fieldid field_id_prefix (Default - Nothing)
// -output source_name [header_name]

#ifdef __TURBOC__
   extern unsigned _stklen = 15000 ;
#endif

#include "d4all.hpp"

#ifndef S4STAND_ALONE
   #error GEN4 can only be compiled in stand-alone mode.
#endif

#ifndef S4UNIX
   #ifdef __TURBOC__
      #pragma hdrstop
   #endif
#endif

static Code4 cb ;

class FileStream : public File4seqWrite
{
public:
   FileStream( char *name ) { u4ncpy( file_name, name, sizeof(file_name) ) ; }
   int open()
   {
      close() ;
      if( file.create(cb, file_name, 1) < 0 )  return -1 ;
      init( file, 0L, write_buf, sizeof(write_buf) ) ;
      Date4 today ;
      today.today( ) ;
      *this << "// "
         << file_name
         << "  Generated File - Avoid hand modification.\r\n" << "// "
         << "Created: " << today.format( "MMM DD, CCYY" )
         << "\r\n\r\n" ;
      return 0 ;
   }
   int close()
   {
      if( !file.isValid() )  return 0 ;
      flush() ;
      if( file.close() < 0 )  return -1 ;
      return 0 ;
   }
   File4 file ;
   char file_name[80] ;
   char write_buf[2048] ;
} ;

class ProgramParameters
{
public:
   ProgramParameters(int c, void **v ) { reset( c, v) ; }

   int get();           // Returns -1 on error; zero length string - done
   void get_undo() { get_undone = 1 ; u4ncpy( parm_save, parm, sizeof(parm_save)) ; }
   char get_on_off() ;
   int get_option( char *o1, char *o2, char *o3 ) ;
   int get_parm( int must_be_parm = 1) ;
   int to_switch() ;
   int is_switch( char *switch_name, int min_chars = 1 ) ;
   int is_valid() { return parm[0] ; }
   void reset( int c, void ** v )
   {
      argc = c;
      argv = v;
      parm_on = 0 ;
      get_undone = 0 ;
   }

   File4 file ;
   File4seqRead seq ;

   int  parm_on, get_undone ;
   char buf[2048] ;
   char parm[256] ;
   char parm_save[256] ;
   char parm_switch[256] ;

   int argc ;
   void **argv ;
} ;

class Settings
{
public:
   Settings()
   {
      tag_id_prefix.assign( "tag_" ) ;
      create_on_off = 1 ;
      case_names = 1 ; // Default is lower case
      class_name_derive.assign( "Data4" ) ;
   }

   Str4large class_name,
      class_name_derive,
      field_id_prefix,
      tag_id_prefix ;
   int create_on_off ;
   int case_names ;
} ;

Settings def, current ;
inline void set_to_global_values() { current = def ; }

FileStream header( "GEN.HPP" ), source( "GEN.CPP" ) ;
static Data4 db ;

char *is_prod = 0 ; // 0 None; 1 Yes; 2 Default
unsigned is_prod_len = 0 ;
unsigned is_prod_i = 0 ;
char  is_prod_default = 1 ;

int generate_code() ;
char *generate_index_name( Index4 ) ;
char *generate_dbf_name() ;
char *generate_case_name( const char * ) ;

int main( int argc, void *argv[] )
{
   int rc = gen4( argc, argv ) ;
   cb.initUndo( ) ;
   return rc ;
}

int gen4( int argc, void *argv[] )
{
   int rc ;
   ProgramParameters parms( argc, argv ) ;
   Settings *set = &def ;

   for(;;)  // Pass 1 through the switches
   {
      if( parms.get() < 0 )  return -1 ;
         if( (rc = parms.to_switch()) < 0 )  return -1 ;
            if( parms.parm[0] == 0 )
               break ;

      if( parms.is_switch( "safety", 2 ) )
      {
          if( is_prod_i > 0 )
             return cb.error( e4result, 0, "Safety must be set ON or OFF before any data files are specified." ) ;

          if( (cb.safety = parms.get_on_off()) < 0 ) return -1 ;
             continue ;
      }

      if( parms.is_switch( "output" ) )
      {
         if( is_prod_i > 0 )
            return cb.error( e4result,0, "Output Files must be specified before any data files." ) ;
         if( parms.get_parm() < 0 )  return -1 ;

         Str4large buf( parms.parm ) ;
         buf.upper() ;
         u4nameExt( buf.ptr(), buf.maximum(), "CPP", 0 ) ;
         u4ncpy( source.file_name, buf.ptr(), sizeof(source.file_name) ) ;

         if( parms.get_parm(0) < 0 )  return -1 ;

         if( parms.parm[0] == 0 )
            u4nameExt( buf.ptr(), buf.maximum(), "HPP", 1 ) ;
         else
         {
            buf.assign( parms.parm ) ;
            u4nameExt( buf.ptr(), buf.maximum(), "HPP", 0 ) ;
         }

         u4ncpy( header.file_name, buf.ptr(), sizeof(header.file_name) ) ;
         continue ;
      }

      if( parms.is_switch( "header" ) )
      {
         if( is_prod_i > 0 )
            return cb.error( e4result,0, "Output Files must be specified before any data files." ) ;
         if( parms.get_parm() < 0 )  return -1 ;

         Str4large buf( parms.parm ) ;
         buf.upper() ;
         if( parms.get_parm(0) < 0 )  return -1 ;

         if( parms.parm[0] == 0 )
            u4nameExt( buf.ptr(), buf.maximum(), "HPP", 1 ) ;
         else
         {
            buf.assign( parms.parm ) ;
            u4nameExt( buf.ptr(), buf.maximum(), "HPP", 0 ) ;
         }

         u4ncpy( header.file_name, buf.ptr(), sizeof(header.file_name) ) ;
         continue ;
      }

      if( parms.is_switch( "classname", 2 ) )
      {
         if( parms.get_parm() < 0 )  return -1 ;
         set->class_name.assign( parms.parm ) ;

         if( parms.get_parm(0) < 0 )  return -1 ;
         if( parms.parm[0] == 0 )
            current.class_name_derive = def.class_name_derive ;
         else
            set->class_name_derive.assign( parms.parm ) ;
         continue ;
      }

      if( parms.is_switch( "data" ) )
      {
         u4allocAgain( &cb, &is_prod, &is_prod_len, sizeof(char) * ++is_prod_i ) ;
         is_prod[is_prod_i-1] = 2 ;
         continue ;
      }

      if( parms.is_switch( "production" ) )
      {
         if( is_prod_i == 0 )
         {
            if( (rc = parms.get_on_off()) < 0 )  return -1 ;
            is_prod_default = char(rc) ;
            continue ;
         }

         if( (rc = parms.get_on_off()) < 0 )  return -1 ;
         is_prod[is_prod_i-1] = char(rc) ;
         continue ;
      }
   }

   parms.reset(argc,argv) ;
   is_prod_i = 0 ;

   for(;;)
   {
      if( parms.get() < 0 )  return -1 ;
      if( (rc = parms.to_switch()) < 0 )  return -1 ;
      if( parms.parm[0] == 0 )
      {
         if( generate_code() < 0 )  return -1 ;
         header.close() ;
         source.close() ;
         return 0 ;
      }

      if( parms.is_switch( "case", 2 ) )
      {
         if( (set->case_names =  parms.get_option( "UPPER", "LOWER", "MIXED" )) < 0 )
            return -1 ;
         continue ;
      }

      if( parms.is_switch( "classname", 2 ) )
      {
         if( parms.get_parm() < 0 )  return -1 ;
         set->class_name.assign( parms.parm ) ;

         if( parms.get_parm(0) < 0 )  return -1 ;
         if( parms.parm[0] == 0 )
            current.class_name_derive =  def.class_name_derive ;
         else
            set->class_name_derive.assign( parms.parm ) ;
         continue ;
      }

      if( parms.is_switch( "create", 2 ) )
      {
         if( (set->create_on_off = parms.get_on_off()) < 0 )  return -1 ;
         continue ;
      }

      if( parms.is_switch( "data" ) )
      {
         if( db.isValid() )
         {
            if( generate_code() < 0 )  return -1 ;
            db.close() ;
         }

         set_to_global_values() ;
         set =  &current ;

         rc = parms.get_parm() ;
         if( rc < 0 )
            return -1 ;

         if( is_prod[is_prod_i] == 2 )
            cb.autoOpen =  is_prod_default ;
         else
            cb.autoOpen = is_prod[is_prod_i] ;
         is_prod_i++ ;

         rc = db.open( cb, parms.parm ) ;
         if( rc < 0 )
            return -1 ;
         set->class_name.assign( db.alias() ) ;
         continue ;
      }

      if( parms.is_switch( "fieldid" ) )
      {
         if( parms.get_parm(0) < 0 )  return -1 ;
         set->field_id_prefix.assign( parms.parm ) ;
         continue ;
      }

      if( parms.is_switch( "index" ) )
      {
         if( parms.get_parm() < 0 )  return -1 ;

         if( ! db.isValid() )
         return cb.error( e4result,0, "Specify a data file before any index files", "Index File:",
                parms.parm ) ;

         Index4 index( db, parms.parm ) ;
         if( cb.errorCode < 0 )
            return -1 ;
         continue ;
      }

      if( parms.is_switch( "tagid", 1 ) )
      {
         if( parms.get_parm(0) < 0 )  return -1 ;
         set->tag_id_prefix.assign( parms.parm ) ;
         continue ;
      }

      if( parms.is_switch("safety",2) || parms.is_switch("output") ||
         parms.is_switch("header")   || parms.is_switch("production") )
      {
         if( parms.get_parm() < 0 ) return -1 ;
         if( parms.get_parm(0) < 0 )  return -1 ;
         continue ;  // Delt with in first pass
      }

      if( cb.errorCode < 0 )  return -1 ;
      cb.error( e4result,0, "Unknown switch encountered under Code Generation:",
      parms.parm ) ;
      return -1 ;
   }
   cb.initUndo( ) ;
}

int ProgramParameters::get()
{
   if( get_undone )
   {
      u4ncpy( parm, parm_save, sizeof(parm) ) ;
      get_undone = 0 ;
      return 0 ;
   }

   if( parm_on == 0 )
   {
      if( argc < 2 )
      {
         printf( "\n GEN4 { INPUT_FILE_NAME | SWITCH_LIST } \n\n" ) ;
         return -1 ;
      }

      if( argc == 2 )
      {
         if( *(char *)argv[1] != '-' )
         {
            parm_on = -1 ;
            if( ! file.isValid() )
            if( file.open( cb, (char *) argv[1] ) < 0 )  return -1 ;
            seq.init( file, 0L, buf, sizeof(buf) ) ;
         }
      }
   }

   if( parm_on < 0 )
   {
      for(int pos = 0;;)
      {
         if( pos == sizeof(parm) )
         {
            cb.error( e4result,0, "Parameter has more than 256 characters." ) ;
            return -1 ;
         }

         char ch ;
         unsigned u = seq.read( &ch, sizeof(ch) ) ;
         if( cb.errorCode < 0 )  return -1 ;
            if( u == 0 )
            {
               parm[pos] = 0 ;
               return 0 ;
            }

         if( ch <= ' ' )
         {
            if( pos == 0 )
              continue ;
            parm[pos++] = 0 ;
            break ;
         }
         parm[pos++] = ch ;
      }
   }
   else
   {
      if( ++parm_on >= argc )
         parm[0] = 0 ;
      else
         u4ncpy( parm, (char *) argv[parm_on], sizeof(parm) ) ;
   }
   return 0 ;
}

char ProgramParameters::get_on_off()
{
   if( get() < 0 )  return -1 ;

   c4upper( parm ) ;
   if( strcmp(parm,"ON") == 0 )
      return 1 ;
   if( strcmp(parm,"OFF") == 0 )
      return 0 ;
   cb.error( e4result,0, "Expecting ON or OFF", parm ) ;
   return -1 ;
}

int ProgramParameters::get_option( char *o0, char *o1, char *o2 )
{
   if( get() < 0 )  return -1 ;

   c4upper( parm ) ;
   if( o0 )
      if( strcmp(parm,o0) == 0 )
         return 0 ;
   if( o1 )
      if( strcmp(parm,o1) == 0 )
         return 1 ;
   if( o2 )
      if( strcmp(parm,o2) == 0 )
         return 2 ;
   cb.error( e4result,0, "Wrong Parameter Value for Switch:", parm_switch, parm ) ;
   return -1 ;
}

int ProgramParameters::get_parm( int must_be_parm )
{
   if( get() < 0 )  return -1 ;
   if( parm[0] == 0  ||  parm[0] == '-' )
   {
      if( must_be_parm )
      {
         cb.error( e4result,0, "Switch Value Missing" ) ;
         return -1 ;
      }
      else
      {
         get_undo() ;
         parm[0] = 0 ;
         return 0 ;
      }
   }
   return 0 ;
}

int ProgramParameters::to_switch()
{
   if( parm[0] == 0 )
      parm[1] = 0 ;
   u4ncpy( parm_switch, parm+1, sizeof(parm_switch) ) ;
   c4lower( parm_switch ) ;
   return 0 ;
}

int ProgramParameters::is_switch( char *switch_name, int min_chars )
{
   if( parm[0] != '-' )  return 0 ;

   int parm_len = strlen( parm_switch ) ;
   if( parm_len < min_chars )  return 0 ;

   int switch_len = strlen( switch_name ) ;
   if( parm_len > switch_len )   return 0 ;

   if( memcmp( switch_name, parm_switch, parm_len )  == 0 )  return 1 ;
   return 0 ;
}

static char name_buf[256] ;

char *generate_index_name( Index4 index )
{
   u4namePiece( name_buf, sizeof(name_buf), index.fileName(),0,0);
   return name_buf ;
}

char *generate_dbf_name()
{
   u4namePiece( name_buf, sizeof(name_buf), db.fileName(),0,0);
   return name_buf ;
}

char *generate_case_name( const char *p )
{
   u4ncpy( name_buf, p, sizeof(name_buf) ) ;
   switch( current.case_names )
   {
      case 0: // Upper
         c4upper( name_buf ) ;
         break ;
      case 1: // Lower
         c4lower( name_buf ) ;
         break ;
      case 2: // Mixed
         char buf[2] ;
         buf[0] =  p[0] ;
         buf[1] = 0 ;
         c4upper(buf) ;
         c4lower( name_buf ) ;
         name_buf[0] = buf[0] ;
         break ;
   }
   return name_buf ;
}

int generate_code()
{
   if( !db.isValid() )
   {
      cb.error( e4result,0, "No data file specified." ) ;
      return -1 ;
   }

   if( !header.file.isValid() )
      if( header.open() < 0 )  return -1 ;
   if( !source.file.isValid() )
   {
      if( source.open() < 0 )  return -1 ;

      source << "#include \"d4all.hpp\"\r\n"
         << "#include \""
         << header.file.fileName()
         << "\"\r\n\r\n" ;
   }

   // Create Header
   header << "class " << current.class_name << " : public " << current.class_name_derive << "\r\n"
        "{\r\n"
        "public:\r\n"
        "   open( Code4& ) ;\r\n\r\n" ;

   if( current.create_on_off )
   {
      source << "static FIELD4INFO " << db.alias() << "_field_info[] =\r\n{\r\n" ;
      for( short j_field = 1; j_field <= db.numFields(); j_field++ )
      {
         Field4 field( db, j_field ) ;
         Str4char fType((char)field.type());

         source << "   { \"" << field.name() << "\", '"
            << fType << "', "
            << field.len() << ", "
            << field.decimals() << " },\r\n" ;
      }
      source << "   { 0,0,0,0 }\r\n} ;\r\n\r\n" ;

      Index4 index_on ;
      Tag4 tag ;
      for( tag.initFirst(db); tag.isValid(); tag.initNext() )
      {
         if( tag.tag->index != index_on.index )
         {
            if( index_on.isValid() )
            {
               source << "   { 0,0,0,0,0 }\r\n"
                  << "} ;\r\n\r\n" ; // Finish previous tag structure
            }
            source << "static TAG4INFO "
               << generate_index_name( (Index4)tag.tag->index )
               << "_tag_info[] =\r\n{\r\n" ;
         }
         Str4large tag_name_buf((char *) tag.expr() ) ;
         tag_name_buf.trim() ;
         source << "   { \"" << tag.alias() << "\", \""
            << tag_name_buf << "\", " ;
         if( tag.filter())
         {
            source << "\"" << tag.filter() << "\"" ;
         }
         else
            source << "0" ;
         source << ", " ;
         switch( tag.unique() )
         {
            case r4unique:
               source << "r4unique, " ;
               break ;
            case r4uniqueContinue:
               source << "r4uniqueContinue, " ;
               break ;
            case e4unique:
               source << "e4unique, " ;
               break ;
            case 0:
               source << "0, " ;
               break ;
         }
         if( tag.descending() )
            source << "r4descending" ;
         else
            source << "0" ;
         source << " },\r\n" ;

         index_on = (Index4 )tag.tag->index ;
      }
      if( index_on.isValid() )
         source << "   { 0,0,0,0,0 }\r\n"
            << "} ;\r\n\r\n" ; // Finish previous tag structure
   }

   source << "int " << current.class_name << "::open( Code4& cb )\r\n"
        "{\r\n" ;
   if( current.create_on_off )
   {
      source << "   int save_autoOpen = cb.autoOpen ;\r\n"
      "   int save_safety =  cb.safety ;\r\n"
      "\r\n"
      "   int rc_open_data = 0, rc_open_index = 0, rc_create_index = 0 ;\r\n"
      "\r\n"
      "   cb.autoOpen = 0 ;\r\n"
      "\r\n"
      "   rc_open_data = Data4::open( cb, \"" << generate_dbf_name() << "\" ) ;\r\n"
      "   if( rc_open_data != 0 )\r\n"
      "   {\r\n"
      "      cb.safety = 1 ;\r\n"
      "      cb.errorCode = 0 ;\r\n"
      "      create( cb, \"" << generate_dbf_name() << "\" , " << db.alias() << "_field_info ) ;\r\n"
      "   }\r\n"
      "\r\n" ;
      Tag4 tag_on ;
      Index4 index_on ;
      for( tag_on.initFirst( db ); tag_on.isValid(); tag_on.initNext() )
      {
         if( tag_on.tag->index != index_on.index )
         {
            if( !index_on.isValid() )
               // This code is generated only once and only if there is an index
               source << "   cb.safety = 0 ;\r\n"
                  "   Index4 index ;\r\n" ;

            index_on = (Index4 )tag_on.tag->index ;
            source << "   if( rc_open_data == 0 )\r\n"
               "      rc_open_index = index.open( *this, \""
               << generate_index_name(index_on) << "\" ) ;\r\n"
               "   if( isValid() && rc_create_index == 0 && \r\n"
               "      (rc_open_data != 0 || rc_open_index != 0) )\r\n"
               "   {\r\n"
               "      cb.errorCode = 0 ;\r\n"
               "      rc_create_index = index.create( *this, " ;
            //if( index_on.isProduction() )
            #ifndef N4OTHER
            if(index4isProduction(index_on.index->indexFile))
            #else
            if(0)
            #endif
               source << "0" ;
            else
               source << "\"" << generate_index_name( index_on ) << "\"" ;
            source << ", " << generate_index_name( index_on )
               << "_tag_info ) ;\r\n"
               "   }\r\n"
               "\r\n" ;
         }
         index_on = (Index4 )tag_on.tag->index ;
      }

      source << "   cb.safety = save_safety ;\r\n"
      "   cb.autoOpen = save_autoOpen ;\r\n"
      "\r\n"
      "   if( cb.errorCode != 0 )\r\n"
      "   {\r\n"
      "      if( isValid() )\r\n"
      "         close() ;\r\n"
      "      return cb.errorCode ;\r\n"
      "   }\r\n"
      "\r\n" ;
   }
   else
   {
      // Create is off
      source << "   int save_autoOpen = cb.autoOpen ;\r\n"
      "   cb.autoOpen = 1 ;\r\n"
      "  \r\n"
      "   int rc = Data4::open( cb, \"" << generate_dbf_name() << "\" ) ;\r\n"
      "\r\n" ;

      Tag4 tag_on ;
      Index4 index_on ;
      int is_first = 1 ;
      for( tag_on.initFirst( db ); tag_on.isValid(); tag_on.initNext() )
      {
         if( memcmp( index_on.index, tag_on.tag->index, sizeof(INDEX4)) != 0 )
         {
            index_on = (Index4 )tag_on.tag->index ;
            //if( ! index_on.isProduction() )
            #ifndef N4OTHER
            if(!index4isProduction(index_on.index->indexFile))
            #else
            if(1)
            #endif
            {
               if( is_first )
               {
                  // This code is generated only once and only if there is an index
                  source << "   Index4 index ;\r\n" ;
                     is_first = 0 ;
               }

               source << "   if( rc == 0 )\r\n"
                  "      rc = index.open( *this, \"" << generate_index_name(index_on) << "\" ) ;\r\n"
                  << "\r\n" ;
         }
      }
      index_on = (Index4 )tag_on.tag->index ;
   }

   source << "   cb.autoOpen = save_autoOpen ;\r\n"
      "\r\n"
      "   if( rc != 0 )\r\n"
      "   {\r\n"
      "      if( isValid() )\r\n"
      "         close() ;\r\n"
      "      return rc ;\r\n"
      "   }\r\n"
      "\r\n" ;
   }

   for( short j_field = 1; j_field <= db.numFields(); j_field++ )
   {
      Field4 field( db, j_field ) ;
      char *case_ptr ;

      if( field.type() == 'M' )
         header << "   Field4memo " ;
      else
         header << "   Field4 " ;
         header << current.field_id_prefix
            << (case_ptr = generate_case_name((char *) field.name() )) << " ;\r\n" ;

      source << "   " << current.field_id_prefix << case_ptr
         << ".init( *this, \"" << field.name() << "\" ) ;\r\n" ;
   }
   source << "\r\n" ;

   Tag4 tag ;
   for( tag.initFirst(db); tag.isValid(); tag.initNext() )
   {
      char *case_ptr ;
      header << "   Tag4 " << current.tag_id_prefix
        << (case_ptr = generate_case_name( tag.alias())) << " ;\r\n" ;

      source << "   " << current.tag_id_prefix << case_ptr << ".init( *this, \""
        << tag.alias() << "\" ) ;\r\n" ;
   }

   header << "} ;\r\n\r\n" ;
   source << "   return 0 ;\r\n"
      "}\r\n\r\n" ;

   return cb.errorCode ;
}
