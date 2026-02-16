using System;
using CodeBase;
 
namespace c4ap
{
   class ex137
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( ) ;
         Index4 index = new Index4( );
 
         Field4info fieldInfo = new Field4info(ref cb ) ;
         fieldInfo.add("FIELD_NAME", 'C', 10, 0, 0 ) ;
         fieldInfo.add( "VALUE", 'N', 7, 2, 0) ;
         
         Tag4info tagInfo = new Tag4info( cb ) ;
         tagInfo.add( "T_NAME", "FIELD_NAME", "FIELD_NAME > 'A'", 0, 0 ) ;
         tagInfo.add( "NAME_TWO", "VALUE", "", Code4.e4unique, Code4.r4descending ) ;
 
         cb.safety = 0 ;
         data.create( ref cb, "DB_NAME", ref fieldInfo ) ;
         if ( data.isValid( ) != 0 )
            index.create( ref data, "name", ref tagInfo ) ;
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}