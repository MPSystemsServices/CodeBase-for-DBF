using System;
using CodeBase;
 
namespace c4ap
{
   class ex187
   {
      static int searchAll( Data4 d, string myValue )
      {
         Tag4 origTag = new Tag4( ) ;
         Tag4 tag = new Tag4( ) ;
         origTag.initSelected( d ) ;  // Save the current tag
         int origRecNo = d.recNo( ) ;
 
         for( tag.initLast( d ) ; tag.isValid( ) != 0; tag.initPrev( ) )
         {
            d.select( tag ) ;
            if( d.seek( myValue ) == 0 )
            {
               d.select( origTag ) ;
               return d.recNo( ) ;
            }
         }
         d.select( origTag ) ;
         d.go( origRecNo ) ;
         return -1 ;
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "INFO" ) ;
         data.top( ) ;
         int rc = searchAll( data, "Abbot" ) ;
         for( short j = 1; j < data.numFields( ); j++ )
            Console.Write( new Field4memo( data, j).str( ) ) ;
 
         Console.WriteLine( "\n{0} is the record number", rc ) ;
         cb.initUndo( ) ;
      }
   }
}