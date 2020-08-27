using System;
using CodeBase;
 
namespace c4ap
{
   class ex136
   {
      static int addLotsOfRecords( Data4 d )
      {
         // get the secondary index file
         Index4 index = d.index( "INFO2" ) ;
         if( index.isValid( ) > 0 )
            index.close( ) ;
 
         d.top( ) ;
         for( int i = 200 ; i != 1 ; i -- )
         {
            // make 200 copies of record 1
            d.appendStart() ;
            d.append( ) ; 
         } 
 
         // open the index file and update it
         index.open( d, d.alias ) ;
         return index.reindex( ) ;
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.autoOpen = 0 ;
         Data4 data = new Data4( cb, "INFO" ) ;
         Index4 index = new Index4( ) ;
         index.open( data, "INFO2" ) ;
         addLotsOfRecords( data ) ;
         cb.initUndo( ) ;
      }
   }
}