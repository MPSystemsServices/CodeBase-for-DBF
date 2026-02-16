using System;
using CodeBase;

namespace c4ap
{
   class ex69
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "INFO" ) ;
         Tag4 nameTag = new Tag4( data, "INF_NAME" ) ;
         Field4 name = new Field4( data, "NAME" ) ;
 
         cb.exitTest( ) ; // check for errors
         data.lockAll( ) ;
         data.select( nameTag ) ;
 
         for( int rc = data.seek( "JONES" ) ; rc == 0 ; rc = data.skip( ) )
         {
            if( name.str( ).Trim( ) == "JONES" )
               Console.WriteLine( "Jones in record {0}\n", data.recNo( ) ) ;
            else
               break ;
         }
         data.unlock( ) ;
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}