using System;
using CodeBase;
 
namespace c4ap
{
   class ex106
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "INFO" ) ;
		 Data4 data2 = new Data4( ) ;
 
         cb.safety = 0 ;
         Field4info fields = new Field4info( data ) ;
         fields.add( "COMMENT", 'M', 8, 0, 0 ) ;
 
         data2.create( ref cb, "DATA2", ref fields ) ;
 
         if( cb.errorCode == 0 )
            Console.WriteLine( "Created successfully\n" ) ;
         fields.free( ) ;
         cb.initUndo( ) ;
      }
   }
}