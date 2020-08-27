using System;
using CodeBase;
 
namespace c4ap
{
   class ex105
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 oneDbf = new Data4( ) ;
         Field4info fields = new Field4info( ref cb ) ;
         cb.safety = 0 ;
 
         fields.add( "NAME",  'C', 20, 0, 0 ) ;
         fields.add( "AGE",   'N', 3, 0, 0 ) ;
         fields.add( "BDATE", 'D', 8, 0, 0 ) ;
 
         oneDbf.create( ref cb, "EX105_1", ref fields ) ;
 
         if( cb.errorCode == 0 )
            Console.WriteLine( "Created successfully\n" ) ;
         else
            Console.WriteLine( "An error occurred\n" ) ;    
 
         fields.free( ) ;
         cb.initUndo( ) ;
      }
   }
}