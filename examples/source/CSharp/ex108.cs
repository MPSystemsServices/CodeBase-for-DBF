using System;
using CodeBase;
 
namespace c4ap
{
   class ex108
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Field4info fields = new Field4info( ref cb ) ; // initialize with no fields
 
         fields.add( "NAME", 'C', 20, 0, 0 ) ;
         fields.add( "AGE",  'N',  3, 0, 0 ) ;
         fields.add( "REGISTER", 'L', 1, 0, 0 ) ;
         fields.add( "birth", 'D', 8, 0, 0 ) ;
 
         Data4 data = new Data4( ) ;
         cb.safety = 0 ; // overwrite existing DATAFILE.DBF
 
         data.create( ref cb, "DATAFILE", ref fields ) ;
         if( data.isValid( ) == 1 && data.numFields( ) == 4 )
            Console.WriteLine( "CREATED SUCCESSFULLY" ) ;
 
         fields.free( ) ;
         cb.initUndo( ) ;
      }
   }
}