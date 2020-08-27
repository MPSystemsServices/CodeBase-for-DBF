using System;
using CodeBase;

namespace c4ap
{
   class ex96
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA" ) ;
         Data4 info = new Data4( cb, "INFO" ) ;
         Expr4 expr = new Expr4( ) ;
 
         expr.parse( data, "FNAME+' '+DTOS( INFO->BIRTH_DATE)" ) ;
 
         data.top( ) ; 
         info.top( ) ;
    
         Console.WriteLine( "First name from DATA and birth date from INFO: {0}\n", expr.str() );
 
         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}