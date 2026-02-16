using System;
using CodeBase;

namespace c4ap
{
	class COPYDATA
	{
		[STAThread]
		static void Main(string[] args)
		{
			if(args.Length != 2)
			{
				Console.WriteLine( "USAGE: COPYDATA <FROM FILE>  <TO FILE>" ) ;
			}
			else
			{
				Code4 codeBase = new Code4();
				Data4 dataFile = new Data4( codeBase, args.GetValue(0).ToString( ) ) ;
				Data4 dataCopy = new Data4();
				codeBase.exitTest( ) ;

				Field4info fields = new Field4info( dataFile ) ;
				codeBase.safety = 0;
				dataCopy.create( ref codeBase, args.GetValue(1).ToString( ), ref fields ) ;
				fields.free( ) ;

				codeBase.closeAll( ) ;
				codeBase.initUndo( ) ;
			}
		}
	}
}