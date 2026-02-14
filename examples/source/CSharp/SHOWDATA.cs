using System;
using CodeBase;

namespace c4ap
{
	class SHOWDATA
	{
		[STAThread]
		static int Main(string[] args)
		{
			if (args.Length != 1)
			{
				Console.WriteLine( "USAGE: SHOWDATA <FILENAME.DBF>" ) ;
			}
			else
			{
				Code4 codeBase  = new Code4() ;
				Data4 dataFile = new Data4( codeBase, args.GetValue(0).ToString() ) ;
				codeBase.exitTest( ) ;

				int numFields = dataFile.numFields( ) ;
				for( int rc = dataFile.top( ); rc == Code4.r4success ; rc = dataFile.skip( ))
				{
					for( short j = 1; j <= numFields; j++ )
					{
						Field4memo field = new Field4memo( dataFile, j ) ;
						Console.WriteLine( " {0}", field.str( ) ) ;
					}
					Console.Write( "\n" ) ;
				}

				dataFile.close( ) ;
				codeBase.initUndo( ) ;
			}

			return 0 ;
		}
	}
}