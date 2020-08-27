using System;
using CodeBase;

namespace c4ap
{
	class COPYDAT2
	{
		[STAThread]
		static void Main(string[] args)
		{
			if(args.Length != 2)
			{
				Console.WriteLine( "USAGE: COPYDATA <FROM FILE> <TO FILE>" ) ;
			}
			else
			{
				Code4 codeBase = new Code4();
				codeBase.safety = 0;
				Data4 dataFile = new Data4( codeBase, args.GetValue(0).ToString( ) ) ;
				codeBase.exitTest( ) ;

				Field4info fields = new Field4info( dataFile ) ;  //copy the fields

				//obtain the Index4 object of the production index if one exists
				Index4 index = new Index4( ) ;
				index = dataFile.index( dataFile.alias ) ;

				Data4 dataCopy = new Data4();

				if( index.isValid( ) != 0 )
				{
					Tag4info tags = new Tag4info( index ) ;
					dataCopy.create( ref codeBase, args.GetValue(1).ToString( ), ref fields, ref tags ) ;
					tags.free( ) ;
				}
				else
					dataCopy.create( ref codeBase, args.GetValue(1).ToString( ), ref fields ) ;

				codeBase.closeAll( ) ;
				fields.free( ) ;
				codeBase.initUndo( ) ;
			}
		}
	}
}