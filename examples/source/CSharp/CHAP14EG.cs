using System;
using CodeBase;

namespace c4ap
{
	class CHAP14EG
	{
		static int openFileEx( ref Code4 codeBase, Data4 data, string name)
		{
			short oldAccessMode = codeBase.accessMode ;
			codeBase.accessMode = Code4.OPEN4DENY_RW ;

			data.open( ref codeBase, name ) ;

			codeBase.accessMode = oldAccessMode ;

			return data.isValid( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			Code4 cb = new Code4() ;
			Data4 db = new Data4() ;
			openFileEx( ref cb, db, "INFO" ) ;
			cb.closeAll( ) ;
			cb.initUndo( ) ;
		}
	}
}