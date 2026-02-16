using System;
using CodeBase;

namespace c4ap
{
	class REUSE
	{
		[STAThread]
		static void Main(string[] args)
		{
			Code4 codeBase = new Code4();
			Data4 dataFile = new Data4();
			Field4info fieldInfo = new Field4info( ref codeBase ) ;
			
			fieldInfo.add( "FIELD1", 'C', 10, 0, 0 ) ;

			codeBase.safety = 0 ;
			
			dataFile.create(ref codeBase, "recycle", ref fieldInfo ) ;
			codeBase.exitTest( ) ;

			Field4 field = new Field4(dataFile, 1);

			dataFile.appendStart();
			field.assign("One");
			dataFile.append();

			dataFile.appendStart();
			field.assign("Two");
			dataFile.append();

			dataFile.appendStart();
			field.assign("Three");
			dataFile.append();

			MessageBox.Show("There are " + dataFile.recCount() + " records", "Information");
			
			dataFile.go(2);
			dataFile.deleteRec();
			dataFile.flush();

			dataFile.appendStart();
			field.assign("Four");
			dataFile.append();

			MessageBox.Show("There are " + dataFile.recCount() + " records", "Information");

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}