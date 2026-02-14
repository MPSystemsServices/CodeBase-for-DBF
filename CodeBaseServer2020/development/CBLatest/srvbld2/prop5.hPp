/* FILE prop5.hpp (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.  */

#ifndef _PROPERTIES5_HPP_
#define _PROPERTIES5_HPP_

#ifndef S4SERVER

enum ROWTYPE{row5none, row5index, row5schema, row5table, row5integrated} ;
typedef enum ROWTYPE enum5type;

/* CJ - These value represent the location in the array of Rowset properties */
/* If properties are removed or add to the middle of the Rowset properties */
/* table these values must be changed to relfect the changes*/
#define numCDC 8        //Cache Deferred Column
#define numFB 9         //Fetch Backwards
#define numSB 11        //Scroll Backwards
#define numDC 17        //Deferred Column
#define numIRC 40       //IRowsetChange
#define numICI 41       //IRowsetCurrentIndex
#define numIRI 42       //IRowsetIdentity
#define numIRIX 43      //IRowsetIndex
#define numIRL 46       //IRowsetLocate
#define numIRS 49       //IRowsetScroll
#define numIRU 50       //IRowsetUpdate
#define numLRI 59       //Literal Row Identity
#ifdef S4CONFORMANCE
   #define numRDR 72    //Remove Deleted Rows
   #define numUDA 80    //Updatability
#else
   #define numRDR 70
   #define numUDA 78
#endif

/*CJ - location of the initialization properties */
#define numINITMODE 2          //Mode
#define numINITLOCATION 4      //Location
#define numTIMEOUT 7

#define numCODEPAGE 1          //CodeBase:CodePage
#define numCOLSEQ 2            //CodeBase:Collating Sequence
#define numCOLSEQU 3           //CodeBase:Collating Sequence Unicode
#define numOPTREAD 4           //CodeBase:Optimize Read
#define numOPTWRITE 5          //CodeBase:Optimize Write
#define numMEMBFSZ 6           //CodeBase:Optimize Buffer Size
#define numLOGALW 7            //CodeBase:Log Always
#define numADOGS  8            //CodeBase:ADO Extended Compatibility
#define numDEFUNQ 9            //CodeBase:Error Default Unique
#define numLA     10           //CodeBase:Lock Attempts
#define numADODI  11           //CodeBase:ADO Use Default Index

/* CJ - location of the data source information properties */
#define numDBSN 5     //DBMS Name (provider name)
#define numMIS 8      //Maximum Index Size

#define numPIU 7     //Index Unique property.

#define FLAGS_DBINITRW         (DBPROPFLAGS_DBINIT         | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )
#define FLAGS_DBINITRO         (DBPROPFLAGS_DBINIT         | DBPROPFLAGS_READ )
#define FLAGS_DATASOURCERW     (DBPROPFLAGS_DATASOURCE     | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )
#define FLAGS_DATASOURCERO     (DBPROPFLAGS_DATASOURCE     | DBPROPFLAGS_READ )
#define FLAGS_DATASOURCEINFORO (DBPROPFLAGS_DATASOURCEINFO | DBPROPFLAGS_READ )
#define FLAGS_SESSIONRW        (DBPROPFLAGS_SESSION        | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )
#define FLAGS_SESSIONRO        (DBPROPFLAGS_SESSION        | DBPROPFLAGS_READ )
#define FLAGS_ROWSETRO         (DBPROPFLAGS_ROWSET         | DBPROPFLAGS_READ )
#define FLAGS_ROWSETRW         (DBPROPFLAGS_ROWSET         | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )
#define FLAGS_ROWSETROC        (DBPROPFLAGS_ROWSET         | DBPROPFLAGS_READ | DBPROPFLAGS_COLUMNOK )
#define FLAGS_ROWSETRWC        (DBPROPFLAGS_ROWSET         | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE | DBPROPFLAGS_COLUMNOK )
#define FLAGS_INDEXRO          (DBPROPFLAGS_INDEX          | DBPROPFLAGS_READ )
#define FLAGS_INDEXRW          (DBPROPFLAGS_INDEX          | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )
#define FLAGS_COLUMNRO         (DBPROPFLAGS_COLUMN         | DBPROPFLAGS_READ )
#define FLAGS_COLUMNRW         (DBPROPFLAGS_COLUMN         | DBPROPFLAGS_READ | DBPROPFLAGS_WRITE )

class5 Rowset5all;
class5 Rowset5;
class5 Rowset5index;
class5 Rowset5integratedIndex;
class5 Source5;
class Property5sets ;
extern Property5sets property5sets ;

// DBPROPSET_SOURCE5 property set GUID
// {B8CFE2C0-7BC2-11d0-B95D-0020AF312DC7}
DEFINE_GUID(DBPROPSET_SOURCE5, 0xb8cfe2c0,0x7bc2,0x11d0,0xb9,0x5d,0x0,0x20,0xaf,0x31,0x2d,0xc7) ;

class S5CLASS Property5info
{
public:
   // Correspond to DBPROPINFO members
   LPOLESTR    description ;
   DBPROPID    propertyID ;
   DBPROPFLAGS flags ;
   VARTYPE     valueType ;

   // Default values
   LONG        valueDefaultLong ; // Long or boolean initialization value
   LPOLESTR    valueDefaultPtr ; // String Initialization Value
   VARIANT     valueDefault ; // Initialized at run time

   // This next one is initialized at run time.
   VARIANT     valuesSupported ;
} ;

class S5CLASS Property5set
{
protected:
   GUID           propertySet ;
   Property5info* info ;
   SHORT          nInfo ;
   ULONG          totalDescriptionSize ;
   ULONG          largestDescriptionSize ;

public:
   Property5set(GUID propertySetIn) ;
   virtual ~Property5set() ;
   void init( Property5info *infoIn ) ;

   GUID getPropertySet() { return propertySet; }
   SHORT getPropNum() { return nInfo; }
   DBPROPID getPropId(SHORT iProp) {return info[iProp].propertyID;}
   VARIANT* getPropDefault(SHORT iProp) ;
   DBPROPFLAGS getPropFlags(SHORT iProp) ;
   VARTYPE getPropType(SHORT iProp) ;
   SHORT getPropIndex(DBPROPID property) ;
   inline ULONG getTotalSize() { return( totalDescriptionSize ) ; }
   inline ULONG getLargestSize() { return( largestDescriptionSize ) ; }
   virtual HRESULT checkValue(SHORT iProp, VARIANT *value,enum5type rowtype, Source5 *pSource ) { return S_OK; }
   virtual void changeDefaultFlags(ULONG initMode) { return ; }

   HRESULT fillInDBPROPINFOSET( const DBPROPIDSET *idSet, DBPROPINFOSET *infoSet, WCHAR** ppDescBuffer, ULONG* cDescBuffer, ULONG totalAllocated ) ;
   static HRESULT fillEmptyDBPROPINFOSET( const DBPROPIDSET *idSet, DBPROPINFOSET *infoSet) ;
} ;

class S5CLASS Property5setDBInit : public Property5set
{
public:
   // Special supported initializer code for when there are a list of possible
   // values for the properties.
   Property5setDBInit() ;
   ~Property5setDBInit() ;
   void initSup( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value, enum5type rowtype, Source5 *pSource ) ;
} ;


class S5CLASS Property5setDataSourceInfo : public Property5set
{
public:

   Property5setDataSourceInfo() ;
   ~Property5setDataSourceInfo() ;
   void initSup( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value ) ;
} ;


class S5CLASS Property5setRowset : public Property5set
{
public:
   // Special supported initializer code for when there are a list of possible
   // values for the properties.
   Property5setRowset() ;
   ~Property5setRowset() ;
   void initSup( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value, enum5type rowtype, Source5 *pSource ) ;
   virtual void changeDefaultFlags(long initMode) ;
} ;

class S5CLASS Property5setSession : public Property5set
{
public:
   // Special supported initializer code for when there are a list of possible
   // values for the properties.
   Property5setSession() ;
   ~Property5setSession() ;
   void init( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value, enum5type rowtype, Source5 *pSsource ) ;
} ;

class S5CLASS Property5setSource5 : public Property5set
{
public:
   // Special supported initializer code for when there are a list of possible
   // values for the properties.
   Property5setSource5() ;
   ~Property5setSource5() ;
   void init( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value, enum5type rowtype, Source5 *pSource ) ;
} ;

class S5CLASS Property5setColumn : public Property5set
{
public:
   Property5setColumn() ;
   ~Property5setColumn() ;
   void init( Property5info *infoIn ) ;

   virtual HRESULT checkValue(SHORT iProp, VARIANT *value, enum5type rowtype, Source5 *pSource ) ;
} ;

// Generic property set code and data for each property set
class S5CLASS Property5sets
{
private:
   static const SHORT iDataSource ;
   static const SHORT iDataSourceInfo ;
   static const SHORT iIndex ;
   static const SHORT iInitialization ;
   static const SHORT iColumn ;
   static const SHORT iRowset ;
   static const SHORT iSession ;
   static const SHORT iSource5 ;
   static const SHORT nProperty5set ;

protected:
   Property5set *sets[8] ;

public:
   Property5sets() ;
   ~Property5sets() ;

   Property5set* dataSource() { return sets[iDataSource]; }
   Property5set* dataSourceInfo() { return sets[iDataSourceInfo]; }
   Property5set* index() { return sets[iIndex]; }
   Property5set* initialization() { return sets[iInitialization]; }
   Property5set* column() { return ( sets[iColumn] ) ; }
   Property5set* rowset() { return sets[iRowset]; }
   //Property5set* session() { return sets[iSession]; }
   Property5set* source5() { return sets[iSource5]; }
   SHORT nSets() { return nProperty5set ; }
   Property5set* getPropSet(SHORT iSet) ;
   SHORT lookUpPropSet(GUID propertySet) ;
   short IsGUIDbad(GUID inCheck);

   DBPROPINFOSET *allocateDBPROPINFOSETarray( ULONG nItems ) ;
   void cleanupDBPROPINFOSETarray(ULONG nPropSet, DBPROPINFOSET **prgInfoSets, OLECHAR **ppDescBuffer ) ;
   HRESULT getPropertyInfo( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcInfoSets, DBPROPINFOSET **prgInfoSets, OLECHAR **ppDescBuffer, BOOL sourceInitialized ) ;
   ULONG calculateBufferSize( ULONG cIDSets, const DBPROPIDSET rgIDSets[], BOOL sourceInitialized ) ;

   DBPROPSET *allocateDBPROPSETarray( ULONG nItems ) ;
   void cleanupDBPROPSETarray(ULONG nPropSet, DBPROPSET **prgPropSets ) ;
} ;


#ifndef S4SERVER
class Schema5rowset ;
#endif

class S5CLASS Property5values
{
protected:
   Property5set *set ;
   DBPROPFLAGS *flags ;
   VARIANT *values ;
   SHORT nValues ;

public:
   enum5type type;

   Property5values() ;
   ~Property5values() ;
   void init( Property5set *set) ;

   HRESULT fillInDBPROPSET( const DBPROPIDSET *idSet, DBPROPSET *propSet ) ;
   static HRESULT fillEmptyDBPROPSET( const DBPROPIDSET *idSet, DBPROPSET *propSet) ;
   HRESULT fillEmptyDBPROPSETns( const DBPROPIDSET *idSet, DBPROPSET *propSet) ;
   HRESULT setDBPROPSET( DBPROPSET *propSet ) ;
   static HRESULT setEmptyDBPROPSET( DBPROPSET *propSet) ;
   HRESULT setEmptyDBPROPSETns( DBPROPSET *propSet) ;
   inline VARIANT *getGENERIC(int x) {return &(values[x]);}
   void setGENERIC( int x, VARIANT *newValue ) ;

   virtual HRESULT checkCOLID(DBID consumer, short iProp, short *columnType, short *columnNum, DBPROPFLAGS propFlags) ;
   virtual HRESULT checkCONFLICTING( short iProp, VARIANT *value ) { return( S_OK ) ; }
   virtual int isColumnEqual(short iProp, short columnNum, VARIANT *value) { return 0 ; }
   virtual HRESULT setColumn(SHORT iProp, short colID, VARIANT *newValue) { return E_FAIL; }
   virtual HRESULT setAllColumns(SHORT iProp, VARIANT *newValue) { return E_FAIL; }
   virtual HRESULT internalConsistancy(short iProp) {return S_OK;}
   virtual Source5 *getSource5Ptr() { return (0); }

   void setIndexDefault();
   void setIntegratedDefault( Rowset5all *rowset ) ;
   void setTableDefault( Rowset5all *rowset );
   void setSchemaDefault( Schema5rowset *rowset );
   void setSchemaDefault();
   void setColumnDefault(short member, DBPROPFLAGS flag, Bool5 value ) ;
} ;

class S5CLASS Property5valuesDBInit : public Property5values
{
public:
   inline void init() { Property5values::init(property5sets.initialization()); }
   // access methods
   inline OLECHAR *getAUTH_USERID() { return V_BSTR(&values[0]); }
   inline static SHORT iGetINIT_MODE() { return 2; }
   inline OLECHAR *getPASSWORD() {return V_BSTR(&values[1]);}
   LONG getINIT_MODE();
   inline HWND getHWND() { return (HWND)V_I4(&values[3]); }
   inline OLECHAR *getLOCATION() { return V_BSTR(&values[numINITLOCATION]) ;}
   inline OLECHAR *getStampInfo() { return V_BSTR(&values[5]) ;}
   inline LONG getTIMEOUT() { return( V_I4(&values[numTIMEOUT]) ); }
} ;

class S5CLASS Property5valuesDataSourceInfo : public Property5values
{
public:
   inline void init() { Property5values::init(property5sets.dataSourceInfo()); }
   // access methods
   inline void setDATASOURCEREADONLY(BOOL valueIn) { V_BOOL(&values[0]) = valueIn; }
   inline void setUSERNAME(OLECHAR *valueIn)
   {
//      VariantClear(&values[1]) ;
      VariantInit(&values[1]) ;
      V_VT(&values[1])=VT_BSTR;
      V_BSTR(&values[1]) = SysAllocString(valueIn);
   }
   inline void freeUSERNAME()
   {
      VariantClear( &values[1] ) ;
//      BSTR *savePtr
//      savePtr = V_BSTR(&values[1]) ;
//      VariantClear( values[1] ) ;
//      SysFreeString(savePtr);
   }
} ;

class S5CLASS Property5valuesSource5 : public Property5values
{
public:
   inline void init() { Property5values::init(property5sets.source5()); }

   // access methods
   inline OLECHAR *getLogName() { return (V_BSTR(&values[0])) ; }
   inline int getLogging() { return( V_BOOL(&values[numLOGALW]) ? LOG4ALWAYS : LOG4TRANS ) ; }
   inline long getCodePage() { return ( V_I4(&values[numCODEPAGE]) ) ; }
   inline OLECHAR *getColSeq() { return(V_BSTR(&values[numCOLSEQ])); }
   inline BOOL getOptRead() { return(V_BOOL(&values[numOPTREAD])) ; }
   inline BOOL getOptWrite() { return( V_BOOL(&values[numOPTWRITE]) ) ; }
   inline BOOL getADOGrid() { return( V_BOOL(&values[numADOGS]) ) ; }
   inline long getOptBufSize()  { return ( V_I4(&values[numMEMBFSZ]) ) ; }
   inline OLECHAR *getColSeqU() { return(V_BSTR(&values[numCOLSEQU])); }
   inline int getDefUnquie() { return(V_I4(&values[numDEFUNQ])) ; }
   inline int getLockAtt() { return(V_I4(&values[numLA])) ; }
   inline BOOL getADODefInd() { return(V_BOOL(&values[numADODI])) ; }

} ;

/*class S5CLASS Property5valuesSession : public Property5values
{
public:
   inline void init() { Property5values::init(property5sets.session()); }
   // access methods
   inline static SHORT iGetSESS_AUTOCOMMITISOLEVELS() { return 0; }
   inline LONG getSESS_AUTOCOMMITISOLEVELS() { return V_I4(&values[0]); }
} ; */

class S5CLASS Property5valuesRowset : public Property5values
{
protected:
   static const SHORT nColumnProperties ;
   VARIANT (* valueColumns)[2];
   SHORT nColumns ;
   Rowset5all *pRowset;

public:
   Property5valuesRowset() ;
   ~Property5valuesRowset() ;
   void init( SHORT nCols, Rowset5all *inRowset) ;
   void setMemoDefered( void ) ; /* Set all memo field rowset column properties to be defered and Cachedefered*/

   virtual HRESULT checkCOLID(DBID consumer, short iProp, short *columnType, short *columnNum, DBPROPFLAGS propFlags) ;
   virtual HRESULT checkCONFLICTING( short iProp, VARIANT *value ) ;
   virtual int isColumnEqual(short iProp, short columnNum, VARIANT *value) ;
   virtual HRESULT setColumn(SHORT iProp, short colID, VARIANT *newValue) ;
   virtual HRESULT setAllColumns(SHORT iProp, VARIANT *newValue) ;
   virtual HRESULT internalConsistancy(short iProp);
   virtual Source5 *getSource5Ptr() ;
   HRESULT fillInDBPROPSET( const DBPROPIDSET *idSet, DBPROPSET *propSet ) ;

   // access methods
   inline BOOL getDEFERRED() { return V_BOOL(&values[0]); }
   inline BOOL getDEFERRED(SHORT colID) { if (colID >= nColumns || colID < 0) return getDEFERRED(); else return V_BOOL(&valueColumns[colID][0]); }
   inline LONG getCMMDTIMEOUT() {return V_I4(&values[13]); }
   inline BOOL getIRC() { return(V_BOOL(&values[numIRC])? TRUE :  FALSE); }
   inline BOOL getICI() { return ( V_BOOL(&values[numICI])? TRUE : FALSE ) ; }
   inline BOOL getIRI() { return V_BOOL(&values[numIRI])? TRUE :  FALSE; }
   inline BOOL getIRL() { return V_BOOL(&values[numIRL])? TRUE :  FALSE; }
   inline BOOL getIRS() { return V_BOOL(&values[numIRS])? TRUE :  FALSE; }
   inline BOOL getIRU() { return V_BOOL(&values[numIRU])? TRUE :  FALSE; }
   inline BOOL getREMDELM() {return V_BOOL(&values[numRDR])? TRUE :  FALSE; }
   inline LONG getUDA() { return( V_I4(&values[numUDA])) ; }
} ;

class S5CLASS Source5properties
{
protected:
   BOOL sourceInitialized ;

public:
   Property5valuesDBInit initialization ;
   Property5valuesDataSourceInfo dataSourceInfo ;
   Property5values dataSource;
   Property5valuesSource5 source5;

   Source5properties() ;
   void init(); // ?? Why two initialization functions.
   void initialize() ;
   void uninitialize();
   HRESULT getPropertyInfo( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcInfoSets, DBPROPINFOSET **prgInfoSets, OLECHAR **ppDescBuffer) ;
   void getPropertyInfoModeAdjustment( ULONG *pcInfoSets, DBPROPINFOSET **prgInfoSets, ULONG mode ) ;
   HRESULT getProperties( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcPropSets, DBPROPSET **prgPropSets) ;
   HRESULT setProperties( ULONG cIDSets, DBPROPSET rgPropSets[]) ;
   inline OLECHAR *getAUTH_USERID() {return initialization.getAUTH_USERID();}
   inline OLECHAR *getPASSWORD() {return initialization.getPASSWORD();}
   inline LONG getINIT_MODE() { return initialization.getINIT_MODE(); }
   inline LONG getTIMEOUT() { return initialization.getTIMEOUT(); }
   inline HWND getHWND() { return initialization.getHWND(); }
   inline OLECHAR *getLOCATION() { return initialization.getLOCATION();}
   inline OLECHAR *getStampInfo() { return initialization.getStampInfo();}
   inline OLECHAR *getCURRCAT() { return V_BSTR(dataSource.getGENERIC(0));}
   #ifdef S4STAND_ALONE
      inline OLECHAR *getLogName() { return source5.getLogName(); }
   #endif
      inline int getLogging() { return source5.getLogging() ; }

   long getCP() ;
   short getCS() ; // CJ - 11/29/99 changed return code from function to support code4collation()
   short getCSU() ; // CJ - 11/29/99 addeed new function to support code4collationUnicode()
   inline int getOR() { return( source5.getOptRead() ) ; }
   inline int getOW() { return( source5.getOptWrite() ) ; }
   inline ULONG getMSB() { return ( source5.getOptBufSize() ) ; }
   inline BOOL getAGS() { return( source5.getADOGrid()); }
   inline int getEDU() { return( source5.getDefUnquie() ) ; }
   inline int getLA()  { return( source5.getLockAtt() ) ; }
   inline BOOL getADI()  { return( source5.getADODefInd() ) ; }
   HRESULT setCURRCAT(OLECHAR *value) ;
   #ifdef S4PROP_UNICODE
      inline BOOL getUNICODE() { return V_BOOL(source5.getGENERIC(0))? TRUE : FALSE; }
   #endif
} ;

class S5CLASS Column5properties
{
protected:
   //BOOL columnsInitialized ;
public:
   Column5properties() ;
   ~Column5properties() ;
   Property5values *columns ;

   /*Column5properties() ; */
   void init(const DBCOLUMNDESC *rgColumnDesc ) ;

   /*CJ -04/19/99- commented out these functions as they have no real use */
   /*void initialize()  {columnsInitialized=TRUE;}
   inline void uninitialize() { columnsInitialized = FALSE; }*/
   HRESULT setProperties( ULONG cIDSets, DBPROPSET rgPropSets[]);
   BOOL getCOL_NULLABLE() { return V_BOOL(columns->getGENERIC(2))? TRUE :  FALSE; }
   VARIANT_BOOL getCOL_FIXEDLEN() { return V_BOOL(columns->getGENERIC(0)); }
   VARIANT_BOOL getCOL_ISLONG() { return V_BOOL(columns->getGENERIC(1)); }
} ;

class S5CLASS Session5properties
{
protected:
public:
   //Property5valuesSession session ;

//      Session5properties() ;
   void init() ;

   HRESULT getProperties( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcPropSets, DBPROPSET **prgPropSets) ;
   HRESULT setProperties( ULONG cIDSets, DBPROPSET rgPropSets[]) ;
} ;


class S5CLASS Rowset5properties
{
protected:
public:
   Property5valuesRowset rowset ;

//      Rowset5properties() ;
   void init(short nColmns, Rowset5 *inRowset) ;
   void init(short nColmns, Schema5rowset *inRowset) ;

   HRESULT getProperties( ULONG cIDSets, const DBPROPSET rgIDSets[] ) {return E_FAIL ;} // Parameters used in Session5

   HRESULT getProperties( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcPropSets, DBPROPSET **prgPropSets) ;
   HRESULT setProperties( ULONG cIDSets, DBPROPSET rgPropSets[]) ;
   inline LONG getCMMDTIMEOUT() {return rowset.getCMMDTIMEOUT() ; }
   inline BOOL getIRC() { return rowset.getIRC() ; }
   inline BOOL getIRI() { return rowset.getIRI() ; }
   inline BOOL getIRL() { return rowset.getIRL() ; }
   inline BOOL getIRS() { return rowset.getIRS() ; }
   inline BOOL getIRU() { return rowset.getIRU() ; }
   inline BOOL getREMDELM() { return rowset.getREMDELM() ; }
   inline LONG getUDA() { return( rowset.getUDA() ) ; }
};

class S5CLASS Index5properties
{
protected:
public:
   Property5values index;
   Index5properties() { isInit = 0 ; }

   HRESULT setProperties(ULONG cIDSets, DBPROPSET rgPropSets[]) ;
   inline BOOL getPIU() { return V_BOOL(index.getGENERIC(numPIU));}
   void setPIU( BOOL value ) ;
   void init();

   Bool5 isInit;
};

class S5CLASS Rowset5indexProperties : public Rowset5properties, public Index5properties
{
public:

   void init(short nColmns, Rowset5index *inRowset) ;
   void init(short nColmns, Rowset5integratedIndex *inRowset) ;
   void initBoth(short nColmns, Rowset5all *inRowset ) ;

   HRESULT setProperties(ULONG cIDSets, DBPROPSET rgPropSets[]) ;
   HRESULT getPropertiesIndex( ULONG cIDSets, const DBPROPIDSET rgIDSets[], ULONG *pcPropSets, DBPROPSET **prgPropSets) ;

} ;


class S5CLASS Table5properties
{

public:
   Property5values table;


   void init() ;
   HRESULT setProperties( ULONG cIDSets, DBPROPSET rgPropSets[]) ;

} ;

#endif /* S4SERVER */

#endif /* _PROPERTIES5_HPP_ */
