// interfa5.hpp
// Defines OLE interfaces inclusion in other header files.

#define DECLARE5_IUnknown \
        STDMETHODIMP                    QueryInterface(REFIID, LPVOID *); \
        STDMETHODIMP_(ULONG)    AddRef(void); \
        STDMETHODIMP_(ULONG)    Release(void);

#define DECLARE5_IColumnsInfo  \
         HRESULT STDMETHODCALLTYPE GetColumnInfo(  \
            /* [out][in] */ ULONG __RPC_FAR *pcColumns, \
            /* [size_is][size_is][out] */ DBCOLUMNINFO __RPC_FAR *__RPC_FAR *prgInfo, \
            /* [out] */ OLECHAR __RPC_FAR *__RPC_FAR *ppStringsBuffer) ; \
         HRESULT STDMETHODCALLTYPE MapColumnIDs(  \
            /* [in] */ ULONG cColumnIDs, \
            /* [size_is][in] */ const DBID __RPC_FAR rgColumnIDs[  ], \
            /* [size_is][out] */ ULONG __RPC_FAR rgColumns[  ]) ;

#define DECLARE5_ISupportErrorInfo \
         HRESULT STDMETHODCALLTYPE InterfaceSupportsErrorInfo(/* [in] */ REFIID riid ) ;

#define DECLARE5_IConvertType  \
         HRESULT STDMETHODCALLTYPE CanConvert(  \
            /* [in] */ DBTYPE wFromType, \
            /* [in] */ DBTYPE wToType, \
            /* [in] */ DBCONVERTFLAGS dwConvertFlags) ;

#define DECLARE5_IRowsetIdentity  \
        HRESULT STDMETHODCALLTYPE __RPC_FAR IsSameRow(  \
            /* [in] */ HROW hThisRow,  \
            /* [in] */ HROW hThatRow);

#define DECLARE5_IRowsetInfo  \
         HRESULT STDMETHODCALLTYPE GetProperties(  \
            /* [in] */ const ULONG cPropertyIDSets, \
            /* [size_is][in] */ const DBPROPIDSET __RPC_FAR rgPropertyIDSets[  ], \
            /* [out][in] */ ULONG __RPC_FAR *pcPropertySets, \
            /* [size_is][size_is][out] */ DBPROPSET __RPC_FAR *__RPC_FAR *prgPropertySets) ; \
         HRESULT STDMETHODCALLTYPE GetReferencedRowset(  \
            /* [in] */ ULONG iOrdinal, \
            /* [in] */ REFIID riid, \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppReferencedRowset) ; \
         HRESULT STDMETHODCALLTYPE GetSpecification(  \
            /* [in] */ REFIID riid, \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppSpecification) ;

#define DECLARE5_IRowsetIndex  \
         HRESULT STDMETHODCALLTYPE GetIndexInfo(  \
            /* [out][in] */ ULONG __RPC_FAR *pcKeyColumns, \
            /* [size_is][size_is][out] */ DBINDEXCOLUMNDESC __RPC_FAR *__RPC_FAR *prgIndexColumnDesc, \
            /* [out][in] */ ULONG __RPC_FAR *pcIndexProperties, \
            /* [size_is][size_is][out] */ DBPROPSET __RPC_FAR *__RPC_FAR *prgIndexProperties) ; \
         HRESULT STDMETHODCALLTYPE Seek(  \
            /* [in] */ HACCESSOR hAccessor, \
            /* [in] */ ULONG cKeyValues, \
            /* [in] */ void __RPC_FAR *pData, \
            /* [in] */ DBSEEK dwSeekOptions) ; \
         HRESULT STDMETHODCALLTYPE SetRange(  \
            /* [in] */ HACCESSOR hAccessor, \
            /* [in] */ ULONG cStartKeyColumns, \
            /* [in] */ void __RPC_FAR *pStartData, \
            /* [in] */ ULONG cEndKeyColumns, \
            /* [in] */ void __RPC_FAR *pEndData, \
            /* [in] */ DBRANGE dwRangeOptions) ;

#define DECLARE5_IRowsetCurrentIndex \
        HRESULT STDMETHODCALLTYPE GetIndex(  \
            /* [out] */ DBID __RPC_FAR *__RPC_FAR *ppIndexID) ; \
        HRESULT STDMETHODCALLTYPE SetIndex(  \
            /* [in] */ DBID __RPC_FAR *pIndexID) ; \

#define DECLARE5_IGetDataSource \
      STDMETHODIMP GetDataSource( REFIID, IUnknown** );

#define DECLARE5_IOpenRowset \
        HRESULT STDMETHODCALLTYPE OpenRowset( \
            /* [in] */ IUnknown __RPC_FAR *pUnkOuter, \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ DBID __RPC_FAR *pIndexID, \
            /* [in] */ REFIID riid, \
            /* [in] */ ULONG cPropertySets, \
            /* [size_is][out][in] */ DBPROPSET __RPC_FAR rgPropertySets[  ], \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppRowset);

#define DECLARE5_ISessionProperties \
      STDMETHODIMP GetProperties(ULONG cPropertySets, const DBPROPIDSET rgPropertySets[], ULONG* pcProperties, DBPROPSET** prgProperties); \
      STDMETHODIMP SetProperties(ULONG cProperties, DBPROPSET rgProperties[]);

#define DECLARE5_IDBSchemaRowset \
      STDMETHODIMP GetRowset(IUnknown *punkOuter, REFGUID rguidSchema, ULONG cRestrictions, const VARIANT rgRestrictions[], REFIID riid, ULONG cPropertySets, DBPROPSET rgPropertySets[], IUnknown **ppRowset); \
      STDMETHODIMP GetSchemas(ULONG *pcSchemas, GUID **prgSchemas, ULONG** prgRestrictionSupport);

#define DECLARE5_ITransactionLocal \
      STDMETHODIMP Abort(BOID *pboidReason, BOOL fRetaining, BOOL fAsync); \
      STDMETHODIMP Commit(BOOL fRetaining, DWORD grfTC, DWORD grfRM); \
      STDMETHODIMP GetTransactionInfo(XACTTRANSINFO *pInfo); \
      STDMETHODIMP GetOptionsObject(ITransactionOptions **ppOptions); \
      STDMETHODIMP StartTransaction(ISOLEVEL isoLevel, ULONG isoFlags, ITransactionOptions *pOtherOptions, ULONG *pulTransactionLevel);

#define DECLARE5_ITransactionOptions \
      HRESULT __stdcall SetOptions(  \
            /* [in] */ XACTOPT __RPC_FAR *pOptions) ; \
      HRESULT __stdcall GetOptions( \
            /* [out][in] */ XACTOPT __RPC_FAR *pOptions) ;

#define DECLARE5_IAccessor \
   HRESULT STDMETHODCALLTYPE AddRefAccessor( \
      /* [in] */ HACCESSOR hAccessor, \
      /* [out] */ ULONG __RPC_FAR *pcRefCount) ; \
   HRESULT STDMETHODCALLTYPE CreateAccessor( \
      /* [in] */ DBACCESSORFLAGS dwAccessorFlags, \
      /* [in] */ ULONG cBindings, \
      /* [size_is][in] */ const DBBINDING __RPC_FAR rgBindings[  ], \
      /* [in] */ ULONG cbRowSize, \
      /* [out] */ HACCESSOR __RPC_FAR *phAccessor, \
      /* [size_is][out] */ DBBINDSTATUS __RPC_FAR rgStatus[  ]) ; \
   HRESULT STDMETHODCALLTYPE GetBindings( \
      /* [in] */ HACCESSOR hAccessor, \
      /* [out] */ DBACCESSORFLAGS __RPC_FAR *pdwAccessorFlags, \
      /* [out][in] */ ULONG __RPC_FAR *pcBindings, \
      /* [size_is][size_is][out] */ DBBINDING __RPC_FAR *__RPC_FAR *prgBindings) ; \
   HRESULT STDMETHODCALLTYPE ReleaseAccessor( \
      /* [in] */ HACCESSOR hAccessor, \
      /* [out] */ ULONG __RPC_FAR *pcRefCount) ;

#define DECLARE5_IRowset \
   HRESULT STDMETHODCALLTYPE AddRefRows( \
         /* [in] */ ULONG cRows, \
         /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
         /* [size_is][out] */ ULONG __RPC_FAR rgRefCounts[  ], \
         /* [size_is][out] */ DBROWSTATUS __RPC_FAR rgRowStatus[  ]); \
   HRESULT STDMETHODCALLTYPE GetData( \
         /* [in] */ HROW hRow, \
         /* [in] */ HACCESSOR hAccessor, \
         /* [out] */ void __RPC_FAR *pData) ; \
   HRESULT STDMETHODCALLTYPE GetNextRows( \
         /* [in] */ HCHAPTER hReserved, \
         /* [in] */ LONG lRowsOffset, \
         /* [in] */ LONG cRows, \
         /* [out] */ ULONG __RPC_FAR *pcRowsObtained, \
         /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prghRows) ; \
   HRESULT STDMETHODCALLTYPE ReleaseRows( \
         /* [in] */ ULONG cRows, \
         /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
         /* [size_is][in] */ DBROWOPTIONS __RPC_FAR rgRowOptions[  ], \
         /* [size_is][out] */ ULONG __RPC_FAR rgRefCounts[  ], \
         /* [size_is][out] */ DBROWSTATUS __RPC_FAR rgRowStatus[  ]); \
   HRESULT STDMETHODCALLTYPE RestartPosition( \
         /* [in] */ HCHAPTER hReserved) = 0 ;

#define DECLARE5_IRowsetChange \
   HRESULT STDMETHODCALLTYPE DeleteRows( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cRows, \
      /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
      /* [size_is][out] */ DBROWSTATUS __RPC_FAR rgRowStatus[  ]) ; \
 \
   HRESULT STDMETHODCALLTYPE SetData( \
      /* [in] */ HROW hRow, \
      /* [in] */ HACCESSOR hAccessor, \
      /* [in] */ void __RPC_FAR *pData) ; \
 \
   HRESULT STDMETHODCALLTYPE InsertRow( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ HACCESSOR hAccessor, \
      /* [in] */ void __RPC_FAR *pData, \
      /* [out] */ HROW __RPC_FAR *phRow) ;


#define DECLARE5_IRowsetLocate \
   HRESULT STDMETHODCALLTYPE Compare( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cbBookmark1, \
      /* [size_is][in] */ const BYTE __RPC_FAR *pBookmark1, \
      /* [in] */ ULONG cbBookmark2, \
      /* [size_is][in] */ const BYTE __RPC_FAR *pBookmark2, \
      /* [out] */ DBCOMPARE __RPC_FAR *pComparison) ; \
 \
   HRESULT STDMETHODCALLTYPE GetRowsAt( \
      /* [in] */ HWATCHREGION hReserved1, \
      /* [in] */ HCHAPTER hReserved2, \
      /* [in] */ ULONG cbBookmark, \
      /* [size_is][in] */ const BYTE __RPC_FAR *pBookmark, \
      /* [in] */ LONG lRowsOffset, \
      /* [in] */ LONG cRows, \
      /* [out] */ ULONG __RPC_FAR *pcRowsObtained, \
      /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prghRows) ; \
 \
   HRESULT STDMETHODCALLTYPE GetRowsByBookmark( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cRows, \
      /* [size_is][in] */ const ULONG __RPC_FAR rgcbBookmarks[  ], \
      /* [size_is][in] */ const BYTE __RPC_FAR *__RPC_FAR rgpBookmarks[  ], \
      /* [size_is][out] */ HROW __RPC_FAR rghRows[  ], \
      /* [size_is][out] */ DBROWSTATUS __RPC_FAR rgRowStatus[  ]) ; \
 \
   HRESULT STDMETHODCALLTYPE Hash( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cBookmarks, \
      /* [size_is][in] */ const ULONG __RPC_FAR rgcbBookmarks[  ], \
      /* [size_is][in] */ const BYTE __RPC_FAR *__RPC_FAR rgpBookmarks[  ], \
      /* [size_is][out] */ DWORD __RPC_FAR rgHashedValues[  ], \
      /* [size_is][out] */ DBROWSTATUS __RPC_FAR rgBookmarkStatus[  ]) ;

#define DECLARE5_IRowsetScroll \
   HRESULT STDMETHODCALLTYPE GetApproximatePosition( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cbBookmark, \
      /* [size_is][in] */ const BYTE __RPC_FAR *pBookmark, \
      /* [out] */ ULONG __RPC_FAR *pulPosition, \
      /* [out] */ ULONG __RPC_FAR *pcRows) ; \
 \
   HRESULT STDMETHODCALLTYPE GetRowsAtRatio( \
      /* [in] */ HWATCHREGION hReserved1, \
      /* [in] */ HCHAPTER hReserved2, \
      /* [in] */ ULONG ulNumerator, \
      /* [in] */ ULONG ulDenominator, \
      /* [in] */ LONG cRows, \
      /* [out] */ ULONG __RPC_FAR *pcRowsObtained, \
      /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prghRows) ;

#define DECLARE5_IRowsetUpdate \
   HRESULT STDMETHODCALLTYPE GetOriginalData( \
      /* [in] */ HROW hRow, \
      /* [in] */ HACCESSOR hAccessor, \
      /* [out] */ void __RPC_FAR *pData) ; \
 \
   HRESULT STDMETHODCALLTYPE GetPendingRows( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ DBPENDINGSTATUS dwRowStatus, \
      /* [out][in] */ ULONG __RPC_FAR *pcPendingRows, \
      /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prgPendingRows, \
      /* [size_is][size_is][out] */ DBPENDINGSTATUS __RPC_FAR *__RPC_FAR *prgPendingStatus) ; \
 \
   HRESULT STDMETHODCALLTYPE GetRowStatus( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cRows, \
      /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
      /* [size_is][out] */ DBPENDINGSTATUS __RPC_FAR rgPendingStatus[  ]) ; \
 \
   HRESULT STDMETHODCALLTYPE Undo( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cRows, \
      /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
      /* [out][in] */ ULONG __RPC_FAR *pcRowsUndone, \
      /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prgRowsUndone, \
      /* [size_is][size_is][out] */ DBROWSTATUS __RPC_FAR *__RPC_FAR *prgRowStatus) ; \
 \
   HRESULT STDMETHODCALLTYPE Update( \
      /* [in] */ HCHAPTER hReserved, \
      /* [in] */ ULONG cRows, \
      /* [size_is][in] */ const HROW __RPC_FAR rghRows[  ], \
      /* [out][in] */ ULONG __RPC_FAR *pcRows, \
      /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prgRows, \
      /* [size_is][size_is][out] */ DBROWSTATUS __RPC_FAR *__RPC_FAR *prgRowStatus) ;


#define DECLARE5_IRowsetFind \
        virtual HRESULT STDMETHODCALLTYPE FindNextRow( \
            /* [in] */ HCHAPTER hChapter,\
            /* [in] */ HACCESSOR hAccessor,\
            /* [in] */ void __RPC_FAR *pFindValue,\
            /* [in] */ DBCOMPAREOP CompareOp,\
            /* [in] */ ULONG cbBookmark,\
            /* [size_is][in] */ const BYTE __RPC_FAR *pBookmark,\
            /* [in] */ LONG lRowsOffset, \
            /* [in] */ LONG cRows,\
            /* [out][in] */ ULONG __RPC_FAR *pcRowsObtained,\
            /* [size_is][size_is][out] */ HROW __RPC_FAR *__RPC_FAR *prghRows) ;

#define DECLARE5_ISequentialStream \
         /* [local] */ HRESULT STDMETHODCALLTYPE Read(  \
            /* [length_is][size_is][out] */ void __RPC_FAR *pv, \
            /* [in] */ ULONG cb, \
            /* [out] */ ULONG __RPC_FAR *pcbRead) ; \
         \
         /* [local] */ HRESULT STDMETHODCALLTYPE Write(  \
            /* [size_is][in] */ const void __RPC_FAR *pv, \
            /* [in] */ ULONG cb, \
            /* [out] */ ULONG __RPC_FAR *pcbWritten) ;

#define DECLARE5_ITableDefinition \
         HRESULT STDMETHODCALLTYPE CreateTable(  \
            /* [in] */ IUnknown __RPC_FAR *pUnkOuter, \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ ULONG cColumnDescs, \
            /* [size_is][in] */ const DBCOLUMNDESC __RPC_FAR rgColumnDescs[  ], \
            /* [in] */ REFIID riid, \
            /* [in] */ ULONG cPropertySets, \
            /* [size_is][out][in] */ DBPROPSET __RPC_FAR rgPropertySets[  ], \
            /* [out] */ DBID __RPC_FAR *__RPC_FAR *ppTableID, \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppRowset) ; \
 \
        HRESULT STDMETHODCALLTYPE DropTable(  \
            /* [in] */ DBID __RPC_FAR *pTableID) ; \
 \
        HRESULT STDMETHODCALLTYPE AddColumn(  \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ DBCOLUMNDESC __RPC_FAR *pColumnDesc, \
            /* [out] */ DBID __RPC_FAR *__RPC_FAR *ppColumnID) ; \
 \
        HRESULT STDMETHODCALLTYPE DropColumn(  \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ DBID __RPC_FAR *pColumnID) ;

#ifdef S4COMMAND
#define DECLARE5_IDBCreateCommand \
        virtual /* [local] */ HRESULT STDMETHODCALLTYPE CreateCommand( \
            /* [in] */ IUnknown __RPC_FAR *pUnkOuter, \
            /* [in] */ REFIID riid, \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppCommand) ;
#endif

#define DECLARE5_IIndexDefinition \
        virtual HRESULT STDMETHODCALLTYPE CreateIndex( \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ DBID __RPC_FAR *pIndexID, \
            /* [in] */ ULONG cIndexColumnDescs, \
            /* [size_is][in] */ const DBINDEXCOLUMNDESC __RPC_FAR rgIndexColumnDescs[  ], \
            /* [in] */ ULONG cPropertySets, \
            /* [size_is][out][in] */ DBPROPSET __RPC_FAR rgPropertySets[  ], \
            /* [out] */ DBID __RPC_FAR *__RPC_FAR *ppIndexID) ; \
 \
        virtual HRESULT STDMETHODCALLTYPE DropIndex( \
            /* [in] */ DBID __RPC_FAR *pTableID, \
            /* [in] */ DBID __RPC_FAR *pIndexID) ; \

#define DECLARE5_IDBCreateSession \
        virtual HRESULT STDMETHODCALLTYPE CreateSession(  \
            /* [in] */ IUnknown __RPC_FAR *pUnkOuter, \
            /* [in] */ REFIID riid, \
            /* [iid_is][out] */ IUnknown __RPC_FAR *__RPC_FAR *ppDBSession) ;

#define DECLARE5_IDBInitialize \
        virtual HRESULT STDMETHODCALLTYPE Initialize( void) ; \
        virtual HRESULT STDMETHODCALLTYPE Uninitialize( void) ;

#define DECLARE5_IDBProperties \
        virtual HRESULT STDMETHODCALLTYPE GetProperties(  \
            /* [in] */ ULONG cPropertyIDSets, \
            /* [size_is][in] */ const DBPROPIDSET __RPC_FAR rgPropertyIDSets[  ], \
            /* [out][in] */ ULONG __RPC_FAR *pcPropertySets, \
            /* [size_is][size_is][out] */ DBPROPSET __RPC_FAR *__RPC_FAR *prgPropertySets) ; \
 \
        virtual HRESULT STDMETHODCALLTYPE GetPropertyInfo(  \
            /* [in] */ ULONG cPropertyIDSets, \
            /* [size_is][in] */ const DBPROPIDSET __RPC_FAR rgPropertyIDSets[  ], \
            /* [out][in] */ ULONG __RPC_FAR *pcPropertyInfoSets, \
            /* [size_is][size_is][out] */ DBPROPINFOSET __RPC_FAR *__RPC_FAR *prgPropertyInfoSets, \
            /* [out] */ OLECHAR __RPC_FAR *__RPC_FAR *ppDescBuffer) ; \
 \
        virtual HRESULT STDMETHODCALLTYPE SetProperties(  \
            /* [in] */ ULONG cPropertySets, \
            /* [size_is][out][in] */ DBPROPSET __RPC_FAR rgPropertySets[  ]) ;

#define DECLARE5_IDBInfo \
        virtual /* [local] */ HRESULT STDMETHODCALLTYPE GetKeywords( \
            /* [out] */ LPOLESTR __RPC_FAR *ppwszKeywords) ; \
 \
        virtual /* [local] */ HRESULT STDMETHODCALLTYPE GetLiteralInfo( \
            /* [in] */ ULONG cLiterals, \
            /* [size_is][in] */ const DBLITERAL __RPC_FAR rgLiterals[  ], \
            /* [out][in] */ ULONG __RPC_FAR *pcLiteralInfo, \
            /* [size_is][size_is][out] */ DBLITERALINFO __RPC_FAR *__RPC_FAR *prgLiteralInfo, \
            /* [out] */ OLECHAR __RPC_FAR *__RPC_FAR *ppCharBuffer) ;
