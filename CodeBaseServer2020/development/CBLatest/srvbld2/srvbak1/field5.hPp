// field5.hpp

/*
   Class derivations list...

  Fieldset5all::Fieldset5low contains an array of:
      Field5all::



*/

#ifdef OLEDB5BUILD
   class5 Ptr5lenAlloc ;

   class5 ByRef5
   {
   public:
   //   inline void initNullPtr() { throw Err5internal() ; }  // not used for base ByRef5
   //   void initNullPtr() { l = 0; p = "\0"; }
      inline void allocAgain( ULONG lenNew )
      {
         if ( lenNew == l )
            return ;
         if ( p )
            free5( p ) ;
         initAlloc( lenNew ) ;
      }
      inline void free() { if(p) free5(p) ; p = NULL ; l = 0 ; }
      //CJ 27/06/00 change input paramater of init so that the length is reported corectly.
      inline void init( char *pParm, ULONG lParm ) { p = pParm, l = lParm ; }
      inline void initAlloc( ULONG len ) { l = len ; p = (char *) alloc5(len) ; }
      inline void initClone( ByRef5 *from )
         { initAlloc( from->len() ) ; memcpy( p, from->ptr(), l ) ; }
      inline void initFromPtr( char *fromPtr, ULONG fromLen )
      {
         initAlloc( fromLen ) ;
         memcpy( p, fromPtr, fromLen ) ;
      }
      inline ULONG len() { return l ; } // The allocated length
      inline ULONG lenContents()  // the length without the 'null' added
         {
            return l
            #ifndef S4SERVER
               - 1
            #endif
            ;
         }
      inline const char *ptr() { return p ; }
      inline char *dynamicPtr() { return p ; }
      inline void copyPointer( ByRef5 *byRef )
         {  // use functions so we can copy correctly from Ptr5alloc
            p = byRef->dynamicPtr() ;
            l = byRef->lenContents() + 1 ;
         }
      void copyPointer( Ptr5lenAlloc *byRef ) ;
   protected:
      char *p ;
      ULONG l ;   // the allocated length
   } ;



   class5 Ptr5lenAlloc : public ByRef5
   {
      // the Ptr5lenAlloc implementation - similar to ByRef5, but does not take
      // into account a null byte.  Therefore, the len() == contentsLen().
   public:
      Ptr5lenAlloc() { p = 0 ; curLen = 0 ; didAlloc = 0 ; l = 0 ; }
      ~Ptr5lenAlloc() { free() ; }

      void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
      void * operator new[]( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
      void operator delete( void *p ) { free5(p) ; }
      void operator delete[]( void *p ) { free5(p) ; }

      inline void allocAgain( ULONG lenNew )
      {
         assert5( didAlloc == 1 ) ;  // ensure we allocated to begin with
         if ( lenNew > l )  // we must re-allocate
            ByRef5::allocAgain( lenNew ) ;
         curLen = lenNew ;  // if lenNew <= l, just change the curLen
      }
      inline void free() { if ( didAlloc ) { ByRef5::free() ; curLen = 0 ; } }
      inline void init( char *ptr, ULONG len )
         { free() ; p = ptr ; curLen = len ; didAlloc = 0 ; l = 0 ; }
      inline void initAlloc( ULONG len )
         {
            if ( didAlloc )
               allocAgain( len ) ;
            else
            {
               ByRef5::initAlloc( len ) ;
               curLen = len ;
               didAlloc = 1 ;
            }
         }
      inline void initNullPtr()
         { free() ; p = "\0" ; curLen = 0 ; didAlloc = 0 ; l = 0 ; } // special case, points to a null emptry string

      inline ULONG lenContents() { return curLen ; }  // no null added for Ptr5lenAllocAlloc
      inline ULONG len() { return curLen ; }
      inline void copyPointer( Ptr5lenAlloc *pt2 )
      {
         p = pt2->p ;
         l = pt2->l ;
         curLen = pt2->curLen ;
         didAlloc = 0 ;
      }
   private:
      ULONG curLen ;
      char didAlloc ;
   } ;



   #ifdef S4SERVER
      class Fieldset5infoServer
      {
      public:
         Fieldset5infoServer() { field = 0 ; isDeferred = 0 ; doSend = 0 ; }
         void * operator new( size_t s ) { void *ptr = u4alloc(s) ; return ptr ; }
         void * operator new[]( size_t s ) { void *ptr = u4alloc(s) ; return ptr ; }
         void operator delete( void *p ) { u4free(p) ; }
         void operator delete[]( void *p ) { u4free(p) ; }

         // contains sufficient information to identify a field on the server
         // (in future) could potentially include more info, such as a fieldToSendLen (maybe
         // send less info than the total length of the field)
         FIELD4 *field ;
         int isDeferred ;
         int doSend ;  // true if this is part of the fieldset we are sending
      } ;
   #endif /* S4SERVER */



   class S5CLASS Fieldset5low
   {
      // for the Table5server, indicate which fields are non-deferred (i.e. need to send
      // back to clients)
   public:
      Fieldset5low() { nFields = 0 ; }

   //   virtual int fieldDeferred( short iOrdinal ) const = 0 ;
   //   virtual int fieldPresent( short iOrdinal ) const = 0 ;
   //   virtual int ordinalStart() = 0 ;
   //   virtual int ordinalEnd() = 0 ;

      // retrieves the field ordinal based on the arrayNumber which == iField entry number...
   //   virtual inline int fieldOrdinalFromFieldArrayNumber( int arrayNumber ) const = 0 ;
      inline short numFields() const { return nFields ; }
      inline void setNumFields( short val ) { nFields = val ; }
      inline short *numFieldsPtr() { return &nFields ; }

   private:
      short nFields ;   // The number of fields in the fieldset.  This is NOT the
                        // same as rowset->nFields.  range from 1 - 32768,
   } ;


   #ifdef S4SERVER
      class Fieldset5server ;
   #endif

   class5 Field5iterator
   {
      // this class is used to iterate through the fields in the table.  The range of this
      // varies.  It includes all fields whether they are in the fieldset or not.
      // only those fields present in the fieldset can be obtained using the next(a,b) or related
      // functions

      // next() must be called before usage.

      // the 'entryOn' should start at the last array entry + 1 since we subtract 1 on the
      // first 'next' call.
   public:
      #ifdef S4SERVER
         inline Field5iterator( Fieldset5server *fieldsetParm ) ;
      #else
         inline Field5iterator( Fieldset5all *fieldsetParm ) ;
      #endif

      // AS 09/10/99 need to have the function be faster, so no longer general function next()...
      // inline int nextNonDeferredPresent() { return next( 0, 1, 1 ) ; }
      // inline short nextDeferredPresent() { return next( 1, 1, 1 ) ; }
      short nextDeferredPresent() ;
      short nextNonDeferredPresent() ;

      // AS 06/02/99 --> changed to include when entryOn == 0, this is the 0th entry of the
      // array, and needs to be included...
      inline short next() { return ( (--entryOn) >= lastEntry ? ( entryOn ) : ( -1 ) ) ; }
      short next( Bool5 skipDuplicates ) ;
      inline int noFields() { return ( fieldset->numFields() == 0 ) ; }
      inline short currentOrdinal() ;

   private:
      short next( Bool5 fieldDeferredOnly, Bool5 fieldPresentOnly, Bool5 skipDuplicates ) ;  // by default return next present field deferred
      short entryOn ;
      short lastEntry ;
      short startEntry ;

      Fieldset5low *fieldset ;
      #ifdef S4SERVER
         Fieldset5server *fieldsetAll ;
      #else
         Fieldset5all *fieldsetAll ;
      #endif
   } ;



   #ifndef S4STAND_ALONE
      #ifdef S4SERVER
         class Table5server ;
      #else
         class Table5all ;
      #endif

      class S5CLASS Fieldset5communications : public Fieldset5low
      {
      public:
         #ifdef S4SERVER
            void init( Table5server *tbl, Fieldset5server *fAll ) { assert5( tbl != 0 ) ; table = tbl ; fieldsetAll = fAll ; }
         #else
            void init( Table5all *tbl, Fieldset5all *fAll ) { assert5( tbl != 0 && fAll != 0 ) ; table = tbl ; lenFieldsRaw = 0 ; fieldsetAll = fAll ; }
         #endif
         #ifdef E4DEBUG
            long countNonDeferredFieldsLen() ;
         #endif
         //   virtual long offsetRaw( ULONG iOrdinal ) = 0 ;
         void sendNonDeferredFields( CONNECT4 *connect, char *buffer ) ;
         void receiveNonDeferredFields( CONNECT4 *connect, char *buffer ) ;
         Ptr5lenAlloc *receiveDeferredFields( CONNECT4 *connect, ByRef5 *byref ) ;
         virtual int nullBytesOffsetRaw() = 0 ;
         #ifdef S4SERVER
            Table5server *table ;
            void sendDefaultValues( CONNECT4 *connect ) ;
         #else
            Table5all *table ;
            ULONG lenFieldsRaw ;   // length of raw buffer for client
            void initializeBlankDeferredFields( ByRef5 *ptrsIn ) ;  // to blank out deferred fields on newly inserted row that has setData called on it
         #endif
      private:
         #ifdef S4SERVER
            Fieldset5server *fieldsetAll ;
         #else
            Fieldset5all *fieldsetAll ;
         #endif
      } ;
   #endif


   #ifdef S4SERVER
      class S5CLASS Fieldset5server : public Fieldset5communications
      {
      public:
         Fieldset5server() { nNonDeferredFields = 0 ; fields = NULL ; nDeferredSend = 0 ; nullBytesOffsetRawVal = 0 ; }
         ~Fieldset5server() ;
         void init( Table5server *tbl ) ;

         void sendDeferredFields( CONNECT4 *connect ) ;
         void markFieldToSend( short iOrdinal, int isDeferred ) ;
         void resetFieldsToSend() ;
         inline int fieldDeferred( short iOrdinal ) const { assert5( iOrdinal > 0 && iOrdinal <= Fieldset5low::numFields() ) ; return fields[iOrdinal-1].isDeferred ; }
         inline int fieldPresent( short iOrdinal ) const { assert5( iOrdinal > 0 && iOrdinal <= Fieldset5low::numFields() ) ; return fields[iOrdinal-1].doSend ; }  // for c/s if in the field-set, it is present

         // our implementation of the field iterator is:
         // start ordinal = nFields+1, which means the first effective ordinal is 'nFields'
         // the last ordinal is 1
         // this is correct because we access the fields array as: [fieldOrdinal-1], so we want to
         // end on 1 and start at nFields.
         inline int ordinalStart() { return numFields() + 1 ; }
         inline int ordinalEnd() { return 1 ; }
         // based on the field iterator, the 'ordinal' will be identical to the array number.
         // the user of this class just needs to keep in mind that they must access the fields
         // member with [ordinal-1] or with [arrayNumber-1] as the correct fieldArrayIndex
         inline short fieldOrdinalFromFieldArrayNumber( short arrayNumber ) const { return arrayNumber ; }
         inline int fieldArrayIndex( int ordinal ) const { return ordinal - 1 ; }

         // inline int ordinalStart() { return nFields ; }
         // inline int ordinalEnd() { return 0 ; }
         int nDeferredSend ;
         inline int nullBytesOffsetRaw() { return nullBytesOffsetRawVal ; }
         int nNonDeferredFields ;
         long offsetRaw( ULONG iOrdinal ) ;
      private:

         // fields is an array of the fields in the server's fieldset.  It is indexed by:
         // fields[iOrdinal-1].  Therefore, the 0th entry contains info for ordinal 1, etc.
         // fields[0] contains info for the 1st field in the databse.
         // the [nFields-1] or last entry contains info on the deleted field
         Fieldset5infoServer *fields ;
         int nullBytesOffsetRawVal ;
      } ;
   #endif /* S4SERVER */



   #ifndef S4SERVER
      //char *Row5contents::getNullBytesPtr()

      #define pFieldset5all(p) ((Fieldset5all *)(p))

      class Row5 ;
      class Rowset5 ;
      class iField5 ;

      /*
      Notes

      The number of fields sent for a created rowset only goes up.
      Once a field is sent for a particular accessor, it will always
      be sent for the rowset.

      That is, if new accessors are removed, do
      not remove the amount of fields sent.  Only when the rowset
      is closed does this change.

      The idea is to protect against having
      to send additional data while optimizing the number of fields
      sent under client/server.  Accessors can
      come and go under the specification and this algorithm
      makes the implementation reasonably simple.

      How does the API indicate something was locked?
      A: It does not seem to explicitly.

      How do you find out if a record is deleted?
      A: Use the implicit DELETED field.  The actual name of this field
      can be changed at runtime through the CONFIG4.DBF.  The default
      name is "DELETED" (unless some other SQL construct has this name).

      ? Do not want deleted records & index specifies one. -> IRowsetLocate.
      A: We will 'fudge' this one for the first implementation and just ignore
      the possibility of deleted records when doing the offset calculation.

      Filter functionality for schemas?
      A: Ken to work on this one still.

      Typically there will be one accessor for a rowset for the main data
      and perhaps a different accessor for a BLOB.  Optimize for this
      senario if possible.  (I am not sure what special optimizations
      might be possible.  My initial idea turned out not to be effective.)



      /* Field5sequential is the class defining the database functionality
      needed for ISequential interface support.

      Several can be going at once on the same memo file.
      Only one per memo field of a row at once.

      We will not support transaction functionality or delayed updates with this impelementation
      on memo fields.  The specification allows this.
      */

      class Table5all ;

      class Row5buf
      {
      public:
         virtual char *pBuffer() = 0 ;
      } ;

      class Row5bufField
      {
      public:
         Row5bufField() { fieldAll = 0 ; ignoreValue = 0 ; }  // get's initialized by Field5all...
         virtual Type5 typ() = 0 ;
         virtual int len() = 0 ;
         virtual int lenActual() = 0 ;
         virtual int decimals() = 0 ;
         virtual int offset() = 0 ;
      //   virtual ULONG nullBitRowPosition() { return -1 ; }
         virtual int isReadOnly() { return 0 ; }
         virtual int isDeferred() { return 0 ; }
         // AS 12/04/00 - Specs allow for ignored fields now...
         Bool5 ignoreValueGet() { return ignoreValue ; }
         void ignoreValueSet( Bool5 val ) { assert5( ignoreValue == 0 ) ; ignoreValue = val ; }

         virtual DBTYPE dbtypeNoRef() { return DBTYPE_EMPTY ; } // If the field is in a 'DBTYPE' format, it is
                                        // returned.  Otherwise, DBTYPE_EMPTY is returned.
         virtual DBTYPE dbtypeWithRef() { return DBTYPE_EMPTY ; }
         void getBufferTypeWithRef( Type5 *typPtr, DBTYPE *dbtypePtr) ;
         Field5all *fieldAll ;
      private:
         Bool5 ignoreValue ;
      } ;



      class Row5bufControl
      {
      public:
      //   virtual Row5buf *Row5bufCreate() ;
         virtual Row5bufField *field( int iOrdinal ) = 0 ;
         virtual Row5bufField *field( int iOrdinal, DBTYPE wType ) = 0 ; // Returns for DBTYPE_EMPTY if wType is not there.
         virtual inline Row5bufField *fieldEntry( int iEntry ) = 0 ; // Starts from 0
         virtual ULONG nEntries( int iOrdinal ) { return 1 ; }
         virtual inline ULONG nEntries() = 0 ; // Total number of entries in the buffer.
         // virtual inline int nullTest( char *buf, ULONG nullPosition, long iOrdinal = -1 ) { return -1 ; }
         // virtual inline void nullSet( char *buf, ULONG nullPosition ) {}
         // virtual inline void nullReset( char *buf, ULONG nullPosition ) {}
         virtual inline int nullTest( char *buf, ULONG nullPosition, long iOrdinal = -1 ) = 0 ;
         virtual inline void nullSet( char *buf, ULONG nullPosition ) = 0 ;
         virtual inline void nullReset( char *buf, ULONG nullPosition ) = 0 ;
         virtual inline UINT    codePage() { return 437; } // Must return single byte code pages only only & not 0.
              virtual inline Collate4name collateName() { return collate4none ; }  // used for collated index rows only, by default not known
      } ;



      #ifndef S4OFF_BLAST
         // Refer to Blast5.hpp
         #ifdef S4SERVER
            class Field5sequential : public ISequentialStream
         #else
            class Field5sequential : public ISequentialStream, public Unknown5
         #endif
         // It seemed odd to me that the interface header Trudy showed me had only the read & write function
         // in it.  I have not actually seen formal documentation on IInterface.  I had trouble
         // finding any.  (Checked the printed docs, VC++ 4.2, On-line docs trudy is using.)
         // However, I think the main principles are reasonably obvious.
         {
         public:
            void * operator new( size_t s ) { void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
            void operator delete( void *p ) { free5(p) ; }

            // NOTE: we only support a single open on a given blob at a given time.  We return
            // DBSTATUS_E_CANTCREATE if a second attempt is made to create a Field5sequential object
            // on the same blob field.
            DECLARE5_ISequentialStream
            DECLARE5_IUnknown

            // Some members we probably need.
            int status ; //??Not yet initialized; For Writing; For reading
            int field ; // The field ordinal.
            Rowset5 *rowset ;
            Row5 *row ;

            #ifndef S4OFF_BLAST
               Blast5field *blast ;
            #endif

            int release() ;  // Frees up all memory.
            #ifndef S4SERVER
               int init( Row5buf *row, int fieldOrdinal ) ; //, int newStatus ) ;
            #endif
            void setData( ISequentialStream *from ) ; // Copies all of the data from 'from' into the field.

            int flush() ; // rc == 0 or rc == r4locked
         } ;
      #endif /* !S4OFF_BLAST */



      class Field5desc  // Integrate with Index5fieldInfo & FieldSet5info
      {
         /* This class describes a field.  */
      public:
         Field5desc()
         {
            offsetRow = -1 ;   // -1 means needs to be initialized...
            ordinal = -1 ;
            typeInternal = type5none ;
         }

         inline int ordinalGet() const { return ordinal ; }
         inline void ordinalSet( int ordSet ) { ordinal = ordSet ; }

         Type5 typeInternal ; // This is always the type of the internal raw format.
         ULONG offsetRow ;  // This is the offset into the main buffer.

      private:
         // make this private to help debugging by forcing going through functions
         int ordinal ; // Identify corresponding field.  Position starts at 0. - 0 is the self-referencing bookmark
      } ;



      const int bad4value = -4 ;  // use for debug comparisons some places
      const int index4nullValue = -5 ;  // use to know that is an index null value

      class Field5all : public Row5bufField, public Field5desc
      {
         /* Field5all array specifies the format of Row5->current & Row5->original buffers.
            Do not worry about deferred data from a point of view of whether some of the
            fields have changed.  The consumer has control over whether to accept this risk
            by setting the appropriate isolation level which controls what locking takes place.
         */
      public:
         Field5all() ;

         inline Type5 typ() { return typeInternal ; }
         inline int len() { return lenInBufferGet() ; }
         inline int lenActual() { return ( isPointer ? type5lenVariable : lenField ) ; }  // returns the actual length of the field, or type5lenVariable if a pointer
         inline int decimals() { return lenFieldDec ; }
         inline int offset() { return offsetRow ; }
         inline ULONG nullBitRowPosition()
         {
            // return ( nullBitRowPositionValue == index4nullValue ) ? -1 : nullBitRowPositionValue ;
            return nullBitRowPositionValue ;
         }
         inline int isReadOnly() { return readOnly ; }
         inline int isDeferred() { return deferred ; }

         inline DBTYPE dbtypeNoRef() { return dbtype ; } // If the field is in a 'DBTYPE' format, it is
                                        // returned.  Otherwise, DBTYPE_EMPTY is returned.
         inline DBTYPE dbtypeWithRef()
         {
            return
            (
               isPointer ?   /* if isPointer */
               (
                  ( dbtype == DBTYPE_EMPTY ) ?   /* if ( dbtype == DBTYPE_EMPTY ) */
                     DBTYPE_EMPTY
                  : /* else */
                     ( dbtype | DBTYPE_BYREF )
               )
               : /* else */
                  dbtype
            ) ;
         }


         virtual void init( Table5all *tbl, iField5 *iField, int iOrdinal, int isProviderMemoryParm ) ;
         void initLenFinish( int len, ULONG *offsetRowParm, int recnoFieldParm = 0 ) ;
         void initIsPointer() ;

         DBTYPE dbtype ;    // This is the OLEDB data type that the field buffer data must be in.
                            // If the value is DBTYPE_EMPTY (0), then the data is in
                            // the format specified by 'typeInternal'.  It is up to
                            // the Accessor/Fieldset initialization code to determine
                            // whether this has to be an OLEDB type.
                            // Refer to the TYPE HANDLING documentation for more details.
                            // DBTYPE_BYREF should never be part of this value.
         int lenField ;     // The length of the field, including nulls, for fixed length fields.
                            // This is as the data is stored in Row4.
                            // This value is invalid if it is variable length.
         int lenFieldDec ;  // Only relevant if the data is in Numeric or Float internal formats.
         int isProviderMemory ; // True if the memory for the data is provider supplied for one or more of the accessors.
         int deferred ; // Is this field value deferred.  Note that 'isDeferred' implies a pointer needs to be in the row buffer.
         int isInterface ; // Is it an interface pointer.
         int isPointer ; // This is whether it is a pointer in the row buffer.
         int readOnly ;

         ULONG nullBitRowPositionValue ; // ULONG(-1) means the field cannot be null.  // offset in row buffer, not raw buffer
         ULONG nullBitRawPositionValue ;

         Table5all *table ;

         // AS 05/31/99 need to store the field offsetRaw here because it may vary from fieldset to
         // fieldset, so cannot store in Column Info
         inline long offsetRaw() { assert5( offsetRawVal != -1 ) ; return offsetRawVal ; }  // call no-verify if may not be inited yet..
         inline long offsetRawNoVerify() { return offsetRawVal ; }
         inline void setOffsetRaw( long offsetIn )
         {
            assert5( offsetIn != -1 ) ;  // -1 used to indicate not initialized, -2 means recno/bookmark, -3 means deferred
            offsetRawVal = offsetIn ;
         }
         inline int lenInBufferGet() { return lenInBufferVal ; }
         inline void lenInBufferSet( int val ) { lenInBufferVal = val ; }
      private:
         int lenInBufferVal ;  // The length in the main buffer. For example, it may be sizeof(void *) if 'isByRef'.
                               // If there is not some kind of data redirection, it will be the same as 'lenField'.
         long offsetRawVal ;   // -1 means not initialized yet, -2 means recno/bookmark field
      } ;

      // This is information for iField5 that sorting is done on.  It is important
      // that this information be kept in this order.
      class iField5sortMembers
      {
      public:
         int isDeferred ;
         int isInterface ;
         // AS 06/02/99 changed order to be above isProviderMemory because of changes for index/integrated indexes.
         // they want to be sub-sorted on order as primary, we carefully avoid the duplicates at a later
         // time...
         int order ;            // similar to iOrdinal, but for integrated rowsets is the key order not the table ordinal
         int isProviderMemory ; // True if the memory for the data is provider supplied for one or more of the accessors.
                                // All 'provider supplied' memory must be pointers in the row buffer so when extra fields
                                // are fetched, we can hang onto this memory.
                                // Must come before ordinal in the sort because the index row buffer
                                // must be in ordinal order excluding the provider memory portion.
      } ;

      class iField5 : public iField5sortMembers
      {
      public:
         // new does not seem to work on arrays according to test program.  operator void *new(size_t s) ;
         iField5() { iField = -1 ; dbtype = DBTYPE_EMPTY ; more = 0;
            #ifdef S4CLIENT
               receiveDeferredValue = 0 ;
            #endif
            }
         #ifdef S4CLIENT
            ~iField5() { if ( receiveDeferredValue != 0 ) delete receiveDeferredValue ; }
         #endif
         void init( const DBBINDING *binding, int isDeferred, short *nFields ) ;
         void init( DBTYPE t, int isDeferred, int isInterface, int isProviderMemory, short *nFields ) ;
         void initLow( DBTYPE t, int isDeferredParm, int isInterfaceParm, int isProviderMemoryParm, short *nFields ) ;
         int nEntries() ; // This is the number of Field5all entries in the fieldset for the ordinal
         //Field5 *field( int iOrdinal ) ; Should be in Fieldset5 ..
         iField5 *search( DBTYPE t ) ;
         iField5 *search( DBTYPE t, int isInterfaceParm, int isDeferredParm ) ;
         iField5 *search( DBTYPE t, int isInterfaceParm, int isDeferredParm, int isProviderMemoryParm ) ;
         int fieldPresent() { return iField != -1 ; }
         int iField ;     // Index into the Field5all array.
                          // -1 Not Present; Otherwise,
                          // Fieldset5all::fields[Fieldset5all::iField[iOrdinal].iField] is the field info.
                          // from an ordinal position.  This information is
                          // used to efficiently build the steps for an
                          // accessor.
                          // Do not change the type from 'int' without adjusting the sorting in Fieldset5all::init()
      //   int isByRef ;    // True if one or more of the DBBINDING(s) is DBTYPE_BYREF for one or more of the accessors.
         DBTYPE dbtype ;  // The DBTYPE of the DBBINDING(s) needing the entry.
                          // This does not include any DBTYPE_BYREF information.
                          // The first item in the list must always be DBTYPE_EMPTY once the fieldset is initialized.
      //   int nMore ;      // # of entries in 'more'  // AS - not used...
         iField5 *more ;  // If there is more than one entry, this points to it.
         int iOrdinal ;
         #ifdef S4CLIENT
            Ptr5lenAlloc  *receiveDeferredValue ;  // used for deferred fields only
         #endif
      } ;

      /* This class defines the data in Row5all ..
      Ordinal Position Conventions
      1 - First Field
      nFields+1 - Delete Flag
      nFields+2 - Record # (This one is not typically used by Fieldset5info as the
                  information is kept as a separate member of Row5.)
      */

      class Mem5 ;


      class Table5rowsetValues ;


      #ifdef S4CLIENT
         class5 Fieldset5all : public Link5, public Fieldset5communications
      #else
         class5 Fieldset5all : public Link5, public Fieldset5low
      #endif
      {
      public:
         virtual void init( Accessor5handler *accParm, int isNullable ) ;
         virtual int isIndex() { return 0 ; }
         Fieldset5all( Table5all *tbl )
         {
            lenFieldsVal = hasReadOnly = 0 ;
            fields = 0 ; iField = 0 ; byRefBuffer = 0 ;
            nFieldsByRef = deferredFieldIndexStart = nInterface = 0 ;
            interfaceFieldIndexStart = nullBytesOffsetRawVal = nullBytesOffsetRow = nullBytesNum = 0 ;
            memInternalRowBuffer = 0 ; table = tbl ; nDeferred = 0 ;
      //      rowset = 0 ;
            accessorHandler = 0 ; defaultEmptyRow = 0 ;
         }
         Field5all *field( Field5all *fieldNew ) ; // Lookup for changing row buffer purposes ..
         Field5all *field( ULONG iOrdinal, DBTYPE t ) ;
                           // Returns the Field5 exactly matching 't' if it exists;
                           // otherwise, an DBTYPE_EMPTY will sufice.
         Field5all *field( ULONG iOrdinal ) // Must return a DBTYPE_EMPTY due to ordering in iField
            {  return ( iField[iOrdinal].iField == -1 ? 0 : fields + iField[iOrdinal].iField ) ; }
         Field5all *field( iField5 *iField ) { return fields + iField->iField ; }
         ~Fieldset5all() ;
         // unsigned int fieldOffset( short iOrdinal ) ;
         Convert5group rawToRow ;
         Convert5group rawToRowDeferred ;
         Convert5group rowToRaw ;
         // Rowset5all *rowset ;
         Accessor5handler *accessorHandler ;

         Field5all *fields ;

         ULONG niField ;
         iField5 *iField ; // Allocated with 'new'.  One per rowset->nFields plus
                           // one for position zero. Note that the first entry
                           // must point to the primary field type.  That is if
                           // there are two or more, the first must be a type the
                           // rest can convert into.  Typically, this will be the
                           // internal field type.
         ByRef5 *byRefBuffer ; // One for each raw data 'byRef' field (ie. memo) in
                              // the fieldset.  Corresponds to the fields specified by iField.
                              // Refer to 'nDeferred' and 'deferredFieldIndexStart'.
         int nFieldsByRef ; // The total number of fields for which memory needs to be freed.
                            // For memory freeing optimization.
         int deferredFieldIndexStart ; // index to first nFieldsInfo that contains
                                   // a deferred field.  Note that because the
                                   // Fieldset5 fields are in order: non-deferred,
                                   // deferred, interface, all deferred fields
                                   // are between this index and the
                                   // startInterfaceFieldIndex index

         int nInterface ;
         int interfaceFieldIndexStart ; // same as startDeferredFieldIndex except
                                       // for the interface data

         int nullBytesOffsetRawVal ; // The null bytes are part of the raw buffer.  This
                                  // is the offset to the start of them. If there are
                                  // no null bytes, this value is undefined.
         int nullBytesOffsetRow ;
         int nullBytesNum ;

         int hasReadOnly ;    // field set includes a read-only field

         Mem5 *memInternalRowBuffer ;   // efficient memory allocator to not zero out row buffer memory when allocating
         #ifdef E4DEBUG_KEN
            void report(char *ptr = 0) ;
         #endif

         inline int fieldOrdinalFromFieldArrayNumber( int arrayNumber ) const
         {
            // based on the field array entry number, adjusted as an array index value
            assert5( fields != 0 ) ;
            return fields[arrayNumber].ordinalGet() ;
            // return arrayNumber ;
            // assert5( arrayNumber >= 0 && arrayNumber < (int)niField ) ;
            // return fields[iField[arrayNumber].iField].ordinal ;
         }
         Table5all *table ;
         int nDeferred ;   /* for the server this == present fields */

         inline int fieldDeferred( short iOrdinal ) const { return iField[iOrdinal].isDeferred ; }
         inline int fieldPresent( short iOrdinal ) const { return ( iField[iOrdinal].iField >= 0 ) ; }
         inline int ordinalStart() ;
         inline int ordinalEnd() { return 0 ; }

         inline long offsetRaw( ULONG iOrdinal )
         {
            return field( iOrdinal )->offsetRaw() ;
         }

         /* made inline functions for easier debugging, several problems related to collations
            etc. involving this value.
         */
         inline void lenFieldsSet( ULONG setValue ) { lenFieldsVal = setValue ; }
         inline ULONG lenFieldsGet() { return lenFieldsVal ; }
         inline int nullBytesOffsetRaw() { return nullBytesOffsetRawVal ; }

         char *defaultEmptyRow ;
      private:
         ULONG lenFieldsVal ; // total length of nondeferred fields + sizeof pointers
                              // to deferred data and interfaces + length of null bytes.
         int initMembers() ;
         #ifndef S4SERVER
            void calculateLengthsAndNulls() ;
            void initializeRowBufferConversions( int isNullable ) ;
            void countFieldTypes() ;
            void sortFieldOrder( int maxField = -1 ) ;
            void initForIndex() ;
            void addFieldsFromOldFieldSets() ;
            void initIFields() ;
         #endif
      } ;

      class5 Fieldset5index : public Fieldset5all
      {
      public:
         Fieldset5index( Table5all *tbl ) : Fieldset5all( tbl ) { ; }
         void init( Accessor5handler *accParm, int isNullable ) ;
         int isIndex() { return 1 ; }
      } ;
   #endif /* !S4SERVER */
#endif /* OLEDB5BUILD */
