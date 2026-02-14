/* inline5.hpp   (c)Copyright Sequiter Software Inc., 1996.  All rights reserved. */

/* inline ole-db related codebase functions
   these functions are included at the end of the header compile because:
     1 - the inline function must be in a header file for Microsoft C to
         use them
     2 - the functions couldn't be included in place where they were because
         they required objects which were not defined yet
*/


#ifdef OLEDB5BUILD
   #ifdef S4STAND_ALONE
      inline int Index5stand::nullable()
      {
         return ((Table5all *)this)->columns()[1].isNullable ;
      }
   #endif /* S4STAND_ALONE */



   #ifdef S4STAND_ALONE
      inline unsigned short Index5stand::numFields()
      {
         return ((Table5all *)this)->nColumns() ;
      }
   #endif /* S4STAND_ALONE */



   #ifndef S4CLIENT
      inline int Index5low::isDeleted()
      {
      /*
         DESCRIPTION

         Returns TRUE if the index filter is set to '.NOT.DELETED()'

         NOTES

         Note that it will be preferable to analyze the E4INFO structure since
           we then do not need to worry about blanks and letter positioning, case
           sensitivity, etc.

         This function does the following:
           Call t4filter().
           Parse the expression.
           Examine the E4INFO structure to see if it is not deleted
      */

         return ( (tagFile->filter)->info[0].functionI == E4DEL ) ? 1 : 0 ;
      }
   #endif /* S4CLIENT */



   /*
   short Index5low::keyLen()
   {
      DESCRIPTION

      Returns the tags key length
      Can be written as an inline function

      NOTES

      made inline ;

      return tagFile->header.keyLen ;
   }
   */



   #ifndef S4SERVER
      inline int Fieldset5all::ordinalStart()
      {
         // this function is used by the field iterator class as a starting point from which
         // to iterate through the 'fields' member of the Fieldset5all structure.
         // there are as many field elements as there are actual fields bound to the fieldset.
         // notice that this differs from niField which is the # of bindings (some fields may
         // be bound twice).

         // therefore, we want to start the fieldset access at nFields
         // we don't subtract 1 because the iterator next() must be called on usage, and that pre-subtracts 1
         return numFields() ;

         // return accessorHandler->getNumFields() + 1 ;
      }
   #endif




   #ifndef S4SERVER
      inline void RowList5::add( Row5 *row )
      {
         ( !( isIdentity && rowListIsSorted ) ) ? List5::add( row ) : addDo( row ) ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT RowList5::find( Row5all *row )
      {
         return( ( seek( row ) == 1 ) ? S_OK : E_FAIL ) ;
      }
   #endif



   #ifndef S4SERVER
      inline void Rowset5::removeIndexes()
      {
         // can't be inline due to table not being defined yet
         tableDbf->removeIndexes() ;
      }
   #endif



   #ifndef S4SERVER
      inline int Rowset5::rowHandleInvalid( Row5 *row )
      {
         return ( ( row == DB_NULL_HROW
            #ifdef S4CONFORMANCE
               // AS 04/23/98 conformance tests require this.
               || verifyRowHandle( (Row5all *)row ) != S_OK
            #endif
            ) ? 1 : 0 ) ;
      }
   #endif



   #ifndef S4SERVER
      inline void Rowset5index::freeRow( Row5all *row )
      {
         // AS 01/09/00 - was not freeing left-over rows, so fixed this...
         rowList.remove( (Row5index *)row ) ;
         delete ( (Row5index *)row ) ;
      }
   #endif



   #ifndef S4SERVER
      inline int Rowset5indexPlusIntegratedIndex::isNullable()
      {
         // CJ 04/07/99 - (AS comment) - if integrated index but no tag selected, index was non-existent,
         // so failing.  In that case can never be nullable, so just return 0.
         if (index)
            return index->nullable() ;
         else
            return 0 ;
      }
   #endif



   #ifndef S4SERVER
      inline int Rowset5indexPlusIntegratedIndex::isDeleted()
      {
         return index->isDeleted() ;
      }
   #endif



   #ifndef S4SERVER
      inline short Rowset5indexPlusIntegratedIndex::keyLen()
      {
         return index->keyLen() ;
      }
   #endif



   #ifndef S4SERVER
      inline int Rowset5indexPlusIntegratedIndex::rowGet( int direction, unsigned long *recNo, void *key, Table5rowsetValues *rowsetValues, Index5rangeInfo *rangeData )
      {
         return index->rowGet( direction, recNo, key, rowsetValues, rangeData ) ;
      }
   #endif



   #ifndef S4SERVER
      inline void Rowset5indexPlusIntegratedIndex::refetchWithKey( unsigned long recNo, char *keyBuf )
      {
         index->refetchWithKey( recNo, keyBuf ) ;
      }
   #endif



   #ifndef S4SERVER
      inline void Rowset5indexPlusIntegratedIndex::remove()
      {
         index->remove() ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT Rowset5index::verifyRowHandle( Row5all *row )
      {
         return S_OK ;
      }
   #endif



   #ifdef S4SERVER
      inline long Fieldset5server::offsetRaw( ULONG iOrdinal )
      {
         return table->getFieldInfoOffset( iOrdinal ) ;
      }
   #endif



   #ifdef S4SERVER
      inline DATA4 *Table5serverBase::data4()
      {
         return table->data4() ;
      }
   #endif



   #ifdef S4SERVER
      inline COLUMN5INFO *Table5serverBase::getColumns( short *nColumns, int *freeRequired )
      {
         return table->getColumns( nColumns, freeRequired ) ;
      }
   #endif



   #ifndef S4SERVER
      inline void Table5::rowRequestSkipNext( Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent )
      {
         // must implement if implementing rowRequestGetNext for sequential rowsets
         throw Err5internal( sessionCache->c4) ;
      }



      inline Schema5type Table5::schemaType()
      {
         // will be overrident if must be available (i.e. schema tables)
         throw Err5internal(sessionCache->c4) ;
         return schema5typeInvalid ;
      }


      inline char *Table5::generateTagName()
      {
          // classes which allow overload
         throw Err5internal( sessionCache->c4) ;
         return 0 ;
      }



      inline HRESULT Table5::find( Accessor5all *a5, unsigned long *startRowNo, const void *seekKey, int seekValueLen,
              int skipAmount, char doConversion, DBCOMPAREOP CompareOp, int fieldOrdinal, int direction )
      {
          // default for some is should never get called
         throw Err5internal(sessionCache->c4) ;
         return S_OK ;
      }

   #endif



   #ifdef S4STAND_ALONE
      inline ULONG Table5schemaTag::recNo()
      {
         long val = tfile4recNo( tagFile() ) ;
         if ( val <= 0 ) throw Err5internal( (data4())->codeBase ) ;
         return val ;
      }



      inline Schema5type Table5schemaTag::schemaType()
      {
         // tag tables should not need to return schema types
         throw Err5internal((data4())->codeBase) ;
         return schema5typeInvalid ;
      }
   #endif



   #ifdef S4SERVER
      inline long Table5serverBase::seek( const char *key, short len, int descend )
      {
         return table->seek( key, len, descend ) ;
      }
   #endif



   #ifdef S4SERVER
      inline void Table5serverBase::tagSelect( char *tagName, int *iRowset )
      {
         table->tagSelect( tagName, iRowset ) ;
      }
   #endif



   #ifndef S4SERVER
      inline COLUMN5INFO *Rowset5index::columnsRowset( short *nColumns )
      {
         // the rowset is the same as the index for Rowset5index
         return index->columns( nColumns ) ;
      }
   #endif



   #ifndef S4SERVER
      inline COLUMN5INFO *Rowset5integratedIndex::columnsRowset( short *nColumns )
      {
         return tableDbf->columns( nColumns ) ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT Accessor5rowset::init( Rowset5all *rowsetParm, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] )
      {
         return initDo( &rowsetParm->accessorHandler, dwAccessorFlags, cBindings, rgBindings, cbRowSize, rgStatus ) ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT Accessor5rowset::initDo( Accessor5handler *accHandler, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] )
      {
         return Accessor5all::initLow( accHandler, dwAccessorFlags, cBindings, rgBindings, cbRowSize, rgStatus ) ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT Accessor5index::init( Rowset5all *rowsetParm, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] )
      {
         return initDo( &rowsetParm->accessorHandler, dwAccessorFlags, cBindings, rgBindings, cbRowSize, rgStatus ) ;
      }
   #endif



   #ifndef S4SERVER
      inline HRESULT Accessor5index::initDo( Accessor5handler *accHandler, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] )
      {
         HRESULT h = Accessor5all::initLow( accHandler, dwAccessorFlags, cBindings, rgBindings, cbRowSize, rgStatus ) ;
         if (FAILED(h))
            return ( h ) ;
         if ( cBindings == 0 )
            nullFieldsetOK = 1 ;
         else
         {
            if ( cBindings == 1 )
               if ( rgBindings[0].iOrdinal == 1 )
                  nullFieldsetOK = 1 ;
         }
         return h ;
      }
   #endif



   //inline COLUMN5INFO *Rowset5index::getIndexColumns( short *nColumns )
   //{
   //   return accessorHandler.tableAll->getColumns( nColumns ) ;
   //}



   #ifndef S4SERVER
      inline ULONG RowsetPlusSchema5::baseTableCount()
      {
         return tableDbf->recCount() ;
      }
   #endif /* S4SERVER */



   #ifndef S4SERVER
      inline HRESULT Accessor5all::dataRead( Row5all *row, char *pConsumerBuffer )
      {
         /* AS 09/03/99 changed bufferToConsumer to return HRESULT not numErrors because numErrors is not needed
            by caller, and this and that function must run AFAP */
         return stepsRead.bufferToConsumer( row->pBuffer(), pConsumerBuffer, nBindings ) ;
      }
   #endif /* S4SERVER */


   inline short Field5iterator::currentOrdinal()
   {
      return fieldsetAll->fieldOrdinalFromFieldArrayNumber( entryOn ) ;
   }

   #ifdef S4SERVER
      inline Session5 *Table5low::session()
      {
         return c4getCurrentClient( source()->c4 )->session ;
      }
   #endif

   #ifdef S4SERVER
      inline Field5iterator::Field5iterator( Fieldset5server *fieldsetParm )
   #else
      inline Field5iterator::Field5iterator( Fieldset5all *fieldsetParm )
   #endif
   {
      fieldset = fieldsetParm ; fieldsetAll = fieldsetParm ;
      startEntry = entryOn = fieldsetAll->ordinalStart() ;
      lastEntry = fieldsetAll->ordinalEnd() ;
   }
#endif /* OLEDB5BUILD */
