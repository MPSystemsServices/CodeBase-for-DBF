/* FILE class5factory.hpp
(c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

// Class Definitions for Class5Factory and DLL Entry Points
//
//
#ifndef _CLASS5FACTORY_HPP_
#define _CLASS5FACTORY_HPP_

#ifndef S4SERVER

extern CLSID CodeBaseClsid ;

// Classes -------------------------------------------------------------------


//----------------------------------------------------------------------------
// Class5Factory
//
class Class5Factory : public IClassFactory   //public | IClassFactory
{
   protected:
      // Reference count
      ULONG cRef;

   public:
      //Constructor
      Class5Factory(void);
      //Destructor
      ~Class5Factory(void);

      //IUnknown members
      //Request an Interface
      STDMETHODIMP QueryInterface(REFIID, LPVOID *);
      //Increments the Reference count
      STDMETHODIMP_(ULONG) AddRef(void);
      //Decrements the Reference count
      STDMETHODIMP_(ULONG) Release(void);

      //IClassFactory members
      //Instantiates an uninitialized instance of an object
      STDMETHODIMP CreateInstance(LPUNKNOWN, REFIID, LPVOID *);
      //Lock Object so that it can not be unloaded
      STDMETHODIMP LockServer(BOOL);
};

#endif // !S4SERVER

#endif
