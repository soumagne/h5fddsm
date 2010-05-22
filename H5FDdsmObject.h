/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmObject.h

  Authors:
     John Biddiscombe     Jerome Soumagne
     biddisco@cscs.ch     soumagne@cscs.ch

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  This work has received funding from the European Community's Seventh
  Framework Programme (FP7/2007-2013) under grant agreement 225967 “NextMuSE”

=========================================================================*/

/*=========================================================================
  This code is derived from an earlier work and is distributed
  with permission from, and thanks to ...
=========================================================================*/

/*******************************************************************/
/*                               XDMF                              */
/*                   eXtensible Data Model and Format              */
/*                                                                 */
/*                                                                 */
/*  Author:                                                        */
/*     Jerry A. Clarke                                             */
/*     clarke@arl.army.mil                                         */
/*     US Army Research Laboratory                                 */
/*     Aberdeen Proving Ground, MD                                 */
/*                                                                 */
/*     Copyright @ 2007 US Army Research Laboratory                */
/*     All Rights Reserved                                         */
/*     See Copyright.txt or http://www.arl.hpc.mil/ice for details */
/*                                                                 */
/*     This software is distributed WITHOUT ANY WARRANTY; without  */
/*     even the implied warranty of MERCHANTABILITY or FITNESS     */
/*     FOR A PARTICULAR PURPOSE.  See the above copyright notice   */
/*     for more information.                                       */
/*                                                                 */
/*******************************************************************/
#ifndef __H5FDdsmObject_h
#define __H5FDdsmObject_h

// use system checks from the HDF5 configure so that
// we can use the same int32/64 bit types etc
#include "H5FDdsmConfig.h"
#include "H5pubconf.h" 
#include <string.h>
#include <iostream>

//------------------------------------------------------------------------------
// Include stdint if possible to get 32/64 bit integers and setup types
//------------------------------------------------------------------------------
#ifdef H5_HAVE_STDINT_H
  #include <stdint.h>
    typedef int64_t Int64;
    typedef int32_t H5FDdsmInt32;
    typedef char    H5FDdsmByte;
    typedef int8_t  H5FDdsmInt8;
#else
  #ifdef _MSC_VER
    typedef signed   __int64 Int64;
    typedef signed   __int32 H5FDdsmInt32;
    typedef char             H5FDdsmByte;
    typedef signed   __int8  H5FDdsmInt8;
  #endif
#endif

typedef const char*   H5FDdsmConstString;
typedef char*         H5FDdsmString;
typedef H5FDdsmInt32  H5FDdsmBoolean;
typedef double        H5FDdsmFloat64;
typedef void*         H5FDdsmPointer;
typedef unsigned char H5FDdsmChar;

#define H5FD_DSM_SUCCESS  1
#define H5FD_DSM_FAIL    -1
#define H5FD_DSM_TRUE     1
#define H5FD_DSM_FALSE    0

//------------------------------------------------------------------------------
// Thread stuff used for mutex control with debug messages, to ensure that
// messages arrive in a readable manner, not overlapped and munged as two
// processes simultaneously write debug statements
//------------------------------------------------------------------------------
#ifdef WIN32 
  #ifndef H5FD_DSM_USE_WIN32_THREADS 
   #define H5FD_DSM_USE_WIN32_THREADS 
  #endif
#else 
  #ifndef H5FD_DSM_USE_PTHREADS 
   #define H5FD_DSM_USE_PTHREADS 
  #endif
#endif

#if defined(H5FD_DSM_USE_PTHREADS) || defined(H5FD_DSM_HP_PTHREADS)
#include <pthread.h> // Needed for PTHREAD implementation of mutex
typedef pthread_mutex_t H5FDdsmMutexType;
#endif
 
#ifdef H5FD_DSM_USE_WIN32_THREADS
  /* Include the real windows header. */
  #define NOMINMAX  
  #include <windows.h>
  typedef void* H5FDdsmMutexType;
#endif

//------------------------------------------------------------------------------
// Mutex lock : used for debug only
//------------------------------------------------------------------------------
class H5FDdsm_EXPORT SimpleMutexLock {
public:
   // left public purposely
   SimpleMutexLock();
  ~SimpleMutexLock();

  // Description:
  // Lock the vtkMutexLock
  void Lock( void );

  // Description:
  // Unlock the vtkMutexLock
  void Unlock( void );

protected:
  H5FDdsmMutexType MutexLock;
public:
  static SimpleMutexLock GlobalLock;
};

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
#define H5FDDebugIsOn ( this->Debug || H5FDdsmObject::GetGlobalDebug() )
#define H5FDDebugIsAbove(a)  ( ( this->Debug >= (a) ) || ( H5FDdsmObject::GetGlobalDebug() >= (a))) 

#define H5FDdsmDebug(x) \
{ if (H5FDDebugIsOn) { \
  SimpleMutexLock::GlobalLock.Lock();   \
  std::cout << "XDMF Debug : " << /*__FILE__ << " line " << __LINE__ << */ x << std::endl; \
  SimpleMutexLock::GlobalLock.Unlock(); \
  } \
}

#define H5FDdsmErrorMessage(x) \
  std::cout << "XDMF Error in " << __FILE__ << " line " << __LINE__ << " ("<< x << ")" << "\n";

#define H5FDdsmSetValueMacro(var,type) \
H5FDdsmInt32 Set##var (type _arg) \
  { \
  this->var = _arg; \
  return ( H5FD_DSM_SUCCESS ); \
  }

#define H5FDdsmSetStringMacro(var) \
H5FDdsmInt32 Set##var (H5FDdsmConstString _arg) \
  { \
  if ( this->var == _arg ) { return H5FD_DSM_SUCCESS; } \
  if ( this->var && _arg && strcmp(this->var, _arg) == 0 ) { return H5FD_DSM_SUCCESS; } \
  if ( this->var ) { delete [] this->var; this->var = 0; } \
  if ( _arg ) { this->var = new char[ strlen(_arg) + 1 ]; strcpy(this->var, _arg); } \
  return ( H5FD_DSM_SUCCESS ); \
  }

#define H5FDdsmGetStringMacro(var) \
H5FDdsmConstString Get##var () \
  { \
  return ( this->var ); \
  }

#define H5FDdsmSetIndexValueMacro(var,type) \
H5FDdsmInt32 Set##var ( Int64 Index, type _arg) \
  { \
  this->var[ Index ]  = _arg; \
  return ( H5FD_DSM_SUCCESS ); \
  }


#define H5FDdsmGetValueMacro(var,type) \
type Get##var () \
  { \
  return ( this->var ); \
  }

#define H5FDdsmGetIndexValueMacro(var,type) \
type Get##var (Int64 Index) \
  { \
  return ( this->var[ Index ]  ); \
  }

//! Base Class for Objects provides debug flags
class H5FDdsm_EXPORT H5FDdsmObject {
  public:
     H5FDdsmObject();
    ~H5FDdsmObject();

    H5FDdsmSetValueMacro(Debug, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(Debug, H5FDdsmBoolean);

    H5FDdsmBoolean GetGlobalDebug();
    void SetGlobalDebug( H5FDdsmBoolean Value );

    void DebugOn()  { H5FDdsmObject::SetDebug( 1 ) ; };
    void DebugOff() { H5FDdsmObject::SetDebug( 0 ) ; };

    void GlobalDebugOn()  { H5FDdsmObject::SetGlobalDebug( 1 ) ; };
    void GlobalDebugOff() { H5FDdsmObject::SetGlobalDebug( 0 ) ; };

  protected:
    H5FDdsmInt32 Debug;  
  private:
};

H5FDdsm_EXPORT void SetGlobalDebugOn();
H5FDdsm_EXPORT void SetGlobalDebugOff();

H5FDdsm_EXPORT H5FDdsmInt32 GetGlobalDebug( void );
H5FDdsm_EXPORT void SetGlobalDebug( H5FDdsmInt32 DebugLevel );

#endif /* __H5FDdsmObject_h */
