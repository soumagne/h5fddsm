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
    typedef int64_t  H5FDdsmInt64;
    typedef int32_t  H5FDdsmInt32;
    typedef int8_t   H5FDdsmInt8;
    typedef uint64_t H5FDdsmUInt64;
    typedef uint32_t H5FDdsmUInt32;
    typedef uint8_t  H5FDdsmUInt8;
#else
  #ifdef _MSC_VER
    typedef signed   __int64 H5FDdsmInt64;
    typedef signed   __int32 H5FDdsmInt32;
    typedef signed   __int8  H5FDdsmInt8;
    typedef unsigned __int64 H5FDdsmUInt64;
    typedef unsigned __int32 H5FDdsmUInt32;
    typedef unsigned __int8  H5FDdsmUInt8;
  #endif
#endif

typedef char          H5FDdsmByte;
typedef const char*   H5FDdsmConstString;
typedef char*         H5FDdsmString;
typedef H5FDdsmUInt32 H5FDdsmBoolean;
typedef float         H5FDdsmFloat32;
typedef double        H5FDdsmFloat64;
typedef void*         H5FDdsmPointer;
typedef H5FDdsmUInt64 H5FDdsmAddr;

#define H5FD_DSM_INT32_MAX (2147483647)

#define H5FD_DSM_SUCCESS  1
#define H5FD_DSM_FAIL    -1
#define H5FD_DSM_TRUE     1
#define H5FD_DSM_FALSE    0

//------------------------------------------------------------------------------
// Below is all missing in non debug build
//------------------------------------------------------------------------------
#ifdef H5FDdsm_DEBUG_SYNCED
#ifdef _WIN32
  #include <winsock2.h>
  #include <windows.h>
#else
  #include <pthread.h>
#endif
//------------------------------------------------------------------------------
// Thread stuff used for mutex control with debug messages, to ensure that
// messages arrive in a readable manner, not overlapped and munged as multiple
// processes simultaneously write debug statements
//------------------------------------------------------------------------------
class H5FDdsm_EXPORT DebugLock {
public:
   DebugLock();
  ~DebugLock();

  void Lock();
  void Unlock();

protected:
#ifdef _WIN32
  HANDLE MutexLock;
#else
  pthread_mutex_t MutexLock;
#endif

public:
  static DebugLock GlobalLock;
};
#endif // H5FDdsm_DEBUG_SYNCED

//------------------------------------------------------------------------------
// Error and Debug message Macros
//------------------------------------------------------------------------------
#ifdef H5FDdsm_DEBUG_SYNCED
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebug(x) \
{ DebugLock::GlobalLock.Lock();   \
  std::cout << "H5FD_DSM Debug : " << x << std::endl; \
  DebugLock::GlobalLock.Unlock(); \
}
#else
#define H5FDdsmDebug(x) \
{ if (this->Debug) { \
    DebugLock::GlobalLock.Lock();   \
    std::cout << "H5FD_DSM Debug : " << x << std::endl; \
    DebugLock::GlobalLock.Unlock(); \
  } \
}
#endif

#define H5FDdsmError(x) \
{ DebugLock::GlobalLock.Lock();   \
  std::cout << "H5FD_DSM Error : " __FILE__ << " line " << __LINE__ << ": " << x << std::endl; \
  DebugLock::GlobalLock.Unlock(); \
}
#else

#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebug(x) \
{ \
  std::cout << "H5FD_DSM Debug : " << x << std::endl; \
}
#else
#define H5FDdsmDebug(x) \
{ if (this->Debug) { \
    std::cout << "H5FD_DSM Debug : " << x << std::endl; \
  } \
}
#endif

#define H5FDdsmError(x) \
{ \
  std::cout << "H5FD_DSM Error : " __FILE__ << " line " << __LINE__ << ": " << x << std::endl; \
}
#endif
//------------------------------------------------------------------------------
// Set/Get Macros
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Base Class for H5FDdsm Objects : provides debug flags
//------------------------------------------------------------------------------
class H5FDdsm_EXPORT H5FDdsmObject {
public:
  H5FDdsmObject();
  ~H5FDdsmObject();

  H5FDdsmSetValueMacro(Debug, H5FDdsmBoolean);
  H5FDdsmGetValueMacro(Debug, H5FDdsmBoolean);

  void DebugOn()  { H5FDdsmObject::SetDebug(1); }
  void DebugOff() { H5FDdsmObject::SetDebug(0); }

protected:
  H5FDdsmInt32 Debug;
private:
};

#endif // __H5FDdsmObject_h
