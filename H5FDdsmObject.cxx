/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmObject.cxx

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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
#include "H5FDdsmObject.h"
#include <string.h>

static H5FDdsmInt32 GlobalDebugFlag = 0;

//------------------------------------------------------------------------------
// Base H5FDdsm class definition from H5FDdsm originally
//------------------------------------------------------------------------------
H5FDdsmObject::H5FDdsmObject() {
  this->Debug = 0;
}

H5FDdsmObject::~H5FDdsmObject() {
}

H5FDdsmInt32 H5FDdsmObject::GetGlobalDebug() {
  return GlobalDebugFlag;
}

void H5FDdsmObject::SetGlobalDebug( H5FDdsmInt32 Value ) {
  GlobalDebugFlag = Value;
}

H5FDdsmInt32 GetGlobalDebug() {
  return GlobalDebugFlag;
}

void SetGlobalDebug( H5FDdsmInt32 Value ) {
  GlobalDebugFlag = Value;
}

void SetGlobalDebugOn() {
  GlobalDebugFlag = 1;
}

void SetGlobalDebugOff() {
  GlobalDebugFlag = 0;
}

//------------------------------------------------------------------------------
// Simple Mutex code taken from VTK
//------------------------------------------------------------------------------
SimpleMutexLock SimpleMutexLock::GlobalLock;

// Construct a new vtkMutexLock 
SimpleMutexLock::SimpleMutexLock()
{
#ifdef H5FD_DSM_USE_SPROC
  init_lock( &this->MutexLock );
#endif

#ifdef H5FD_DSM_USE_WIN32_THREADS
  this->MutexLock = CreateMutex( NULL, FALSE, NULL ); 
#endif

#ifdef H5FD_DSM_USE_PTHREADS
#ifdef H5FD_DSM_HP_PTHREADS
  pthread_mutex_init(&(this->MutexLock), pthread_mutexattr_default);
#else
  pthread_mutex_init(&(this->MutexLock), NULL);
#endif
#endif
}
//------------------------------------------------------------------------------
SimpleMutexLock::~SimpleMutexLock()
{
#ifdef H5FD_DSM_USE_WIN32_THREADS
  CloseHandle(this->MutexLock);
#endif

#ifdef H5FD_DSM_USE_PTHREADS
  pthread_mutex_destroy( &this->MutexLock);
#endif
}
//------------------------------------------------------------------------------
void SimpleMutexLock::Lock()
{
#ifdef H5FD_DSM_USE_SPROC
  spin_lock( &this->MutexLock );
#endif

#ifdef H5FD_DSM_USE_WIN32_THREADS
  WaitForSingleObject( this->MutexLock, INFINITE );
#endif

#ifdef H5FD_DSM_USE_PTHREADS
  pthread_mutex_lock( &this->MutexLock);
#endif
}
//------------------------------------------------------------------------------
void SimpleMutexLock::Unlock()
{
#ifdef H5FD_DSM_USE_SPROC
  release_lock( &this->MutexLock );
#endif

#ifdef H5FD_DSM_USE_WIN32_THREADS
  ReleaseMutex( this->MutexLock );
#endif

#ifdef H5FD_DSM_USE_PTHREADS
  pthread_mutex_unlock( &this->MutexLock);
#endif
}
