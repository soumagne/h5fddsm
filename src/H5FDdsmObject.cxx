/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmObject.cxx

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
#include "H5FDdsmObject.h"
#include <string.h>

//------------------------------------------------------------------------------
// Base H5FDdsm class definition
//------------------------------------------------------------------------------
H5FDdsmObject::H5FDdsmObject() {
  this->Debug = 0;
}

H5FDdsmObject::~H5FDdsmObject() {
}

#ifdef H5FD_DSM_DEBUG_SYNCED
DebugLock DebugLock::GlobalLock;

// Construct a new vtkMutexLock 
DebugLock::DebugLock()
{
#ifdef _WIN32
  this->MutexLock = CreateMutex(NULL, FALSE, NULL);
#else
  pthread_mutex_init(&(this->MutexLock), NULL);
#endif
}
//------------------------------------------------------------------------------
DebugLock::~DebugLock()
{
#ifdef _WIN32
  CloseHandle(this->MutexLock);
#else
  pthread_mutex_destroy(&this->MutexLock);
#endif
}
//------------------------------------------------------------------------------
void DebugLock::Lock()
{
#ifdef _WIN32
  WaitForSingleObject(this->MutexLock, INFINITE);
#else
  pthread_mutex_lock(&this->MutexLock);
#endif
}
//------------------------------------------------------------------------------
void DebugLock::Unlock()
{
#ifdef _WIN32
  ReleaseMutex(this->MutexLock);
#else
  pthread_mutex_unlock(&this->MutexLock);
#endif
}
#endif // H5FD_DSM_DEBUG_SYNCED
