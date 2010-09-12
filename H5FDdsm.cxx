/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsm.cxx

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

#define H5_INTERFACE_INIT_FUNC  H5FD_dsm_init_interface

#include "mpi.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "H5private.h"    // Generic Functions
#include "H5ACprivate.h"  // Metadata cache
#include "H5Dprivate.h"   // Dataset functions
#include "H5Eprivate.h"   // Error handling
#include "H5Fprivate.h"   // File access
#include "H5FDprivate.h"  // File drivers
#include "H5FDdsm.h"      // MPI-based file drivers
#include "H5Iprivate.h"   // IDs
#include "H5MMprivate.h"  // Memory management
#include "H5Pprivate.h"   // Property lists

#include "H5FDmpio.h"

#ifdef __cplusplus
}
#endif

// H5private.h defines attribute, but we don't want it as it causes link errors on some gcc versions
#ifdef __GNUC__
# undef __attribute__
#endif

//--------------------------------------------------------------------------
// Unfortunately, some of the HDF5 macros use internal variables which are not exported
// from the hdf5 lib/dll so we must override the macros and lose some debugging info

#undef FUNC_ENTER_NOAPI_NOINIT_NOFUNC
#define FUNC_ENTER_NOAPI_NOINIT_NOFUNC(a)

#undef FUNC_ENTER_API
#define FUNC_ENTER_API(a,b)

#undef FUNC_LEAVE_API
#define FUNC_LEAVE_API(a) return a;

#undef FUNC_ENTER_NOAPI
#define FUNC_ENTER_NOAPI(a,b)

#undef FUNC_LEAVE_NOAPI
#define FUNC_LEAVE_NOAPI(a) return a;

#undef FUNC_LEAVE_NOAPI_VOID
#define FUNC_LEAVE_NOAPI_VOID

#undef HGOTO_ERROR
#define HGOTO_ERROR(a,b,c,d) { printf("%s",d); goto done; }
//--------------------------------------------------------------------------
//
#include "H5FDdsmManager.h"
//
#ifdef H5_HAVE_PARALLEL

// #define H5FD_DSM_DEBUG
#ifdef H5FD_DSM_DEBUG
#  define PRINT_INFO(x) std::cout << "(" << file->DsmBuffer->GetComm()->GetId() << ") " << x << std::endl;
#  define PRINT_DSM_INFO(a,x) std::cout << "(" << a << ") " << x << std::endl;
#else
#  define PRINT_INFO(x)
#  define PRINT_DSM_INFO(a,x)
#endif

#undef MAX
#define MAX(X,Y)  ((X)>(Y)?(X):(Y))
#undef MIN
#define MIN(X,Y)  ((X)<(Y)?(X):(Y))

//--------------------------------------------------------------------------

// The driver identification number, initialized at runtime
static hid_t H5FD_DSM_g = 0;

// @TODO Warning, the use of static objects here is very dangerous!
// This H5FDdsmManager is used only when no DsmBuffer
// is passed to set_fapl_dsm function
static H5FDdsmManager *dsmManagerSingleton = NULL;

//--------------------------------------------------------------------------
/*
 * The description of a file belonging to this driver. The `eoa' and `eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying memory).
 */
typedef struct H5FD_dsm_t
{
  H5FD_t pub; // public stuff, must be first
  char *name; // for equivalence testing
  H5FDdsmBuffer *DsmBuffer;
  haddr_t eoa; // end of allocated region
  haddr_t eof; // current allocated size
  size_t increment; // multiples for mem allocation
  haddr_t entry_addr; // DSM Address of this entry
  haddr_t start; // Current DSM Start Address
  haddr_t end; // Current DSM End Address
  hbool_t dirty; // changes not saved?
} H5FD_dsm_t;

//--------------------------------------------------------------------------
// Driver-specific file access properties
typedef struct H5FD_dsm_fapl_t
{
  size_t increment; // how much to grow memory
  H5FDdsmBuffer *buffer; // Default DSM Buffer
} H5FD_dsm_fapl_t;

//--------------------------------------------------------------------------
/*
 * These macros check for overflow of various quantities.  These macros
 * assume that file_offset_t is signed and haddr_t and size_t are unsigned.
 *
 * ADDR_OVERFLOW:  Checks whether a file address of type `haddr_t'
 *      is too large to be represented by the second argument
 *      of the file seek function.
 *
 * SIZE_OVERFLOW:  Checks whether a buffer size of type `hsize_t' is too
 *      large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW:  Checks whether an address and size pair describe data
 *      which can be addressed entirely in memory.
 */
#if (H5_VERS_MAJOR>1)||((H5_VERS_MAJOR==1)&&(H5_VERS_MINOR==6))
#  ifdef H5_HAVE_LSEEK64
#    define file_offset_t        off64_t
#    define file_seek            lseek64
#    define file_truncate        ftruncate64
#  elif defined (WIN32) && !defined(__MWERKS__)
#    // MSVC
#    define file_offset_t __int64
#    define file_seek _lseeki64
#    define file_truncate        _ftruncatei64
#  else
#    define file_offset_t        off_t
#    define file_seek            lseek
#    define file_truncate        HDftruncate
#  endif
#  define MAXADDR (((haddr_t)1<<(8*sizeof(file_offset_t)-1))-1)
#  define DSM_HSIZE_T size_t
#else
#  define MAXADDR     ((haddr_t)((~(size_t)0)-1))
#  define DSM_HSIZE_T size_t
//#  define DSM_HSIZE_T hsize_t
#endif
#define ADDR_OVERFLOW(A)        (HADDR_UNDEF==(A) || (A) > (haddr_t)MAXADDR)
#define SIZE_OVERFLOW(Z)        ((Z) > (hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||      \
                                 HADDR_UNDEF==(A)+(Z) ||                      \
                                 (size_t)((A)+(Z))<(size_t)(A))

//--------------------------------------------------------------------------
// Private Callback Prototypes used within driver
//--------------------------------------------------------------------------

static void    *H5FD_dsm_fapl_get(H5FD_t *_file);
static H5FD_t  *H5FD_dsm_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t   H5FD_dsm_close(H5FD_t *_file);
static int      H5FD_dsm_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static haddr_t  H5FD_dsm_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t   H5FD_dsm_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t  H5FD_dsm_get_eof(const H5FD_t *_file);
static herr_t   H5FD_dsm_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, DSM_HSIZE_T size, void *buf);
static herr_t   H5FD_dsm_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, DSM_HSIZE_T size, const void *buf);
static herr_t   H5FD_dsm_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);

// MPI specific ones
static int      H5FD_dsm_mpi_rank(const H5FD_t *_file);
static int      H5FD_dsm_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD_dsm_communicator(const H5FD_t *_file);

//--------------------------------------------------------------------------
// The H5FD_class_mpi_t Callback structure which 'defines' our driver
//--------------------------------------------------------------------------
static const H5FD_class_mpi_t H5FD_dsm_g = {
    {
        "dsm",                    // name
        MAXADDR,                  // maxaddr
        H5F_CLOSE_SEMI,           // fc_degree
        NULL,                     // sb_size
        NULL,                     // sb_encode
        NULL,                     // sb_decode
        sizeof(H5FD_dsm_fapl_t),  // fapl_size
        H5FD_dsm_fapl_get,        // fapl_get
        NULL,                     // fapl_copy
        NULL,                     // fapl_free
        0,                        // dxpl_size
        NULL,                     // dxpl_copy
        NULL,                     // dxpl_free
        H5FD_dsm_open,            // open
        H5FD_dsm_close,           // close
        H5FD_dsm_cmp,             // cmp
        H5FD_dsm_query,           // query
        NULL,                     // get_type_map
        NULL,                     // alloc
        NULL,                     // free
        H5FD_dsm_term,            // terminate
        H5FD_dsm_get_eoa,         // get_eoa
        H5FD_dsm_set_eoa,         // set_eoa
        H5FD_dsm_get_eof,         // get_eof
        NULL,                     // get_handle
        H5FD_dsm_read,            // read
        H5FD_dsm_write,           // write
        NULL,                     // flush
        NULL,                    	// truncate
        NULL,                     // lock
        NULL,                     // unlock
        H5FD_FLMAP_SINGLE         // fl_map
    },
    H5FD_dsm_mpi_rank, // get_rank
    H5FD_dsm_mpi_size, // get_size
    H5FD_dsm_communicator // get_comm
};

//--------------------------------------------------------------------------
typedef struct
{
  H5FDdsmInt64 start;
  H5FDdsmInt64 end;
} DsmEntry;
//--------------------------------------------------------------------------
H5FDdsmInt32
DsmUpdateEntry(H5FD_dsm_t *file)
{
  H5FDdsmInt64 addr;
  DsmEntry entry;

  PRINT_INFO("DsmUpdateEntry()");

  if (!file->DsmBuffer) return (H5FD_DSM_FAIL);

  file->end = MAX((H5FDdsmInt64)(file->start + file->eof), (H5FDdsmInt64)file->end);
  file->eof = file->end - file->start;

  entry.start = file->start;
  entry.end = file->end;
  addr = file->DsmBuffer->GetTotalLength() - sizeof(entry) - sizeof(H5FDdsmInt64);

  PRINT_INFO("DsmUpdateEntry start " <<
      file->start <<
      " end " << file->end <<
      " addr " << addr);

  // Only one of the processes writing to the DSM needs to write file metadata
  // but we must be careful that all the processes keep the metadata synchronized
  // Do not send anything if the end of the file is 0
  if ((file->DsmBuffer->GetComm()->GetId() == 0) && (entry.end > 0)) {
    if (file->DsmBuffer->Put(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  file->DsmBuffer->GetComm()->Barrier();
  //else {
    // Send is non blocking, make sure it's there
    //if (file->DsmBuffer->Get(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  //}
  return H5FD_DSM_SUCCESS;
}
//--------------------------------------------------------------------------
H5FDdsmInt32
DsmGetEntry(H5FD_dsm_t *file)
{
  H5FDdsmInt64 addr;
  DsmEntry entry;

  PRINT_INFO("DsmGetEntry()");

  if (!file->DsmBuffer) return (H5FD_DSM_FAIL);

  addr = file->DsmBuffer->GetTotalLength() - sizeof(DsmEntry) - sizeof(H5FDdsmInt64);

  if (file->DsmBuffer->Get(addr, sizeof(DsmEntry), &entry) != H5FD_DSM_SUCCESS) {
    PRINT_INFO("DsmGetEntry failed");
    return H5FD_DSM_FAIL;
  }

  file->start = entry.start;
  file->end = entry.end;

  PRINT_INFO("DsmGetEntry start " <<
      file->start <<
      " end " << file->end <<
      " addr " << addr);

  return H5FD_DSM_SUCCESS;
}
/*--------------------------------------------------------------------------
 NAME
 H5FD_dsm_init_interface -- Initialize interface-specific information
 USAGE
 herr_t H5FD_dsm_init_interface()

 RETURNS
 Non-negative on success/Negative on failure
 DESCRIPTION
 Initializes any interface-specific data or routines.  (Just calls
 H5FD_dsm_init currently).

 --------------------------------------------------------------------------*/
static herr_t
H5FD_dsm_init_interface(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_init_interface)

  FUNC_LEAVE_NOAPI(H5FD_dsm_init());
} // H5FD_dsm_init_interface()

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_init
 *
 * Purpose:  Initialize this driver by registering the driver with the
 *    library.
 *
 * Return:  Success:  The driver ID for the dsm driver.
 *
 *    Failure:  Negative.
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_dsm_init(void)
{
  hid_t ret_value = H5FD_DSM_g; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_init, FAIL)

  if (H5I_VFL != H5Iget_type(H5FD_DSM_g)) {
    H5FD_DSM_g = H5FD_register(&H5FD_dsm_g, sizeof(H5FD_class_mpi_t), FALSE );
  }

  // Set return value
  ret_value = H5FD_DSM_g;

  FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_term
 *
 * Purpose:  Shut down the driver
 *
 * Return:  <none>
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_dsm_term(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_term)

  if (dsmManagerSingleton) {
    if (!dsmManagerSingleton->GetDsmIsServer() && dsmManagerSingleton->GetDSMHandle()->GetIsConnected()) {
      dsmManagerSingleton->DisconnectDSM();
    }
    if (dsmManagerSingleton->GetDsmIsServer()) {
      dsmManagerSingleton->UnpublishDSM();
    }
    delete dsmManagerSingleton;
    dsmManagerSingleton = NULL;
  }

  // Reset VFL ID
  H5FD_DSM_g = 0;

  FUNC_LEAVE_NOAPI_VOID
} // end H5FD_dsm_term()

/*-------------------------------------------------------------------------
 * Function:  H5Pset_fapl_dsm
 *
 * Purpose:  Modify the file access property list to use the H5FDdsm
 *    driver defined in this source file.  The INCREMENT specifies
 *    how much to grow the memory each time we need more.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_dsm(hid_t fapl_id, MPI_Comm dsmComm, void *dsmBuffer)
{
  H5FD_dsm_fapl_t fa;
  herr_t ret_value = SUCCEED;
  H5P_genplist_t *plist; // Property list pointer

  FUNC_ENTER_API(H5Pset_fapl_dsm, FAIL)

  // Check argument
  if (NULL == (plist = (H5P_genplist_t*) H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

  fa.increment = H5FD_DSM_INCREMENT;

  if (dsmBuffer) {
    fa.buffer = static_cast <H5FDdsmBuffer*> (dsmBuffer);
  }
  else {
    if (dsmManagerSingleton == NULL) {
      dsmManagerSingleton = new H5FDdsmManager();
      dsmManagerSingleton->ReadDSMConfigFile();
      dsmManagerSingleton->SetCommunicator(dsmComm);
      dsmManagerSingleton->CreateDSM();
      if (dsmManagerSingleton->GetDsmIsServer()) {
        dsmManagerSingleton->PublishDSM();
        while (!dsmManagerSingleton->GetDSMHandle()->GetIsConnected()) {
          // Spin
        }
      } else {
        dsmManagerSingleton->ConnectDSM();
      }
      dsmManagerSingleton->GetDSMHandle()->SetIsAutoAllocated(true);
    }
    fa.buffer = dsmManagerSingleton->GetDSMHandle();
  }
  PRINT_DSM_INFO(fa.buffer->GetComm()->GetId(), "Get Write to DSM value: " << fa.buffer->GetSteerer()->GetWriteToDSM());
  if (!fa.buffer->GetSteerer()->GetWriteToDSM()) {
    PRINT_DSM_INFO(fa.buffer->GetComm()->GetId(), "Using MPIO driver temporarily");
    H5Pset_fapl_mpio(fapl_id, dsmComm, MPI_INFO_NULL);
    // next time step will go back to the DSM
    fa.buffer->GetSteerer()->SetWriteToDSM(1);
    goto done;
  }
  ret_value = H5P_set_driver(plist, H5FD_DSM, &fa);

  done: FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5Pget_fapl_dsm
 *
 * Purpose:  Queries properties set by the H5Pset_fapl_dsm() function.
 *
 * Return:  Success:  Non-negative
 *
 *    Failure:  Negative
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_dsm(hid_t fapl_id, MPI_Comm *dsmComm/*out*/, void **dsmBuffer /* out */)
{
  H5FD_dsm_fapl_t *fa;
  H5P_genplist_t *plist; // Property list pointer
  herr_t ret_value = SUCCEED; // Return value

  FUNC_ENTER_API(H5Pget_fapl_dsm, FAIL)

  if (NULL == (plist = (H5P_genplist_t*) H5P_object_verify(fapl_id,
      H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
  if (H5FD_DSM != H5P_get_driver(plist))
    HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
  if (NULL == (fa = (H5FD_dsm_fapl_t*) H5P_get_driver_info(plist)))
    HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info");

  if (dsmComm) {
    if (fa->buffer->GetComm()->GetCommType() == H5FD_DSM_COMM_SOCKET) {
      *dsmComm = dynamic_cast <H5FDdsmCommSocket*> (fa->buffer->GetComm())->GetComm();
    }
    else if (fa->buffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI) {
      *dsmComm = dynamic_cast <H5FDdsmCommMpi*> (fa->buffer->GetComm())->GetComm();
    }
  }
  if (dsmBuffer) *dsmBuffer = fa->buffer;

  done: FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_fapl_get
 *
 * Purpose:  Returns a copy of the file access properties.
 *
 * Return:  Success:  Ptr to new file access properties.
 *
 *    Failure:  NULL
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_dsm_fapl_get(H5FD_t *_file)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  H5FD_dsm_fapl_t *fa = (H5FD_dsm_fapl_t *) calloc(1, sizeof(H5FD_dsm_fapl_t));
  void *ret_value = NULL; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_fapl_get, NULL)

  PRINT_INFO("Fapl Get");

  if (NULL == (fa = (H5FD_dsm_fapl_t*) calloc((size_t)1, sizeof(H5FD_dsm_fapl_t))))
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

  fa->increment = file->increment;
  fa->buffer = file->DsmBuffer;

  // Set return value
  ret_value = fa;

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_open
 *
 * Purpose:  Create memory as an HDF5 file.
 *
 * Return:  Success:  A pointer to a new file data structure. The
 *        public fields will be initialized by the
 *        caller, which is always H5FD_open().
 *
 *    Failure:  NULL
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_dsm_open(const char *name, unsigned UNUSED flags, hid_t fapl_id, haddr_t maxaddr)
{
  H5FD_dsm_t *file = NULL;
  H5FD_dsm_fapl_t *fa = NULL;
  H5P_genplist_t *plist; // Property list pointer
  H5FD_t *ret_value = NULL;


  FUNC_ENTER_NOAPI(H5FD_dsm_open, NULL)

  // Check arguments
  if (0 == maxaddr || HADDR_UNDEF == maxaddr) {
    HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr");
  }
  if (ADDR_OVERFLOW(maxaddr)) {
    HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "maxaddr overflow");
  }
  if (H5P_DEFAULT != fapl_id) {
    if (NULL == (plist = (H5P_genplist_t*) H5I_object(fapl_id))) {
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");
    }
    fa = (H5FD_dsm_fapl_t*) H5P_get_driver_info(plist);
  } // end if

  // Create the new file struct
  if (NULL == (file = (H5FD_dsm_t *) calloc((size_t)1, sizeof(H5FD_dsm_t)))) {
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct");
  }

  if (name && *name) {
    file->name = HDstrdup(name);
  }

  // See if DsmBuffer exists
  if (!fa->buffer) {
    HGOTO_ERROR(H5E_VFL, H5E_NOTFOUND, NULL, "DSM buffer not found");
  }
  else {
    file->DsmBuffer = fa->buffer;

    if (file->DsmBuffer->GetIsServer()) {

      if (H5F_ACC_CREAT & flags) {
        // This is a critical part, we need to synchronize before
        // and after the ClearStorage method call
        PRINT_INFO("Clear DSM before create");
        file->DsmBuffer->GetComm()->Barrier();
        file->DsmBuffer->ClearStorage();
        file->DsmBuffer->GetComm()->Barrier();
      } else {
        // Check that the DSM ready for update flag is set
        while (!file->DsmBuffer->GetIsUpdateReady() && file->DsmBuffer->GetIsAutoAllocated()) {
          // Spin
        }
        // For now a file in a DSM Buffer which is not created is considered as open in read-only
        PRINT_INFO("SetIsReadOnly(true)");
        file->DsmBuffer->SetIsReadOnly(true);
      }

      PRINT_INFO("Opening " << name);

      if (DsmGetEntry(file) == H5FD_DSM_FAIL) {

        if (H5F_ACC_CREAT & flags) {
          PRINT_INFO("Creating " << name);
          DsmUpdateEntry(file);
        }
        else {
          if (file->name) H5MM_xfree(file->name);
          H5MM_xfree(file);
          HGOTO_ERROR(H5E_VFL, H5E_NOTFOUND, NULL, "DSM buffer already existing not found");
        }
      }
      else {
        PRINT_INFO("Opened from Entry "
            << name << " Start " << file->start << " End " << file->end);
      }

      file->eof = file->end - file->start;

    } else { // try to connect to see if the DSM is handled by someone else

      if (!file->DsmBuffer->GetIsConnected()) {
#ifdef H5FD_DSM_DEBUG
        file->DsmBuffer->DebugOn();
        file->DsmBuffer->GetComm()->DebugOn();
        if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_SOCKET) {
          H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommSocket*> (file->DsmBuffer->GetComm())->GetDsmMasterHostName();
          H5FDdsmInt32 port = dynamic_cast<H5FDdsmCommSocket*> (file->DsmBuffer->GetComm())->GetDsmMasterPort();
          PRINT_INFO("DSM driver connecting on: " << hostName << ":" << port);
        }
        else if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI) {
          H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommMpi*> (file->DsmBuffer->GetComm())->GetDsmMasterHostName();
          PRINT_INFO("DSM driver connecting on: " << hostName);
        }
#endif
        if (file->DsmBuffer->GetComm()->RemoteCommConnect() != H5FD_DSM_SUCCESS) {
          HGOTO_ERROR(H5E_VFL, H5E_MPI, NULL, "Comm_connect error");
        }
        PRINT_INFO("Connected!");
        file->DsmBuffer->SetIsConnected(true);

        // Receive DSM info
        H5FDdsmInt64 length;
        H5FDdsmInt64 totalLength;
        H5FDdsmInt32 startServerId, endServerId;

        file->DsmBuffer->GetComm()->RemoteCommRecvInfo(&length, &totalLength, &startServerId, &endServerId);

        file->DsmBuffer->SetLength(length, 0);
        PRINT_INFO("Length received: " << file->DsmBuffer->GetLength());

        file->DsmBuffer->SetTotalLength(totalLength);
        PRINT_INFO("totalLength received: " << file->DsmBuffer->GetTotalLength());

        file->DsmBuffer->SetStartServerId(startServerId);
        PRINT_INFO("startServerId received: " << file->DsmBuffer->GetStartServerId());

        file->DsmBuffer->SetEndServerId(endServerId);
        PRINT_INFO("endServerId received: " << file->DsmBuffer->GetEndServerId());
      }

      if (H5F_ACC_CREAT & flags) {
        // Receive ready for write from DSM
        file->DsmBuffer->GetComm()->RemoteCommRecvReady();

        // TODO Probably do this somewhere else but here for now
        file->DsmBuffer->GetSteerer()->ReceiveSteeringCommands();

        // This is a critical part, we need to synchronize before
        // and after the ClearStorage method call
        PRINT_INFO("Request Clear DSM before create");
        file->DsmBuffer->GetComm()->Barrier();
        file->DsmBuffer->RequestClearStorage();
      }
    }
  }
  //
  // The increment comes from either the file access property list or the
  // default value. But if the file access property list was zero then use
  // the default value instead.
  //
  file->increment = (fa && fa->increment > 0) ? fa->increment
      : H5FD_DSM_INCREMENT;
  if (fa->buffer) {
    PRINT_INFO(
        "Opened " << name <<
        " Start " << file->start <<
        " End " << file->end <<
        " Eoa " << file->eoa <<
        " Eof  " << file->eof);
  }

  // Don't let any proc return until all have created the file.
  if (H5F_ACC_CREAT & flags) {
    if (file->DsmBuffer->GetComm()->Barrier() != H5FD_DSM_SUCCESS) {
      HGOTO_ERROR(H5E_VFL, H5E_CANTLOCK, NULL, "Unable to sync processes");
    }
  }

  // Set return value
  ret_value = (H5FD_t *) file;

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_close
 *
 * Purpose:  Closes the file.
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_close(H5FD_t *_file)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; // Return value
  hbool_t isSomeoneDirty = FALSE;
  MPI_Comm comm = MPI_COMM_NULL;

  FUNC_ENTER_NOAPI(H5FD_dsm_close, FAIL)

  PRINT_INFO(
      "Closing Start " << file->start << " End " << file->end <<
      " Eoa " << file->eoa << " Eof " << file->eof);

  if (DsmUpdateEntry(file) != H5FD_DSM_SUCCESS)
    HGOTO_ERROR(H5E_VFL, H5E_CLOSEERROR, FAIL, "can't close DSM");
  PRINT_INFO("UpdateEntry");

  if (!file->DsmBuffer->GetIsReadOnly()) {
    PRINT_INFO("Gathering dirty info");
    if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_SOCKET) {
      comm = dynamic_cast <H5FDdsmCommSocket*> (file->DsmBuffer->GetComm())->GetComm();
    }
    else if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI) {
      comm = dynamic_cast <H5FDdsmCommMpi*> (file->DsmBuffer->GetComm())->GetComm();
    }

    // Be sure that everyone's here before releasing resources (done with collective op)
    // We're now ready to read from the DSM
    // Gather all the dirty flags because some processes may not have written yet
    MPI_Allreduce(&file->dirty, &isSomeoneDirty, sizeof(hbool_t), MPI_UNSIGNED_CHAR, MPI_MAX, comm);

    if (isSomeoneDirty) {
      file->DsmBuffer->RequestPipelineUpdate();
    }
  } else {
    if (file->DsmBuffer->GetIsUpdateReady() && file->DsmBuffer->GetIsAutoAllocated()) {
      // file->DsmBuffer->GetComm()->Barrier();
      file->DsmBuffer->SetIsUpdateReady(false);
      file->DsmBuffer->RequestRemoteChannel();
    }
    PRINT_INFO("SetIsReadOnly(false)");
    file->DsmBuffer->SetIsReadOnly(false);
  }

  // Release resources
  if (file->name) H5MM_xfree(file->name);
  HDmemset(file, 0, sizeof(H5FD_dsm_t));
  H5MM_xfree(file);
  PRINT_DSM_INFO("Undef", "File closed");
  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_cmp
 *
 * Purpose:  Compares two files belonging to this driver by name. If one
 *    file doesn't have a name then it is less than the other file.
 *    If neither file has a name then the comparison is by file
 *    address.
 *
 * Return:  Success:  A value like strcmp()
 *
 *    Failure:  never fails (arguments were checked by the
 *        caller).
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_dsm_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
  const H5FD_dsm_t *f1 = (const H5FD_dsm_t*) _f1;
  const H5FD_dsm_t *f2 = (const H5FD_dsm_t*) _f2;
  int ret_value;

  FUNC_ENTER_NOAPI(H5FD_dsm_cmp, FAIL)

  if (NULL == f1->name && NULL == f2->name) {
    if (f1 < f2)
      HGOTO_DONE(-1);
    if (f1 > f2)
      HGOTO_DONE(1);
    HGOTO_DONE(0);
  }

  if (NULL == f1->name)
    HGOTO_DONE(-1);
  if (NULL == f2->name)
    HGOTO_DONE(1);

  ret_value = HDstrcmp(f1->name, f2->name);

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_get_eoa
 *
 * Purpose:  Gets the end-of-address marker for the file. The EOA marker
 *    is the first address past the last byte allocated in the
 *    format address space.
 *
 * Return:  Success:  The end-of-address marker.
 *
 *    Failure:  HADDR_UNDEF
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_dsm_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
  haddr_t ret_value; // Return value
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;

  FUNC_ENTER_NOAPI(H5FD_dsm_get_eoa, HADDR_UNDEF)

  // Set return value
  ret_value = file->eoa;

  FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_set_eoa
 *
 * Purpose:  Set the end-of-address marker for the file. This function is
 *    called shortly after an existing HDF5 file is opened in order
 *    to tell the driver where the end of the HDF5 data is located.
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_set_eoa, FAIL)

  PRINT_INFO("H5FD_dsm_set_eoa Called " << addr);

  if (ADDR_OVERFLOW(addr)) {
    PRINT_INFO("H5FD_dsm_set_eoa Address OverFLow at " << addr);
    PRINT_INFO("H5FD_dsm_set_eoa MAXADDR = " << MAXADDR);
    PRINT_INFO("H5FD_dsm_set_eoa Address (addr) & ~(haddr_t)MAXADDR) = " << (addr & ~(haddr_t)MAXADDR));
    HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "address overflow");
  }
  file->eof = file->eoa = addr;
  DsmUpdateEntry(file);

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_get_eof
 *
 * Purpose:  Returns the end-of-file marker, which is the greater of
 *    either the size of the underlying memory or the HDF5
 *    end-of-address markers.
 *
 * Return:  Success:  End of file address, the first address past
 *        the end of the "file", either the memory
 *        or the HDF5 file.
 *
 *    Failure:  HADDR_UNDEF
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_dsm_get_eof(const H5FD_t *_file)
{
  haddr_t ret_value; // Return value

  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;

  FUNC_ENTER_NOAPI(H5FD_dsm_get_eof, HADDR_UNDEF)

  PRINT_INFO("H5FD_dsm_get_eof Called " << MAX(file->eof, file->eoa));

  // Set return value
  ret_value = MAX(file->eof, file->eoa);

  FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_read
 *
 * Purpose:  Reads SIZE bytes of data from FILE beginning at address ADDR
 *    into buffer BUF according to data transfer properties in
 *    DXPL_ID.
 *
 * Return:  Success:  Zero. Result is stored in caller-supplied
 *        buffer BUF.
 *
 *    Failure:  -1, Contents of buffer BUF are undefined.
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_read(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
    haddr_t addr, DSM_HSIZE_T size, void *buf/*out*/)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_read, FAIL)

  PRINT_INFO(
      "Read Start " << file->start <<
      " End " << file->end <<
      " Addr " << addr <<
      " Size " << size <<
      " Eoa " << file->eoa <<
      " Eof " << file->eof);

  assert(file && file->pub.cls);
  assert(buf);

  // Check for overflow conditions
  if (HADDR_UNDEF == addr)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
  if (REGION_OVERFLOW(addr, size))
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
  if (addr + size > file->eoa)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");

  // Read the part which is before the EOF marker
  PRINT_INFO("check addr ( " << addr << " )  < file->eof ( " << file->eof << " )");

  if (addr < file->eof) {
    DSM_HSIZE_T nbytes;
    hsize_t temp_nbytes;

    temp_nbytes = file->eof - addr;
    H5_CHECK_OVERFLOW(temp_nbytes,hsize_t,DSM_HSIZE_T);
    nbytes = MIN(size,(DSM_HSIZE_T)temp_nbytes);
    if (file->DsmBuffer->Get(file->start + addr, nbytes, buf) <= 0) {
      HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't read from DSM");
    }
    else {
      size -= nbytes;
      addr += nbytes;
      buf = (char*) buf + nbytes;
    }
  }
  // Read zeros for the part which is after the EOF markers
  if (size > 0) HDmemset(buf, 0, size);

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_write
 *
 * Purpose:  Writes SIZE bytes of data to FILE beginning at address ADDR
 *    from buffer BUF according to data transfer properties in
 *    DXPL_ID.
 *
 * Return:  Success:  Zero
 *
 *    Failure:  -1
 *
 * Programmer:  Jerry Clarke
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_write(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
    haddr_t addr, DSM_HSIZE_T size, const void *buf)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_write, FAIL)

  assert(file && file->pub.cls);
  assert(buf);

  if (file->DsmBuffer->GetIsReadOnly()) {
    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write to DSM open in read-only");
  }

  PRINT_INFO(
      "Write Start " << file->start <<
      " End " << file->end <<
      " Addr " << addr <<
      " Size " << size <<
      " Eoa " << file->eoa <<
      " Eof " << file->eof);

  // Check for overflow conditions
  if (REGION_OVERFLOW(addr, size))
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
  if (addr + size > file->eoa)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");

  if (addr + size > file->eof) {
    haddr_t new_eof;

    new_eof = file->increment * ((addr + size) / file->increment);
    if ((addr + size) % file->increment) new_eof += file->increment;
    // TODO Dangerous here since we don't do any realloc
    PRINT_INFO("HDF::Write New eof " << new_eof);

    // Blindly Grab more DSM for now
    file->end = file->start + new_eof;
    file->eof = new_eof;
    // Write it out to DSM
    if (DsmUpdateEntry(file) != H5FD_DSM_SUCCESS)
      HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "dsm update entry failed");
  }

  // Write from BUF to DSM
  if (file->DsmBuffer->Put(file->start + addr, size, (void *) buf) <= 0)
    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write to DSM");

  file->dirty = TRUE;

  done: FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_flush
 *
 * Purpose:  Flushes the file to backing store if there is any and if the
 *    dirty flag is set.
 *
 * Return:  Success:  0
 *
 *    Failure:  -1
 *
 * Programmer: JB : Unfinished (potentially used to signal 'finished')
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_flush(H5FD_t *_file, hid_t UNUSED dxpl_id, unsigned UNUSED closing)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_flush, FAIL)

  // Write to backing store
  if (file->dirty) {
   /* haddr_t size = file->eof;
    
         unsigned char *ptr = file->mem;

         if (0!=HDlseek(file->fd, (off_t)0, SEEK_SET))
         HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "error seeking in backing store")

         while (size) {
         ssize_t n;

         H5_CHECK_OVERFLOW(size,hsize_t,size_t);
         n = HDwrite(file->fd, ptr, (size_t)size);
         if (n<0 && EINTR==errno)
         continue;
         if (n<0)
         HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "error writing backing store")
         ptr += (size_t)n;
         size -= (size_t)n;
         }
     */
    file->dirty = FALSE;
  }

  /*done: */FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_mpi_rank
 *
 * Purpose:  Returns the MPI rank for a process
 *
 * Return:  Success: non-negative
 *    Failure: negative
 *
 * Programmer:	JB : After H5FDmpio : By Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_dsm_mpi_rank(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;
  int ret_value; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_mpi_rank, FAIL)

  assert(file);
  assert(H5FD_DSM==file->pub.driver_id);

  // Set return value
  ret_value = file->DsmBuffer->GetComm()->GetId();

  FUNC_LEAVE_NOAPI(ret_value)
} // end H5FD_dsm_mpi_rank()

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_mpi_size
 *
 * Purpose:  Returns the number of MPI processes
 *
 * Return:  Success: non-negative
 *    Failure: negative
 *
 * Programmer:	JB : After H5FDmpio : By Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_dsm_mpi_size(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;
  int ret_value; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_mpi_size, FAIL)

  assert(file);
  assert(H5FD_DSM==file->pub.driver_id);

  /* Set return value */
  ret_value = file->DsmBuffer->GetComm()->GetTotalSize();

  FUNC_LEAVE_NOAPI(ret_value)
} // end H5FD_dsm_mpi_size()

/*-------------------------------------------------------------------------
 * Function:  H5FD_dsm_communicator
 *
 * Purpose:  Returns the MPI communicator for the file.
 *
 * Return:  Success:  The communicator
 *
 *    Failure:  NULL
 *
 * Programmer:	JB : After H5FDmpio : By Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_dsm_communicator(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;
  MPI_Comm ret_value = MPI_COMM_NULL; // Return value

  FUNC_ENTER_NOAPI(H5FD_dsm_communicator, MPI_COMM_NULL)

  assert(file);
  assert(H5FD_DSM==file->pub.driver_id);

  // Set return value
  if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_SOCKET) {
  ret_value
  = dynamic_cast <H5FDdsmCommSocket*> (file->DsmBuffer->GetComm())->GetComm();
  }
  else if (file->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI) {
    ret_value
    = dynamic_cast <H5FDdsmCommMpi*> (file->DsmBuffer->GetComm())->GetComm();
  }

  FUNC_LEAVE_NOAPI(ret_value)
} // end H5FD_mpi_posix_communicator()

/*-------------------------------------------------------------------------
 * Function:	H5FD_dsm_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	JB : After H5FDmpio : By Quincey Koziol
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_dsm_query(const H5FD_t *_file, unsigned long *flags)
{
    H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5FD_dsm_query, FAIL)

    /* Set the VFL feature flags that this driver supports */
    if(flags && !file->DsmBuffer->GetIsReadOnly()) { // If it is read-only use the driver serially
        *flags=0;
        *flags|=H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations */
        *flags|=H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        *flags|=H5FD_FEAT_HAS_MPI;             /* This driver uses MPI */
        *flags|=H5FD_FEAT_ALLOCATE_EARLY;      /* Allocate space early instead of late */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 *-------------------------------------------------------------------------
 */


#endif // H5_HAVE_PARALLEL
