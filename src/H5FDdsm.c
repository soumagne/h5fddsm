/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsm.c

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

#include "H5private.h"    /* Generic Functions      */
#include "H5ACprivate.h"  /* Metadata cache         */
#include "H5Dprivate.h"   /* Dataset functions      */
#include "H5Eprivate.h"   /* Error handling         */
#include "H5Fprivate.h"   /* File access            */
#include "H5FDprivate.h"  /* File drivers           */
#include "H5FDmpi.h"      /* MPI-based file drivers */
#include "H5Iprivate.h"   /* IDs                    */
#include "H5Pprivate.h"   /* Property lists         */
#include "H5FDdsm.h"      /* DSM driver             */

/*
 * H5private.h defines attribute, but we don't want it
 * as it causes link errors on some gcc versions
#ifdef __GNUC__
# undef __attribute__
#endif
 */

/*
 * Unfortunately, some of the HDF5 macros use internal variables which are
 * not exported from the hdf5 lib/dll so we must override the macros and
 * lose some debugging info
 */
#ifdef _WIN32
  #if defined(H5_INIT_GLOBAL)
    #undef H5_INIT_GLOBAL
    int dummy = TRUE;
    #define H5_INIT_GLOBAL dummy
  #endif
  #undef HMPI_ERROR
  #define HMPI_ERROR(X)
#endif

#include "H5FDdsmDriver.h"

#ifdef H5_HAVE_PARALLEL

#undef MAX
#define MAX(X,Y)  ((X) > (Y) ? (X) : (Y))
#undef MIN
#define MIN(X,Y)  ((X) < (Y) ? (X) : (Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_DSM_g = 0;

/*
 * The description of a file belonging to this driver. The `eoa' and `eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying memory).
 */
typedef struct H5FD_dsm_t
{
  H5FD_t pub;           /* public stuff, must be first             */
  char *name;           /* for equivalence testing                 */
  MPI_Comm intra_comm;  /* intra-communicator                      */
  int intra_rank;       /* this process's rank in intra_comm       */
  int intra_size;       /* total number of processes in intra_comm */
  void *local_buf_ptr;  /* underlying local DSM buffer             */
  size_t local_buf_len; /* local DSM buffer length                 */
  haddr_t eoa;          /* end of address marker                   */
  haddr_t eof;          /* end of file marker                      */
  haddr_t start;        /* current DSM start address               */
  haddr_t end;          /* current DSM end address                 */
  hbool_t read_only;    /* file access is read-only                */
  hbool_t dirty;        /* dirty marker                            */
} H5FD_dsm_t;

/* Driver-specific file access properties */
typedef struct H5FD_dsm_fapl_t
{
  MPI_Comm intra_comm;      /* intra-communicator          */
  void  *local_buf_ptr;     /* local buffer pointer        */
  size_t local_buf_len;     /* local buffer length         */
} H5FD_dsm_fapl_t;


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
#define MAXADDR                 ((haddr_t)((~(size_t)0)-1))
#define ADDR_OVERFLOW(A)        (HADDR_UNDEF==(A) || (A) > (haddr_t)MAXADDR)
#define SIZE_OVERFLOW(Z)        ((Z) > (hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||      \
                                 HADDR_UNDEF==(A)+(Z) ||                      \
                                 (size_t)((A)+(Z))<(size_t)(A))

/* Private Prototypes */
static void    *H5FD_dsm_fapl_get(H5FD_t *_file);
static void    *H5FD_dsm_fapl_copy(const void *_old_fa);
static herr_t   H5FD_dsm_fapl_free(void *_fa);
static H5FD_t  *H5FD_dsm_open(const char *name, unsigned flags, hid_t fapl_id,
    haddr_t maxaddr);
static herr_t   H5FD_dsm_close(H5FD_t *_file);
static herr_t   H5FD_dsm_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t  H5FD_dsm_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t   H5FD_dsm_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t  H5FD_dsm_get_eof(const H5FD_t *_file);
static herr_t   H5FD_dsm_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id,
    haddr_t addr, size_t size, void *buf);
static herr_t   H5FD_dsm_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id,
    haddr_t addr, size_t size, const void *buf);
static herr_t   H5FD_dsm_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static int      H5FD_dsm_mpi_rank(const H5FD_t *_file);
static int      H5FD_dsm_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD_dsm_communicator(const H5FD_t *_file);

/* The DSM file driver information */
static const H5FD_class_mpi_t H5FD_dsm_g = {
    {
        "dsm",                                  /* name                 */
        MAXADDR,                                /* maxaddr              */
        H5F_CLOSE_SEMI,                         /* fc_degree            */
#if H5_VERSION_GE(1,9,0)
        H5FD_dsm_term,                          /* terminate            */
#endif
        NULL,                                   /* sb_size              */
        NULL,                                   /* sb_encode            */
        NULL,                                   /* sb_decode            */
        sizeof(H5FD_dsm_fapl_t),                /* fapl_size            */
        H5FD_dsm_fapl_get,                      /* fapl_get             */
        H5FD_dsm_fapl_copy,                     /* fapl_copy            */
        H5FD_dsm_fapl_free,                     /* fapl_free            */
        0,                                      /* dxpl_size            */
        NULL,                                   /* dxpl_copy            */
        NULL,                                   /* dxpl_free            */
        H5FD_dsm_open,                          /* open                 */
        H5FD_dsm_close,                         /* close                */
        NULL,                                   /* cmp                  */
        H5FD_dsm_query,                         /* query                */
        NULL,                                   /* get_type_map         */
        NULL,                                   /* alloc                */
        NULL,                                   /* free                 */
#ifdef H5_HAVE_VFD_EXTENSIONS
        H5FD_dsm_term,                          /* terminate            */
#endif
        H5FD_dsm_get_eoa,                       /* get_eoa              */
        H5FD_dsm_set_eoa,                       /* set_eoa              */
        H5FD_dsm_get_eof,                       /* get_eof              */
        NULL,                                   /* get_handle           */
        H5FD_dsm_read,                          /* read                 */
        H5FD_dsm_write,                         /* write                */
        H5FD_dsm_flush,                         /* flush                */
        NULL,                                   /* truncate             */
        NULL,                                   /* lock                 */
        NULL,                                   /* unlock               */
        H5FD_FLMAP_SINGLE                       /* fl_map               */
    },
    H5FD_dsm_mpi_rank,                          /* get_rank             */
    H5FD_dsm_mpi_size,                          /* get_size             */
    H5FD_dsm_communicator                       /* get_comm             */
};

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
#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_init_interface)
#endif

  FUNC_LEAVE_NOAPI(H5FD_dsm_init())
} /* end H5FD_dsm_init_interface() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:        The driver ID for the dsm driver.
 *
 *              Failure:        Negative.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_dsm_init(void)
{
  hid_t ret_value; /* Return value */

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI(FAIL)
#else
  FUNC_ENTER_NOAPI(H5FD_dsm_init, FAIL)
#endif

  if (H5I_VFL != H5Iget_type(H5FD_DSM_g)) {
    H5FD_DSM_g = H5FD_register(&H5FD_dsm_g, sizeof(H5FD_class_mpi_t), FALSE);
  }

  /* Set return value */
  ret_value = H5FD_DSM_g;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_init() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_term
 *
 * Purpose:     Shut down the driver
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
#if H5_VERSION_GE(1,9,0)
herr_t
#else
void
#endif
H5FD_dsm_term(void)
{
#if H5_VERSION_GE(1,9,0)
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(FAIL)
#else
#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_term)
#endif
#endif

  if (SUCCEED != dsm_free()) {
#if H5_VERSION_GE(1,9,0)
    HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "dsm_free failed")
#endif
  }

  /* Reset VFL ID */
  H5FD_DSM_g = 0;

#if H5_VERSION_GE(1,9,0)
done:
  FUNC_LEAVE_NOAPI(ret_value)
#else
  FUNC_LEAVE_NOAPI_VOID
#endif
} /* end H5FD_dsm_term() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_set_options
 *
 * Purpose:     Set a specific option to the DSM
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_dsm_set_options(unsigned long flags)
{
  herr_t ret_value = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI(FAIL)
#else
  FUNC_ENTER_NOAPI(H5FD_dsm_set_options, FAIL)
#endif

  if (SUCCEED != dsm_set_options(flags))
    HGOTO_ERROR(H5E_VFL, H5E_CANTMODIFY, FAIL, "cannot set options")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_set_options() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_notify
 *
 * Purpose:     Send a notification to the DSM host
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_dsm_notify(unsigned long flags)
{
  herr_t ret_value = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI(FAIL)
#else
  FUNC_ENTER_NOAPI(H5FD_dsm_notify, FAIL)
#endif

  if (SUCCEED != dsm_notify(flags))
    HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "cannot notify DSM")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_notify() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_set_manager
 *
 * Purpose:     (C++ only) Associate an existing DSM manager to the driver.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_dsm_set_manager(void *manager)
{
  herr_t ret_value = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI(FAIL)
#else
  FUNC_ENTER_NOAPI(H5FD_dsm_set_manager, FAIL)
#endif

  dsm_set_manager(manager);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_set_manager() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_dsm
 *
 * Purpose:     Modify the file access property list to use the H5FDdsm
 *              driver defined in this source file.
 *              If local_buf_ptr is NULL, the local memory buffer will be
 *              automatically allocated or (C++ only) be used from an
 *              existing H5FDdsmBuffer object using H5FD_dsm_set_buffer().
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_dsm(hid_t fapl_id, MPI_Comm intra_comm, void *local_buf_ptr,
    size_t local_buf_len)
{
  H5FD_dsm_fapl_t fa;
  herr_t ret_value = SUCCEED;
  H5P_genplist_t *plist; /* Property list pointer */

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_API(FAIL)
#else
  FUNC_ENTER_API(H5Pset_fapl_dsm, FAIL)
#endif

  /* Check arguments */
  if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
  if (MPI_COMM_NULL == intra_comm)
    HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

  if (!dsm_get_manager()) {
    if (SUCCEED != dsm_alloc(intra_comm, local_buf_ptr, local_buf_len))
      HGOTO_ERROR(H5E_PLIST, H5E_CANTALLOC, FAIL, "cannot allocate DSM buffer")
  }

  if (SUCCEED != dsm_get_properties(&fa.intra_comm, &fa.local_buf_ptr, &fa.local_buf_len))
    HGOTO_ERROR(H5E_PLIST, H5E_CANTALLOC, FAIL, "cannot get DSM properties")

  if (!dsm_is_connected() && !dsm_is_server()) {
    /* TODO we should decide what to do if we cannot connect */
    dsm_connect();
  }

  /* duplication is done during driver setting */
  ret_value = H5P_set_driver(plist, H5FD_DSM, &fa);

done:
  FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_dsm() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_dsm
 *
 * Purpose:     Query properties set by the H5Pset_fapl_dsm() function.
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_dsm(hid_t fapl_id, MPI_Comm *intra_comm /* out */,
    void **local_buf_ptr_ptr /* out */, size_t *local_buf_len_ptr /* out */)
{
  H5FD_dsm_fapl_t *fa;
  MPI_Comm intra_comm_tmp = MPI_COMM_NULL;
  H5P_genplist_t *plist; /* Property list pointer */
  herr_t ret_value = SUCCEED;
  int mpi_code;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_API(FAIL)
#else
  FUNC_ENTER_API(H5Pget_fapl_dsm, FAIL)
#endif

  if (NULL == (plist = (H5P_genplist_t*) H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
  if (H5FD_DSM != H5P_get_driver(plist))
    HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
  if (NULL == (fa = (H5FD_dsm_fapl_t*) H5P_get_driver_info(plist)))
    HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")

  if (intra_comm) {
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(fa->intra_comm, &intra_comm_tmp)))
      HMPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code)
    *intra_comm = intra_comm_tmp;
  }

  if (local_buf_ptr_ptr) *local_buf_ptr_ptr = fa->local_buf_ptr;

  if (local_buf_len_ptr) *local_buf_len_ptr = fa->local_buf_len;

done:
  if (FAIL == ret_value) {
    /* need to free anything created here */
    if (intra_comm_tmp != MPI_COMM_NULL)
      MPI_Comm_free(&intra_comm_tmp);
  }

  FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_dsm() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_fapl_get
 *
 * Purpose:     Return a copy of the file access properties.
 *
 * Return:      Success:        Ptr to a new property list
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_dsm_fapl_get(H5FD_t *_file)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  H5FD_dsm_fapl_t *fa = NULL;
  void *ret_value = NULL;
  int mpi_code;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_fapl_get)
#endif

  HDassert(file);
  HDassert(H5FD_DSM == file->pub.driver_id);

  if (NULL == (fa = (H5FD_dsm_fapl_t *) calloc((size_t)1, sizeof(H5FD_dsm_fapl_t))))
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

  /* Duplicate communicator. */
  fa->intra_comm = MPI_COMM_NULL;
  if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file->intra_comm, &fa->intra_comm)))
    HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)

  fa->local_buf_ptr = file->local_buf_ptr;
  fa->local_buf_len = file->local_buf_len;

  /* Set return value */
  ret_value = fa;

done:
  if ((NULL == ret_value) && err_occurred) {
    /* need to free anything created here */
    if (fa && (MPI_COMM_NULL != fa->intra_comm))
      MPI_Comm_free(&fa->intra_comm);
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_fapl_copy
 *
 * Purpose:     Copy the dsm-specific file access properties.
 *
 * Return:      Success:        Ptr to a new property list
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_dsm_fapl_copy(const void *_old_fa)
{
  void *ret_value = NULL;
  const H5FD_dsm_fapl_t *old_fa = (const H5FD_dsm_fapl_t*)_old_fa;
  H5FD_dsm_fapl_t *new_fa = NULL;
  int mpi_code;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_fapl_copy)
#endif

  if(NULL == (new_fa = (H5FD_dsm_fapl_t *) calloc((size_t)1, sizeof(H5FD_dsm_fapl_t))))
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

  /* Copy the general information */
  HDmemcpy(new_fa, old_fa, sizeof(H5FD_dsm_fapl_t));

  /* Duplicate communicator. */
  new_fa->intra_comm = MPI_COMM_NULL;
  if (!dsm_is_driver_serial()) {
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(old_fa->intra_comm, &new_fa->intra_comm)))
      HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)
  } else {
    new_fa->intra_comm = old_fa->intra_comm;
  }
  ret_value = new_fa;

done:
  if ((NULL == ret_value) && err_occurred) {
    /* cleanup */
    if (!dsm_is_driver_serial() && new_fa && (MPI_COMM_NULL != new_fa->intra_comm))
      MPI_Comm_free(&new_fa->intra_comm);
    if (new_fa) free(new_fa);
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_fapl_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_fapl_free
 *
 * Purpose:     Free the dsm-specific file access properties.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_fapl_free(void *_fa)
{
  herr_t ret_value = SUCCEED;
  H5FD_dsm_fapl_t *fa = (H5FD_dsm_fapl_t*)_fa;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_fapl_free)
#endif

  assert(fa);

  if (!dsm_is_driver_serial()) {
    /* Free the internal communicator */
    assert(MPI_COMM_NULL != fa->intra_comm);
    MPI_Comm_free(&fa->intra_comm);
  }
  free(fa);

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_open
 *
 * Purpose:     Create memory as an HDF5 file.
 *
 * Return:      Success:        A pointer to a new file data structure. The
 *              public fields will be initialized by the
 *              caller, which is always H5FD_open().
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_dsm_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
  H5FD_dsm_t *file = NULL;
  int mpi_rank; /* MPI rank of this process */
  int mpi_size; /* Total number of MPI processes */
  int mpi_code;  /* mpi return code */
  const H5FD_dsm_fapl_t *fa = NULL;
  MPI_Comm intra_comm_dup = MPI_COMM_NULL;
  H5P_genplist_t *plist; /* Property list pointer */
  H5FD_t *ret_value = NULL;
  herr_t dsm_code = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_open)
#endif

  /* Check arguments */
  if(!name || !*name)
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
  if (0 == maxaddr || HADDR_UNDEF == maxaddr)
    HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
  if (ADDR_OVERFLOW(maxaddr))
    HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "maxaddr overflow")

  if (H5P_DEFAULT != fapl_id) {
    if (NULL == (plist = (H5P_genplist_t*) H5I_object(fapl_id)))
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    fa = (const H5FD_dsm_fapl_t *) H5P_get_driver_info(plist);
    assert(fa);
  }

  if (!dsm_is_driver_serial()) {
    /* Duplicate communicator. */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(fa->intra_comm, &intra_comm_dup)))
      HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)
  }

  /* Get the MPI rank of this process and the total number of processes */
  if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank (fa->intra_comm, &mpi_rank)))
    HMPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code)
  if (MPI_SUCCESS != (mpi_code = MPI_Comm_size (fa->intra_comm, &mpi_size)))
    HMPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code)

  /* Build the return value and initialize it */
  if (NULL == (file = (H5FD_dsm_t *) calloc((size_t)1, sizeof(H5FD_dsm_t))))
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

  if (!dsm_is_driver_serial()) {
    file->intra_comm = intra_comm_dup;
  } else {
    file->intra_comm = fa->intra_comm;
  }
  file->intra_rank = mpi_rank;
  file->intra_size = mpi_size;

  if (name && *name) {
    file->name = HDstrdup(name);
  }

  /* Get address information from DSM */
  if (!dsm_get_manager())
    HGOTO_ERROR(H5E_VFL, H5E_NOTFOUND, NULL, "DSM buffer not found")

  file->local_buf_ptr = fa->local_buf_ptr;
  file->local_buf_len = fa->local_buf_len;

  if (SUCCEED != dsm_lock())
    HGOTO_ERROR(H5E_VFL, H5E_CANTLOCK, NULL, "cannot lock DSM")

  if (((file->intra_rank == 0) || dsm_is_driver_serial()) && (SUCCEED != dsm_get_entry(&file->start, &file->end)))
    dsm_code = FAIL;
  if (!dsm_is_driver_serial()) {
    /* Wait for the DSM entry to be updated */
    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&dsm_code, sizeof(herr_t),
        MPI_UNSIGNED_CHAR, 0, file->intra_comm)))
      HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)
  }
  if (SUCCEED != dsm_code)
    HGOTO_ERROR(H5E_VFL, H5E_CANTRESTORE, NULL, "cannot restore DSM entries")

  if (!dsm_is_driver_serial()) {
    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&file->start, sizeof(haddr_t),
        MPI_UNSIGNED_CHAR, 0, file->intra_comm)))
      HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)
    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&file->end, sizeof(haddr_t),
        MPI_UNSIGNED_CHAR, 0, file->intra_comm)))
      HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)
  }

  if (H5F_ACC_RDWR & flags) {
    file->read_only = FALSE;
  } else {
    file->read_only = TRUE;
  }

  if (H5F_ACC_CREAT & flags) {
    /* Reset start and end markers */
    file->start = 0;
    file->end = 0;
    file->eof = 0;
  } else {
    file->eof = file->end - file->start;
  }

  /* Don't let any proc return until all have created the file. */
  if (!dsm_is_driver_serial() && (H5F_ACC_CREAT & flags)) {
    if (MPI_SUCCESS != (mpi_code = MPI_Barrier(intra_comm_dup)))
      HMPI_GOTO_ERROR(NULL, "MPI_Barrier failed", mpi_code)
  }

  /* Set return value */
  ret_value = (H5FD_t *) file;

done:
  if((ret_value == NULL) && err_occurred) {
    if (file && file->name) HDfree(file->name);
    if (!dsm_is_driver_serial() && (MPI_COMM_NULL != intra_comm_dup)) MPI_Comm_free(&intra_comm_dup);
    if (file) free(file);
  } /* end if */

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_open() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_close
 *
 * Purpose:     Close the file.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_close(H5FD_t *_file)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; /* Return value */
  int mpi_code;
  herr_t dsm_code = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_close)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  if (!file->read_only) {
    file->end = MAX((file->start + file->eof), file->end);

    if (((file->intra_rank == 0) || dsm_is_driver_serial()) && (SUCCEED != dsm_update_entry(file->start, file->end)))
      dsm_code = FAIL;
    if (!dsm_is_driver_serial()) {
      /* Wait for the DSM entry to be updated */
      if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&dsm_code, sizeof(herr_t),
          MPI_UNSIGNED_CHAR, 0, file->intra_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code)
    }
    if (SUCCEED != dsm_code)
      HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "cannot update DSM entries")

    if (!dsm_is_server() || !dsm_is_connected()) {
      if (!dsm_is_driver_serial()) {
        /*
         * Be sure that everyone's here before releasing resources (done with
         * collective op). Gather all the dirty flags because some processes may
         * not have written yet.
         */
        if (MPI_SUCCESS != (mpi_code = MPI_Allreduce(MPI_IN_PLACE, &file->dirty,
            sizeof(hbool_t), MPI_UNSIGNED_CHAR, MPI_MAX, file->intra_comm)))
          HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
      }
      if (file->dirty) {
        if (SUCCEED != dsm_set_modified())
          HGOTO_ERROR(H5E_VFL, H5E_CANTUNLOCK, FAIL, "cannot mark DSM as modified")
      }
    }
  }

  if (SUCCEED != dsm_unlock())
    HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "cannot unlock DSM")

  /* Release resources */
  if (file->name) HDfree(file->name);
  if (!dsm_is_driver_serial() && (MPI_COMM_NULL != file->intra_comm)) MPI_Comm_free(&file->intra_comm);
  HDmemset(file, 0, sizeof(H5FD_dsm_t));
  free(file);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      Success:        non-negative
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_query(const H5FD_t UNUSED *_file, unsigned long *flags /* out */)
{
#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_query)
#endif

  /* Set the VFL feature flags that this driver supports */
  if (flags && !dsm_is_driver_serial()) {
    *flags = 0;
    *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations */
    *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
    *flags |= H5FD_FEAT_HAS_MPI;             /* This driver uses MPI */
    *flags |= H5FD_FEAT_ALLOCATE_EARLY;      /* Allocate space early instead of late */
  } /* end if */

  FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_dsm_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:      Success:        The end-of-address marker.
 *
 *              Failure:        HADDR_UNDEF
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_dsm_get_eoa(const H5FD_t *_file, H5FD_mem_t UNUSED type)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_get_eoa)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  FUNC_LEAVE_NOAPI(file->eoa)
} /* end H5FD_dsm_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_set_eoa(H5FD_t *_file, H5FD_mem_t UNUSED type, haddr_t addr)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; /* Return value */
  int mpi_code;
  herr_t dsm_code = SUCCEED;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_set_eoa)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  if (ADDR_OVERFLOW(addr))
    HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "address overflow")

  file->eoa = addr;

  file->end = MAX((file->start + file->eoa), file->end);
  file->eof = file->end - file->start;
  if (!file->read_only) {
    if (((file->intra_rank == 0) || dsm_is_driver_serial()) && (SUCCEED != dsm_update_entry(file->start, file->end)))
      dsm_code = FAIL;
    if (!dsm_is_driver_serial()) {
      /* Wait for the DSM entry to be updated */
      if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&dsm_code, sizeof(herr_t),
          MPI_UNSIGNED_CHAR, 0, file->intra_comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code)
    }
    if (SUCCEED != dsm_code)
      HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "cannot update DSM entries")
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_get_eof
 *
 * Purpose:     Returns the end-of-file marker, which is the greater of
 *              either the size of the underlying memory or the HDF5
 *              end-of-address markers.
 *
 * Return:      Success:        End of file address, the first address past
 *              the end of the "file", either the memory or the HDF5 file.
 *
 *              Failure:        HADDR_UNDEF
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_dsm_get_eof(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_get_eof)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  FUNC_LEAVE_NOAPI(MAX(file->eof, file->eoa))
} /* end H5FD_dsm_get_eof() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_read
 *
 * Purpose:     Reads SIZE bytes of data from FILE beginning at address ADDR
 *              into buffer BUF.
 *
 * Return:      Success:        Zero. Result is stored in caller-supplied
 *                              buffer BUF.
 *
 *              Failure:        -1, Contents of buffer BUF are undefined.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_read(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
    haddr_t addr, size_t size, void *buf /* out */)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; /* Return value */

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_read)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);
  assert(buf);

  /* Check for overflow conditions */
  if (HADDR_UNDEF == addr)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed")
  if (REGION_OVERFLOW(addr, size))
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed")
  if (addr + size > file->eoa)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed")

  /* Read the part which is before the EOF marker */
  if (addr < file->eof) {
    size_t  nbytes;
    hsize_t temp_nbytes;

    temp_nbytes = file->eof - addr;
    H5_CHECK_OVERFLOW(temp_nbytes,hsize_t,size_t);
    nbytes = MIN(size,(size_t)temp_nbytes);

    /* Read from DSM to BUF */
    if (SUCCEED != dsm_read(file->start + addr, nbytes, buf)) {
      HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "cannot read from DSM")
    } else {
      size -= nbytes;
      addr += nbytes;
      buf = (char*) buf + nbytes;
    }
  }
  /* Read zeros for the part which is after the EOF markers */
  if (size > 0) HDmemset(buf, 0, size);

done: 
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_write
 *
 * Purpose:     Writes SIZE bytes of data to FILE beginning at address ADDR
 *              from buffer BUF.
 *
 * Return:      Success:        Zero
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_write(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
    haddr_t addr, size_t size, const void *buf)
{
  H5FD_dsm_t *file = (H5FD_dsm_t*) _file;
  herr_t ret_value = SUCCEED; /* Return value */

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT
#else
  FUNC_ENTER_NOAPI_NOINIT(H5FD_dsm_write)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);
  assert(buf);

  if (file->read_only)
    HGOTO_ERROR(H5E_IO, H5E_RESOURCE, FAIL, "cannot write to DSM open in read-only")

  /* Check for overflow conditions */
  if (REGION_OVERFLOW(addr, size))
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed")
  if (addr + size > file->eoa)
    HGOTO_ERROR(H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed")

  /* For now, do not allow dynamic reallocation of the DSM */
  if (addr + size > file->eof)
    HGOTO_ERROR(H5E_IO, H5E_NOSPACE, FAIL, "not enough space in DSM")

  /* Write from BUF to DSM */
  if (SUCCEED != dsm_write(file->start + addr, size, buf))
    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "cannot write to DSM")

  /* Set dirty flag so that we know someone has written something */
  file->dirty = TRUE;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_write() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_flush
 *
 * Purpose:     Flushes the file to backing store if there is any and if the
 *              dirty flag is set.
 *              Could use flush if we have an additional caching mechanism.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_dsm_flush(H5FD_t *_file, hid_t UNUSED dxpl_id, unsigned UNUSED closing)
{
  /* H5FD_dsm_t *file = (H5FD_dsm_t*) _file; */
  herr_t ret_value = SUCCEED; /* Return value */

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_flush)
#endif

  /* Write to backing store */
  /*
  if (file->dirty) {
    haddr_t size = file->eof;

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

    file->dirty = FALSE;
  }
  */

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_dsm_flush() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_mpi_rank
 *
 * Purpose:     Returns the MPI rank for a process
 *
 * Return:      Success: non-negative
 *              Failure: negative
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_dsm_mpi_rank(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_mpi_rank)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  FUNC_LEAVE_NOAPI(file->intra_rank)
} /* end H5FD_dsm_mpi_rank() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_mpi_size
 *
 * Purpose:     Returns the number of MPI processes
 *
 * Return:      Success: non-negative
 *              Failure: negative
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_dsm_mpi_size(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_mpi_size)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  FUNC_LEAVE_NOAPI(file->intra_size)
} /* end H5FD_dsm_mpi_size() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_dsm_communicator
 *
 * Purpose:     Returns the MPI communicator for the file.
 *
 * Return:      Success:        The communicator
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_dsm_communicator(const H5FD_t *_file)
{
  const H5FD_dsm_t *file = (const H5FD_dsm_t*) _file;

#if H5_VERSION_GE(1,8,9)
  FUNC_ENTER_NOAPI_NOINIT_NOERR
#else
  FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_dsm_communicator)
#endif

  assert(file);
  assert(H5FD_DSM == file->pub.driver_id);

  FUNC_LEAVE_NOAPI(file->intra_comm)
} /* end H5FD_mpi_posix_communicator() */

#endif /* H5_HAVE_PARALLEL */
