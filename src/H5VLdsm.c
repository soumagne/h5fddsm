/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5VLdsm.h

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

#define H5D_PACKAGE		/* suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/* suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/* suppress error about including H5Gpkg      */
#define H5O_PACKAGE		/* suppress error about including H5Opkg	  */
#define H5T_PACKAGE		/* suppress error about including H5Tpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_dsm_init_interface


#include "H5private.h"		/* Generic Functions              */
#include "H5Aprivate.h"		/* Attributes                     */
#include "H5Dpkg.h"       /* Dataset pkg                    */
#include "H5Dprivate.h"		/* Datasets                       */
#include "H5Eprivate.h"		/* Error handling                 */
#include "H5Fprivate.h"		/* File access                    */
#include "H5Fpkg.h"       /* File pkg                       */
#include "H5FLprivate.h"	/* Free lists                     */
#include "H5Gpkg.h"		    /* Groups                         */
#include "H5Iprivate.h"		/* IDs                            */
#include "H5MFprivate.h"	/* File memory management         */
#include "H5MMprivate.h"	/* Memory management              */
#include "H5Opkg.h"       /* Object headers                 */
#include "H5Pprivate.h"		/* Property lists                 */
#include "H5SMprivate.h"	/* Shared Object Header Messages  */
#include "H5Tpkg.h"		    /* Datatypes                      */
#include "H5VLprivate.h"	/* VOL plugins                    */
#include "H5VLdsm.h"      /* DSM VOL plugin                 */

/* The driver identification number, initialized at runtime */
static hid_t H5VL_DSM_g = 0;


/* Prototypes */
static herr_t H5VL_dsm_term(void);
static hid_t  H5VL_dsm_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static hid_t  H5VL_dsm_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_dsm_file_close(hid_t file_id, hid_t req);

H5VL_class_t H5VL_dsm_g = {
    "dsm",					/* name */
    0,                                          /* nrefs */
    H5VL_dsm_term,                              /* terminate */
    {                                           /* attribute_cls */
        NULL,                                   /* create */
        NULL,                                   /* open */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL,                                   /* get */
        NULL,                                   /* remove */
        NULL                                    /* close */
    },
    {                                           /* datatype_cls */
        NULL,                                   /* commit */
        NULL,                                   /* open */
        NULL                                    /* close */
    },
    {                                           /* dataset_cls */
        NULL,                                   /* create */
        NULL,                                   /* open */
        NULL,                                   /* read */
        NULL,                                   /* write */
        NULL,                                   /* set extent */
        NULL,                                   /* get */
        NULL                                    /* close */
    },
    {                                           /* file_cls */
        H5VL_dsm_file_create,                   /* create */
        H5VL_dsm_file_open,                     /* open */
        NULL,                                   /* flush */
        NULL,                                   /* get */
        NULL,                                   /* misc */
        NULL,                                   /* optional */
        H5VL_dsm_file_close                     /* close */
    },
    {                                           /* group_cls */
        NULL,                                   /* create */
        NULL,                                   /* move */
        NULL,                                   /* get */
        NULL                                    /* remove */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* move */
        NULL,                                   /* get */
        NULL                                    /* remove */
    },
    {                                           /* object_cls */
        NULL,                                   /* open */
        NULL,                                   /* copy */
        NULL,                                   /* lookup */
        NULL,                                   /* free location */
        NULL,                                   /* get */
        NULL,                                   /* misc */
        NULL,                                   /* optional */
        NULL                                    /* close */
    }
};

/*--------------------------------------------------------------------------
NAME
   H5VL_dsm_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_dsm_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_dsm_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_dsm_init_interface(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_dsm_init_interface() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dsm_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the dsm plugin.
 *		Failure:	Negative.
 *
 * Programmer:	Jerome Soumagne
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_dsm_init(void)
{
  H5VL_class_t *ret_value = NULL;            /* Return value */

  FUNC_ENTER_NOAPI_NOINIT_NOERR

  /* Set return value */
  ret_value = &H5VL_dsm_g;
  ret_value->nrefs ++;

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dsm_init() */

/*---------------------------------------------------------------------------
 * Function:	H5VL_dsm_term
 *
 * Purpose:	Shut down the VOL plugin
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Jerome Soumagne
 *              May, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_dsm_term(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  /* Reset VOL ID */
  H5VL_DSM_g = 0;
  H5VL_dsm_g.nrefs = 0;

  FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_dsm_term() */

/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_dsm
 *
 * Purpose:	Modify the file access property list to use the H5VL_dsm
 *		plugin defined in this source file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Jerome Soumagne
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_dsm_vol(hid_t fapl_id)
{
  H5P_genplist_t *plist;      /* Property list pointer */
  herr_t ret_value;

  FUNC_ENTER_API(FAIL)
  H5TRACE1("e", "i", fapl_id);

  if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_vol(plist, &H5VL_dsm_g);

  done:
  FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_dsm() */

/*-------------------------------------------------------------------------
 * Function:  H5VL_dsm_create
 *
 * Purpose: Creates a file as a dsm HDF5 file.
 *
 * Return:  Success:  A pointer to a new file data structure.
 *    Failure:  NULL
 *
 * Programmer:  Jerome Soumagne
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_dsm_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t UNUSED req)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  printf ("dsm CREATE\n");

  FUNC_LEAVE_NOAPI(2)
} /* end H5VL_dsm_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dsm_open
 *
 * Purpose:	Opens a file as a dsm HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		Failure:	NULL
 *
 * Programmer:  Jerome Soumagne
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL_dsm_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t UNUSED req)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  printf ("dsm OPEN\n");

  FUNC_LEAVE_NOAPI(1)
} /* end H5VL_dsm_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dsm_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:	0
 *		Failure:	-1, file not closed.
 *
 * Programmer:  Jerome Soumagne
 *              May, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dsm_file_close(hid_t file_id, hid_t UNUSED req)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  printf ("dsm CLOSE\n");

  FUNC_LEAVE_NOAPI(1)
} /* end H5VL_dsm_close() */
