/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5VLdso.c

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

#define H5A_PACKAGE   /*suppress error about including H5Apkg   */
#define H5D_PACKAGE   /*suppress error about including H5Dpkg   */
#define H5F_PACKAGE   /*suppress error about including H5Fpkg   */
#define H5G_PACKAGE   /*suppress error about including H5Gpkg   */
#define H5L_PACKAGE   /*suppress error about including H5Lpkg   */
#define H5O_PACKAGE   /*suppress error about including H5Opkg   */
#define H5R_PACKAGE   /*suppress error about including H5Rpkg   */
#define H5T_PACKAGE   /*suppress error about including H5Tpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_dso_init_interface


#include "H5private.h"      /* Generic Functions              */
#include "H5Aprivate.h"     /* Attributes                     */
#include "H5Dpkg.h"         /* Dataset pkg                    */
#include "H5Dprivate.h"     /* Datasets                       */
#include "H5Eprivate.h"     /* Error handling                 */
#include "H5Fprivate.h"     /* File access                    */
#include "H5Fpkg.h"         /* File pkg                       */
#include "H5FLprivate.h"    /* Free lists                     */
#include "H5Gpkg.h"         /* Groups                         */
#include "H5Iprivate.h"     /* IDs                            */
#include "H5MFprivate.h"    /* File memory management         */
#include "H5MMprivate.h"    /* Memory management              */
#include "H5Opkg.h"         /* Object headers                 */
#include "H5Pprivate.h"     /* Property lists                 */
#include "H5SMprivate.h"    /* Shared Object Header Messages  */
#include "H5Tpkg.h"         /* Datatypes                      */
#include "H5VLprivate.h"    /* VOL plugins                    */
#include "H5VLdso.h"        /* DSO VOL plugin                 */

/* The driver identification number, initialized at runtime */
static hid_t H5VL_DSO_g = 0;


/* Prototypes */
static herr_t H5VL_dso_term(void);

static void *H5VL_dso_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
static void *H5VL_dso_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
static herr_t H5VL_dso_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
static herr_t H5VL_dso_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
static herr_t H5VL_dso_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
static herr_t H5VL_dso_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_dso_dataset_close(void *dset, hid_t req);

static void *H5VL_dso_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
static void *H5VL_dso_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
static herr_t H5VL_dso_file_close(void *file, hid_t req);

static void *H5VL_dso_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
static void *H5VL_dso_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
static herr_t H5VL_dso_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
static herr_t H5VL_dso_group_close(void *grp, hid_t req);

H5VL_class_t H5VL_dso_g = {
    "dso",                                      /* name */
    NULL,                                       /* initialize */
    H5VL_dso_term,                              /* terminate */
    0,                                          /* fapl_size */
    NULL,                                       /* fapl_copy */
    NULL,                                       /* fapl_free */
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
        NULL,                                   /* get_size */
        NULL                                    /* close */
    },
    {                                           /* dataset_cls */
        H5VL_dso_dataset_create,                /* create */
        H5VL_dso_dataset_open,                  /* open */
        H5VL_dso_dataset_read,                  /* read */
        H5VL_dso_dataset_write,                 /* write */
        H5VL_dso_dataset_set_extent,            /* set extent */
        H5VL_dso_dataset_get,                   /* get */
        H5VL_dso_dataset_close                  /* close */
    },
    {                                           /* file_cls */
        H5VL_dso_file_create,                   /* create */
        H5VL_dso_file_open,                     /* open */
        NULL,                                   /* flush */
        NULL,                                   /* get */
        NULL,                                   /* misc */
        NULL,                                   /* optional */
        H5VL_dso_file_close                     /* close */
    },
    {                                           /* group_cls */
        H5VL_dso_group_create,                  /* create */
        H5VL_dso_group_open,                    /* open */
        H5VL_dso_group_get,                     /* get */
        H5VL_dso_group_close                    /* close */
    },
    {                                           /* link_cls */
        NULL,                                   /* create */
        NULL,                                   /* move */
        NULL,                                   /* iterate */
        NULL,                                   /* get */
        NULL                                    /* remove */
    },
    {                                           /* object_cls */
        NULL,                                   /* open */
        NULL,                                   /* copy */
        NULL,                                   /* visit */
//        NULL,                                   /* lookup */
//        NULL,                                   /* free location */
        NULL,                                   /* get */
        NULL,                                   /* misc */
        NULL,                                   /* optional */
        NULL                                    /* close */
    }
};

/*--------------------------------------------------------------------------
NAME
   H5VL_dso_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_dso_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_dso_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_dso_init_interface(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_dso_init_interface() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dso_init
 *
 * Purpose:	Initialize this vol plugin by registering the driver with the
 *		library.
 *
 * Return:	Success:	The ID for the dso plugin.
 *		Failure:	Negative.
 *
 *-------------------------------------------------------------------------
 */
H5VL_class_t *
H5VL_dso_init(void)
{
  H5VL_class_t *ret_value = NULL;            /* Return value */

  FUNC_ENTER_NOAPI_NOINIT_NOERR

  /* TODO Necessary or not ? */
//  if (H5I_VOL != H5Iget_type(H5VL_dso_g)) {
//    H5VL_DSO_g = H5VL_register(&H5VL_dso_g, sizeof(H5VL_class_t), FALSE);
//  }

  /* Set return value */
  ret_value = &H5VL_dso_g;

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_init() */

/*---------------------------------------------------------------------------
 * Function:    H5VL_dso_term
 *
 * Purpose:	    Shut down the VOL plugin
 *
 * Returns:     Non-negative on success or negative on failure
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_term(void)
{
  FUNC_ENTER_NOAPI_NOINIT_NOERR

  /* Reset VOL ID */
  H5VL_DSO_g = 0;

  FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_dso_term() */

/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_dso
 *
 * Purpose:	    Modify the file access property list to use the H5VL_DSO
 *	            plugin defined in this source file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_dso(hid_t fapl_id)
{
  H5P_genplist_t *plist;      /* Property list pointer */
  herr_t ret_value;

  FUNC_ENTER_API(FAIL)
  H5TRACE1("e", "i", fapl_id);

  if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

  ret_value = H5P_set_vol(plist, &H5VL_dso_g, NULL);

done:
  FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_dso() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_create
 *
 * Purpose:     Creates a dataset inside a DSO H5 file.
 *
 * Return:  Success:    dataset id.
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id,
    hid_t dapl_id, hid_t UNUSED req)
{
  H5P_genplist_t *plist;             /* Property list pointer */
  H5G_loc_t       loc;               /* Object location to insert dataset into */
  hid_t           type_id, space_id, lcpl_id;
  H5D_t          *dset = NULL;       /* New dataset's info */
  const H5S_t    *space;             /* Dataspace for dataset */
  void           *ret_value;

  FUNC_ENTER_NOAPI_NOINIT

  /* Get the plist structure */
  if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
    HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

  /* get creation properties */
  if(H5P_get(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for datatype id")
  if(H5P_get(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for space id")
  if(H5P_get(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

  /* Check arguments */
  if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")
  if(H5I_DATATYPE != H5I_get_type(type_id))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype ID")
  if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace ID")

  /* H5Dcreate_anon */
  if(NULL == name) {
    /* build and open the new dataset */
    if(NULL == (dset = H5D__create(loc.oloc->file, type_id, space, dcpl_id, dapl_id, H5AC_dxpl_id)))
      HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")
  }
  /* H5Dcreate2 */
  else {
    /* Create the new dataset & get its ID */
    if(NULL == (dset = H5D__create_named(&loc, name, type_id, space, lcpl_id,
        dcpl_id, dapl_id, H5AC_dxpl_id)))
      HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")
  }
  ret_value = (void *)dset;

done:
  if (err_occurred) {
    /* Nothing */
  }

  if(NULL == name) {
    /* Release the dataset's object header, if it was created */
    if(dset) {
      H5O_loc_t *oloc;         /* Object location for dataset */

      /* Get the new dataset's object location */
      if(NULL == (oloc = H5D_oloc(dset)))
        HDONE_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "unable to get object location of dataset")

      /* Decrement refcount on dataset's object header in memory */
      if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
    } /* end if */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_create() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_open
 *
 * Purpose: Opens a dataset inside a dso h5 file.
 *
 * Return:  Success:    dataset id.
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name,
    hid_t dapl_id, hid_t UNUSED req)
{
  H5D_t      *dset = NULL;
  H5G_loc_t   loc;                    /* Object location of group */
  H5G_loc_t   dset_loc;               /* Object location of dataset */
  H5G_name_t  path;                   /* Dataset group hier. path */
  H5O_loc_t   oloc;                   /* Dataset object location */
  H5O_type_t  obj_type;               /* Type of object at location */
  hbool_t     loc_found = FALSE;      /* Location at 'name' found */
  hid_t       dxpl_id = H5AC_dxpl_id; /* dxpl to use to open datset */
  void       *ret_value = NULL;

  FUNC_ENTER_NOAPI_NOINIT

  if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

  /* Set up dataset location to fill in */
  dset_loc.oloc = &oloc;
  dset_loc.path = &path;
  H5G_loc_reset(&dset_loc);

  /* Find the dataset object */
  if(H5G_loc_find(&loc, name, &dset_loc, dapl_id, dxpl_id) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found")
  loc_found = TRUE;

  /* Check that the object found is the correct type */
  if(H5O_obj_type(&oloc, &obj_type, dxpl_id) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get object type")
  if(obj_type != H5O_TYPE_DATASET)
    HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, NULL, "not a dataset")

  /* Open the dataset */
  if(NULL == (dset = H5D_open(&dset_loc, dapl_id, dxpl_id)))
    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't open dataset")

  ret_value = (void *)dset;

done:
  if (err_occurred) {
    /* Nothing */
  }

  if(NULL == dset && loc_found && H5G_loc_free(&dset_loc) < 0)
    HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, NULL, "can't free location")

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_open() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_read
 *
 * Purpose: Reads raw data from a dataset into a buffer.
 *
 * Return:  Success:    0
 *          Failure:    -1, dataset not readd.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_dataset_read(void *obj, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t plist_id, void *buf, hid_t UNUSED req)

{
  H5D_t         *dset = (H5D_t *)obj;
  const H5S_t   *mem_space = NULL;
  const H5S_t   *file_space = NULL;
  char           fake_char;
  herr_t         ret_value = SUCCEED;                 /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  /* check arguments */
  if(NULL == dset->oloc.file)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
  if(H5S_ALL != mem_space_id) {
    if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    /* Check for valid selection */
    if(H5S_SELECT_VALID(mem_space) != TRUE)
      HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
  } /* end if */
  if(H5S_ALL != file_space_id) {
    if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    /* Check for valid selection */
    if(H5S_SELECT_VALID(file_space) != TRUE)
      HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
  } /* end if */

  if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

  /* If the buffer is nil, and 0 element is selected, make a fake buffer.
   * This is for some MPI package like ChaMPIon on NCSA's tungsten which
   * doesn't support this feature.
   */
  if(!buf)
    buf = &fake_char;

  /* read raw data */
  if(H5D__read(dset, mem_type_id, mem_space, file_space, plist_id, buf/*out*/) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_read() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_write
 *
 * Purpose: Writes raw data from a buffer into a dataset.
 *
 * Return:  Success:    0
 *          Failure:    -1, dataset not writed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_dataset_write(void *obj, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, const void *buf, hid_t UNUSED req)
{
  H5D_t         *dset = (H5D_t *)obj;
  const H5S_t   *mem_space = NULL;
  const H5S_t   *file_space = NULL;
  char           fake_char;
  herr_t         ret_value = SUCCEED;                 /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  /* check arguments */
  if(NULL == dset->oloc.file)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
  if(H5S_ALL != mem_space_id) {
    if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    /* Check for valid selection */
    if(H5S_SELECT_VALID(mem_space) != TRUE)
      HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
  } /* end if */
  if(H5S_ALL != file_space_id) {
    if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
      HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

  /* Check for valid selection */
  if(H5S_SELECT_VALID(file_space) != TRUE)
    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
  } /* end if */

  if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

  /* If the buffer is nil, and 0 element is selected, make a fake buffer.
   * This is for some MPI package like ChaMPIon on NCSA's tungsten which
   * doesn't support this feature.
   */
  if(!buf)
    buf = &fake_char;

  /* write raw data */
  if(H5D__write(dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_write() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_set_extent
 *
 * Purpose: Set Extent of dataset
 *
 * Return:  Success:    0
 *          Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_dataset_set_extent(void *obj, const hsize_t size[], hid_t UNUSED req)
{
  H5D_t       *dset = (H5D_t *)obj;
  herr_t       ret_value = SUCCEED;    /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  /* Private function */
  if(H5D__set_extent(dset, size, H5AC_dxpl_id) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extend dataset")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_set_extent() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_get
 *
 * Purpose: Gets certain information about a dataset
 *
 * Return:  Success:    0
 *          Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_dataset_get(void *obj, H5VL_dataset_get_t get_type, hid_t UNUSED req, va_list arguments)
{
  H5D_t       *dset = (H5D_t *)obj;
  herr_t       ret_value = SUCCEED;    /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  switch (get_type) {
    /* H5Dget_space */
    case H5VL_DATASET_GET_SPACE:
    {
      hid_t   *ret_id = va_arg (arguments, hid_t *);

      if((*ret_id = H5D_get_space(dset)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get space ID of dataset")

      break;
    }
    /* H5Dget_space_statuc */
    case H5VL_DATASET_GET_SPACE_STATUS:
    {
      H5D_space_status_t *allocation = va_arg (arguments, H5D_space_status_t *);

      /* Read data space address and return */
      if(H5D__get_space_status(dset, allocation, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get space status")

      break;
    }
    /* H5Dget_type */
    case H5VL_DATASET_GET_TYPE:
    {
      hid_t   *ret_id = va_arg (arguments, hid_t *);

      if((*ret_id = H5D_get_type(dset)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get datatype ID of dataset")

      break;
    }
    /* H5Dget_create_plist */
    case H5VL_DATASET_GET_DCPL:
    {
      hid_t   *ret_id = va_arg (arguments, hid_t *);

      if((*ret_id = H5D_get_create_plist(dset)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for dataset")

      break;
    }
    /* H5Dget_access_plist */
    case H5VL_DATASET_GET_DAPL:
    {
      hid_t   *ret_id = va_arg (arguments, hid_t *);

      if((*ret_id = H5D_get_access_plist(dset)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get access property list for dataset")

      break;
    }
    /* H5Dget_storage_size */
    case H5VL_DATASET_GET_STORAGE_SIZE:
    {
      hsize_t *ret = va_arg (arguments, hsize_t *);

      /* Set return value */
      if(H5D__get_storage_size(dset, H5AC_ind_dxpl_id, ret) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, 0, "can't get size of dataset's storage")
      break;
    }
    /* H5Dget_offset */
    case H5VL_DATASET_GET_OFFSET:
    {
      haddr_t *ret = va_arg (arguments, haddr_t *);

      /* Set return value */
      *ret = H5D__get_offset(dset);
      if(!H5F_addr_defined(*ret))
        *ret = HADDR_UNDEF;
      break;
    }
    default:
      HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from dataset")
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_get() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_dataset_close
 *
 * Purpose: Closes a dataset.
 *
 * Return:  Success:    0
 *          Failure:    -1, dataset not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_dataset_close(void *dset, hid_t UNUSED req)
{
  herr_t ret_value = SUCCEED;                 /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  if(H5D_close((H5D_t*)dset) < 0)
    HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't close dataset")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_dataset_close() */

/*-------------------------------------------------------------------------
 * Function:  H5VL_dso_create
 *
 * Purpose: Creates a file as a dso HDF5 file.
 *
 * Return:  Success:  A pointer to a new file data structure.
 *          Failure:  NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
                        hid_t UNUSED req)
{
  H5F_t *new_file = NULL;
  void  *ret_value = NULL;

  FUNC_ENTER_NOAPI_NOINIT

  /*
   * Adjust bit flags by turning on the creation bit and making sure that
   * the EXCL or TRUNC bit is set.  All newly-created files are opened for
   * reading and writing.
   */
  if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
    flags |= H5F_ACC_EXCL;   /*default*/
  flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

  /* Create the file */
  if(NULL == (new_file = H5F_open(name, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file")

  new_file->id_exists = TRUE;
  ret_value = (void *)new_file;

done:
  if (err_occurred) {
    /* Nothing */
  }

  if(NULL == ret_value && new_file)
    if(H5F_close(new_file) < 0)
      HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dso_open
 *
 * Purpose:	Opens a file as a DSO HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. 
 *		    Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t UNUSED req)
{
  H5F_t *new_file = NULL;
  void  *ret_value = NULL;

  FUNC_ENTER_NOAPI_NOINIT

  /* Open the file */
  if(NULL == (new_file = H5F_open(name, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

  new_file->id_exists = TRUE;
  ret_value = (void *)new_file;

done:
  if (err_occurred) {
    /* Nothing */
  }

  if(NULL == ret_value && new_file && H5F_try_close(new_file) < 0)
    HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_open() */

/*-------------------------------------------------------------------------
 * Function: H5VL_dso_close
 *
 * Purpose:	Closes a file.
 *
 * Return:	Success:    0
 *          Failure:    -1, file not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_file_close(void *file, hid_t UNUSED req)
{
//  int nref;
  H5F_t *f = (H5F_t *)file;
//  hid_t file_id = FAIL;
  herr_t ret_value = SUCCEED;                 /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  /* Flush file if this is the last reference to this id and we have write
   * intent, unless it will be flushed by the "shared" file being closed.
   * This is only necessary to replicate previous behaviour, and could be
   * disabled by an option/property to improve performance. */
//  if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
    /* get the file ID corresponding to the H5F_t struct */
//    if((file_id = H5I_get_id(f, H5I_FILE)) < 0)
//      HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "invalid atom")
    /* get the number of references outstanding for this file ID */
//    if((nref = H5I_get_ref(file_id, FALSE)) < 0)
//      HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
//    if(nref == 1)
//      if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
//        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
//  } /* end if */
  /* close the file */
  if(H5F_close(f) < 0)
    HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "can't close file")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_close() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_group_create
 *
 * Purpose: Creates a group inside a DSO h5 file.
 *
 * Return:  Success:    group id.
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id,
                         hid_t gapl_id, hid_t UNUSED req)
{
  H5P_genplist_t *plist;      /* Property list pointer */
  H5G_loc_t       loc;        /* Location to create group */
  H5G_t          *grp = NULL; /* New group created */
  hid_t           lcpl_id;
  void           *ret_value;

  FUNC_ENTER_NOAPI_NOINIT

  /* Get the plist structure */
  if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
    HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "can't find object for ID")

  /* get creation properties */
  if(H5P_get(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get property value for lcpl id")

  if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

  /* if name is NULL then this is from H5Gcreate_anon */
  if (name == NULL) {
    H5G_obj_create_t gcrt_info;         /* Information for group creation */
    /* Set up group creation info */
    gcrt_info.gcpl_id = gcpl_id;
    gcrt_info.cache_type = H5G_NOTHING_CACHED;
    HDmemset(&gcrt_info.cache, 0, sizeof(gcrt_info.cache));

    /* Create the new group & get its ID */
    if(NULL == (grp = H5G__create(loc.oloc->file, &gcrt_info, H5AC_dxpl_id)))
      HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")
  }
  /* otherwise it's from H5Gcreate */
  else {
    /* Create the new group & get its ID */
    if(NULL == (grp = H5G__create_named(&loc, name, lcpl_id, gcpl_id, gapl_id, H5AC_dxpl_id)))
      HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group")
  }
  ret_value = (void *)grp;

done:
  if (err_occurred) {
    /* Nothing */
  }

  if (name == NULL) {
    /* Release the group's object header, if it was created */
    if(grp) {
      H5O_loc_t *oloc;         /* Object location for group */

      /* Get the new group's object location */
      if(NULL == (oloc = H5G_oloc(grp)))
        HDONE_ERROR(H5E_SYM, H5E_CANTGET, NULL, "unable to get object location of group")

      /* Decrement refcount on group's object header in memory */
      if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object")
    } /* end if */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_group_create() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_group_open
 *
 * Purpose: Opens a group inside a DSO h5 file.
 *
 * Return:  Success:    group id.
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5VL_dso_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t UNUSED req)
{
  H5G_loc_t   loc;        /* Location to open group */
  H5G_t      *grp = NULL; /* New group opend */
  void       *ret_value;

  FUNC_ENTER_NOAPI_NOINIT

  if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file or file object")

  /* Open the group */
  if((grp = H5G__open_name(&loc, name, gapl_id, H5AC_dxpl_id)) == NULL)
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open group")

  ret_value = (void *)grp;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_group_open() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_group_get
 *
 * Purpose: Gets certain data about a group
 *
 * Return:  Success:    0
 *          Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_group_get(void *obj, H5VL_group_get_t get_type, hid_t UNUSED req, va_list arguments)
{
  herr_t ret_value = SUCCEED;    /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  switch (get_type) {
    /* H5Gget_create_plist */
    case H5VL_GROUP_GET_GCPL:
    {
      hid_t *new_gcpl_id = va_arg (arguments, hid_t *);
      H5G_t *grp = (H5G_t *)obj;

      if((*new_gcpl_id = H5G_get_create_plist(grp)) < 0)
          HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "can't get creation property list for group")
      break;
    }
    /* H5Gget_info */
    case H5VL_GROUP_GET_INFO:
    {
      H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
      H5G_info_t  *grp_info = va_arg (arguments, H5G_info_t *);
      H5G_loc_t    loc;

      if(H5G_loc_real(obj, loc_params.obj_type, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

      if(loc_params.type == H5VL_OBJECT_BY_SELF) { /* H5Gget_info */
        /* Retrieve the group's information */
        if(H5G__obj_info(loc.oloc, grp_info, H5AC_ind_dxpl_id) < 0)
          HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
      }
      else if(loc_params.type == H5VL_OBJECT_BY_NAME) { /* H5Gget_info_by_name */
        H5G_loc_t   grp_loc;                /* Location used to open group */
        H5G_name_t  grp_path;             /* Opened object group hier. path */
        H5O_loc_t   grp_oloc;             /* Opened object object location */

        /* Set up opened group location to fill in */
        grp_loc.oloc = &grp_oloc;
        grp_loc.path = &grp_path;
        H5G_loc_reset(&grp_loc);

        /* Find the group object */
        if(H5G_loc_find(&loc, loc_params.loc_data.loc_by_name.name, &grp_loc/*out*/,
            loc_params.loc_data.loc_by_name.plist_id, H5AC_ind_dxpl_id) < 0)
          HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")

        /* Retrieve the group's information */
        if(H5G__obj_info(grp_loc.oloc, grp_info/*out*/, H5AC_ind_dxpl_id) < 0) {
          H5G_loc_free(&grp_loc);
          HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
        }

        /* Release the object location */
        if(H5G_loc_free(&grp_loc) < 0)
          HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
      }
      else if(loc_params.type == H5VL_OBJECT_BY_IDX) { /* H5Gget_info_by_idx */
        H5G_loc_t   grp_loc;                /* Location used to open group */
        H5G_name_t  grp_path;             /* Opened object group hier. path */
        H5O_loc_t   grp_oloc;             /* Opened object object location */

        /* Set up opened group location to fill in */
        grp_loc.oloc = &grp_oloc;
        grp_loc.path = &grp_path;
        H5G_loc_reset(&grp_loc);

        /* Find the object's location, according to the order in the index */
        if(H5G_loc_find_by_idx(&loc, loc_params.loc_data.loc_by_idx.name,
            loc_params.loc_data.loc_by_idx.idx_type,
            loc_params.loc_data.loc_by_idx.order,
            loc_params.loc_data.loc_by_idx.n, &grp_loc/*out*/,
            loc_params.loc_data.loc_by_idx.plist_id,
            H5AC_ind_dxpl_id) < 0)
          HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")

        /* Retrieve the group's information */
        if(H5G__obj_info(grp_loc.oloc, grp_info/*out*/, H5AC_ind_dxpl_id) < 0) {
          H5G_loc_free(&grp_loc);
          HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve group info")
        }

        /* Release the object location */
        if(H5G_loc_free(&grp_loc) < 0)
          HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
      }
      else {
        HGOTO_ERROR(H5E_VOL, H5E_UNSUPPORTED, FAIL, "unknown get info parameters")
      }
      break;
    }
    default:
      HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group")
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_group_get() */

/*-------------------------------------------------------------------------
 * Function:    H5VL_dso_group_close
 *
 * Purpose: Closes a group.
 *
 * Return:  Success:    0
 *          Failure:    -1, group not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL_dso_group_close(void *grp, hid_t UNUSED req)
{
  herr_t ret_value = SUCCEED;                 /* Return value */

  FUNC_ENTER_NOAPI_NOINIT

  if(H5G_close((H5G_t *)grp) < 0)
    HGOTO_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close group")

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_dso_group_close() */
