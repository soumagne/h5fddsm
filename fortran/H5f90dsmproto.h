/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef _H5f90dsmproto_H
#define _H5f90dsmproto_H

#include "H5f90.h"
#include "H5public.h"

#define nh5pset_fapl_dsm_c H5_FC_FUNC_(h5pset_fapl_dsm_c, H5PSET_FAPL_DSM_C)
int_f nh5pset_fapl_dsm_c(hid_t_f *prp_id, int_f* increment);

//#   define nh5pget_fapl_dsm_c         H5_FC_FUNC_(h5pget_fapl_dsm_c, H5PGET_FAPL_DSM_C)
//int_f nh5pget_fapl_dsm_c(hid_t_f *prp_id, int_f* comm, int_f* info);

#endif /* _H5f90dsmproto_H */
