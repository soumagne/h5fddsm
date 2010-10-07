/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Project                 : vtkCSCS                                        *
 *  Module                  : h5dump.h                                       *
 *  Revision of last commit : $Rev: 1466 $                                   *
 *  Author of last commit   : $Author: biddisco $                            *
 *  Date of last commit     : $Date:: 2009-12-04 13:32:32 +0100 #$           *
 *                                                                           *
 *  Copyright (C) CSCS - Swiss National Supercomputing Centre.               *
 *  You may use modify and and distribute this code freely providing         *
 *  1) This copyright notice appears on all copies of source code            *
 *  2) An acknowledgment appears with any substantial usage of the code      *
 *  3) If this code is contributed to any other open source project, it      *
 *  must not be reformatted such that the indentation, bracketing or         *
 *  overall style is modified significantly.                                 *
 *                                                                           *
 *  This software is distributed WITHOUT ANY WARRANTY; without even the      *
 *  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

/* Modifies H5Dump to add DSM driver support and stream output */

#ifndef H5DUMP_H__
#define H5DUMP_H__

#include "mpi.h"

extern "C" {
#include "hdf5.h"
}

#include "H5FDdsmConfig.h"

#include <sstream>

#define H5DUMP_MAX_RANK     H5S_MAX_RANK

#define ATTRIBUTE_DATA  0
#define DATASET_DATA    1
#define ENUM_DATA       2
#define COL             3

/* Strings for output */
#define ATTRIBUTE       "ATTRIBUTE"
#define BLOCK           "BLOCK"
#define SUPER_BLOCK     "SUPER_BLOCK"
#define COMPRESSION     "COMPRESSION"
#define CONCATENATOR    "//"
#define COMPLEX         "COMPLEX"
#define COUNT           "COUNT"
#define CSET            "CSET"
#define CTYPE           "CTYPE"
#define DATA            "DATA"
#define DATASPACE       "DATASPACE"
#define EXTERNAL        "EXTERNAL"
#define FILENO          "FILENO"
#define HARDLINK        "HARDLINK"
#define NLINK           "NLINK"
#define OBJID           "OBJECTID"
#define OBJNO           "OBJNO"
#define S_SCALAR        "SCALAR"
#define S_SIMPLE        "SIMPLE"
#define S_NULL          "NULL"
#define SOFTLINK        "SOFTLINK"
#define EXTLINK         "EXTERNAL_LINK"
#define UDLINK          "USERDEFINED_LINK"
#define START           "START"
#define STRIDE          "STRIDE"
#define STRSIZE         "STRSIZE"
#define STRPAD          "STRPAD"
#define SUBSET          "SUBSET"
#define FILTERS         "FILTERS"
#define DEFLATE         "COMPRESSION DEFLATE"
#define DEFLATE_LEVEL   "LEVEL"
#define SHUFFLE         "PREPROCESSING SHUFFLE"
#define FLETCHER32      "CHECKSUM FLETCHER32"
#define SZIP            "COMPRESSION SZIP"
#define NBIT            "COMPRESSION NBIT"
#define SCALEOFFSET            "COMPRESSION SCALEOFFSET"
#define SCALEOFFSET_MINBIT            "MIN BITS"
#define STORAGE_LAYOUT  "STORAGE_LAYOUT"
#define CONTIGUOUS      "CONTIGUOUS"
#define COMPACT         "COMPACT"
#define CHUNKED         "CHUNKED"
#define EXTERNAL_FILE   "EXTERNAL_FILE"
#define FILLVALUE       "FILLVALUE"
#define FILE_CONTENTS   "FILE_CONTENTS"

#define BEGIN           "{"
#define END             "}"

typedef struct h5dump_header_t {
    const char *name;
    const char *filebegin;
    const char *fileend;
    const char *bootblockbegin;
    const char *bootblockend;
    const char *groupbegin;
    const char *groupend;
    const char *datasetbegin;
    const char *datasetend;
    const char *attributebegin;
    const char *attributeend;
    const char *datatypebegin;
    const char *datatypeend;
    const char *dataspacebegin;
    const char *dataspaceend;
    const char *databegin;
    const char *dataend;
    const char *softlinkbegin;
    const char *softlinkend;
    const char *extlinkbegin;
    const char *extlinkend;
    const char *udlinkbegin;
    const char *udlinkend;
    const char *subsettingbegin;
    const char *subsettingend;
    const char *startbegin;
    const char *startend;
    const char *stridebegin;
    const char *strideend;
    const char *countbegin;
    const char *countend;
    const char *blockbegin;
    const char *blockend;

    const char *fileblockbegin;
    const char *fileblockend;
    const char *bootblockblockbegin;
    const char *bootblockblockend;
    const char *groupblockbegin;
    const char *groupblockend;
    const char *datasetblockbegin;
    const char *datasetblockend;
    const char *attributeblockbegin;
    const char *attributeblockend;
    const char *datatypeblockbegin;
    const char *datatypeblockend;
    const char *dataspaceblockbegin;
    const char *dataspaceblockend;
    const char *datablockbegin;
    const char *datablockend;
    const char *softlinkblockbegin;
    const char *softlinkblockend;
    const char *extlinkblockbegin;
    const char *extlinkblockend;
    const char *udlinkblockbegin;
    const char *udlinkblockend;
    const char *strblockbegin;
    const char *strblockend;
    const char *enumblockbegin;
    const char *enumblockend;
    const char *structblockbegin;
    const char *structblockend;
    const char *vlenblockbegin;
    const char *vlenblockend;
    const char *subsettingblockbegin;
    const char *subsettingblockend;
    const char *startblockbegin;
    const char *startblockend;
    const char *strideblockbegin;
    const char *strideblockend;
    const char *countblockbegin;
    const char *countblockend;
    const char *blockblockbegin;
    const char *blockblockend;

    const char *dataspacedescriptionbegin;
    const char *dataspacedescriptionend;
    const char *dataspacedimbegin;
    const char *dataspacedimend;

} h5dump_header_t;

// H5 dump common functions
H5FDdsm_EXPORT int H5dump(int argc, char *argv[], void *dsmBuffer);
H5FDdsm_EXPORT int H5dump_xml(const char *fileName, std::ostringstream &);

// DSM specific functions
H5FDdsm_EXPORT int H5dump_dsm(const char *fileName, void *dsmBuffer);
H5FDdsm_EXPORT int H5dump_dsm_light(const char *fileName, void *dsmBuffer);
H5FDdsm_EXPORT int H5dump_dsm_xml(const char *fileName, std::ostringstream &, void *dsmBuffer);

#endif  /* !H5DUMP_H__ */
