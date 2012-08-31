/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmTest.h

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

#ifndef __H5FDdsmTest_h
#define __H5FDdsmTest_h

#include "H5FDdsmManager.h"

#define H5CHECK_ERROR(var, msg) if (var<0) printf("Error %s", msg);

// Names of datasets and groups
#define GROUPNAME       "Step#0"
#define TIME_ATTRIBUTE  "TimeValue"
#define DATASETNAME     "Position"

#define LOOPS          1
#define AVERAGE       20
#define SKIP          10
#define NUM_DATASETS   1
#define DIM_DATASETS   3
#define TYPES          1 // 2 if additional disk output test required

extern "C" {

typedef struct ParticleBuffer {
  H5FDdsmFloat64 *Ddata;
} ParticleBuffer_t;

void initBuffer(ParticleBuffer_t *buffer);
void freeBuffer(ParticleBuffer_t *buffer);

// pointer to a function, either HDF5 or DSM write function
typedef void (*FuncPointer)(ParticleBuffer*, H5FDdsmConstString, H5FDdsmUInt64,
    H5FDdsmUInt64, H5FDdsmUInt32, H5FDdsmUInt64, H5FDdsmUInt64, MPI_Comm, H5FDdsmManager*);

void particleWriteHdf(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 ntuples, H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets,
    H5FDdsmUInt64 start, H5FDdsmUInt64 total, MPI_Comm comm, H5FDdsmManager *dsmManager);

const FuncPointer usingHDF = particleWriteHdf;

void particleReadHdf(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 ntuples, H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets,
    H5FDdsmUInt64 start, H5FDdsmUInt64 total, MPI_Comm comm, H5FDdsmManager *dsmManager
);

H5FDdsmFloat64 TestParticleWrite(H5FDdsmConstString filename, H5FDdsmUInt64 ntuples,
    H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets, H5FDdsmInt32 rank, H5FDdsmInt32 size,
    MPI_Comm comm, H5FDdsmManager *dsmManager, FuncPointer pointer);

H5FDdsmFloat64 TestParticleRead(H5FDdsmConstString filename, H5FDdsmUInt64 ntuples,
    H5FDdsmUInt64 ncomponents, H5FDdsmUInt32 ndatasets, H5FDdsmInt32 rank, H5FDdsmInt32 size,
    MPI_Comm comm, H5FDdsmManager *dsmManager, bool checkresults=true);

void receiverInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm);
void receiverFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm);

void senderInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm,
    H5FDdsmInt32 *dataSizeMB=NULL);
void senderFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm);

}
#endif /* __H5FDdsmTest_h */
