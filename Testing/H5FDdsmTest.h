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

extern "C" {

typedef struct ParticleBuffer {
  H5FDdsmFloat64 *Ddata;
} ParticleBuffer_t;

void initBuffer(ParticleBuffer_t *buffer);
void freeBuffer(ParticleBuffer_t *buffer);

// pointer to a function, either HDF5 or DSM write function
typedef void (*FuncPointer)(ParticleBuffer*, H5FDdsmConstString, H5FDdsmUInt64, H5FDdsmUInt64, H5FDdsmUInt64, H5FDdsmUInt64, H5FDdsmBuffer*);

void WriteParticlesHDF5(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 N, H5FDdsmUInt64 C, H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer);

void WriteParticlesDSM(ParticleBuffer *buf, H5FDdsmConstString filename,
    H5FDdsmUInt64 N, H5FDdsmUInt64 C, H5FDdsmUInt64 start, H5FDdsmUInt64 total, H5FDdsmBuffer *dsmBuffer);
 
const FuncPointer usingHDF = WriteParticlesHDF5;
const FuncPointer usingDSM = WriteParticlesDSM;

H5FDdsmFloat64 TestParticleWrite(
    H5FDdsmConstString filename, H5FDdsmUInt64 N, H5FDdsmUInt64 C, H5FDdsmInt32 mpiId, H5FDdsmInt32 mpiNum,
    MPI_Comm dcomm, H5FDdsmBuffer *dsmBuffer, FuncPointer pointer);

void particle_read(
    ParticleBuffer_t *buf, const char *filename, int rank,
    H5FDdsmBuffer *dsmBuffer);

void receiverInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm);
void receiverFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm);

void senderInit(int argc, char* argv[], H5FDdsmManager *dsmManager, MPI_Comm *comm, H5FDdsmInt32 *dataSizeMB=NULL);
void senderFinalize(H5FDdsmManager *dsmManager, MPI_Comm *comm);

}
#endif /* __H5FDdsmTest_h */
