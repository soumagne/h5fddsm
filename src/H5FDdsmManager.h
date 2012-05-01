/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmManager.h

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
// .NAME H5FDdsmManager - Create/Expose a DSM buffer to an external application
// .SECTION Description
// Create/Expose a DSM buffer to an external application

#ifndef __H5FDdsmManager_h
#define __H5FDdsmManager_h

#include <hdf5.h>
#include "H5FDdsmBufferService.h"
#include "H5FDdsmComm.h"
#include <stack>

#ifdef H5FDdsm_HAVE_STEERING
class H5FDdsmSteerer;
#endif
struct H5FDdsmManagerInternals;

class H5FDdsm_EXPORT H5FDdsmManager : public H5FDdsmObject
{
  public:
     H5FDdsmManager();
    ~H5FDdsmManager();

    // Description:
    // Get the process rank and communicator size
    H5FDdsmGetValueMacro(UpdatePiece, H5FDdsmInt32);
    H5FDdsmGetValueMacro(UpdateNumPieces, H5FDdsmInt32);

    // Description:
    // Set/Get the MPI Communicator used by this DSM manager
    void SetMpiComm(MPI_Comm comm);
    H5FDdsmGetValueMacro(MpiComm, MPI_Comm);

    // Description:
    // Get the DSM buffer used by this DSM manager
    H5FDdsmGetValueMacro(DsmBuffer, H5FDdsmBufferService*);

    // Description:
    // Set/Get the size of the buffer to be reserved on this process
    // the DSM total size will be the sum of the local sizes from all processes
    H5FDdsmSetValueMacro(LocalBufferSizeMBytes, H5FDdsmUInt32);
    H5FDdsmGetValueMacro(LocalBufferSizeMBytes, H5FDdsmUInt32);

    // Is the DSMBuffer auto allocated within the driver or not
    H5FDdsmGetValueMacro(IsAutoAllocated, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsAutoAllocated, H5FDdsmBoolean);

    // Description:
    // Set/Get IsServer info
    H5FDdsmSetValueMacro(IsServer, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(IsServer, H5FDdsmBoolean);

    // Description:
    // Set/Get IsStandAlone info
    H5FDdsmSetValueMacro(IsStandAlone, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(IsStandAlone, H5FDdsmBoolean);

    // Description:
    // Set/Get IsDriverSerial info
    H5FDdsmSetValueMacro(IsDriverSerial, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(IsDriverSerial, H5FDdsmBoolean);

    // Description:
    // Set/Get the interprocess communication subsystem
    // Valid values are H5FD_DSM_TYPE_UNIFORM, H5FD_DSM_TYPE_BLOCK_CYCLIC
    H5FDdsmSetValueMacro(DsmType, H5FDdsmInt32);
    H5FDdsmGetValueMacro(DsmType, H5FDdsmInt32);

    // Description:
    // Set/Get the DSM block length when using H5FD_DSM_TYPE_BLOCK_CYCLIC
    H5FDdsmSetValueMacro(BlockLength, H5FDdsmUInt64);
    H5FDdsmGetValueMacro(BlockLength, H5FDdsmUInt64);

    // Description:
    // Set/Get the interprocess communication subsystem
    // Valid values are: - H5FD_DSM_COMM_MPI
    //                   - H5FD_DSM_COMM_SOCKET
    //                   - H5FD_DSM_COMM_MPI_RMA
    //                   - H5FD_DSM_COMM_DMAPP
    H5FDdsmSetValueMacro(InterCommType, H5FDdsmInt32);
    H5FDdsmGetValueMacro(InterCommType, H5FDdsmInt32);

    // Description:
    // Set/Get UseStaticInterComm -- Force to use static MPI comm model
    // when dynamic MPI communication is not supported by the system
    H5FDdsmSetValueMacro(UseStaticInterComm, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(UseStaticInterComm, H5FDdsmBoolean);

    // Description:
    // Set/Get the published host name of our connection.
    // Real value valid after a Publish call has been made.
    H5FDdsmSetStringMacro(ServerHostName);
    H5FDdsmGetStringMacro(ServerHostName);

    // Description:
    // Set/Get the published port of our connection.
    // Real value valid after a Publish call has been made.
    H5FDdsmSetValueMacro(ServerPort, H5FDdsmInt32);
    H5FDdsmGetValueMacro(ServerPort, H5FDdsmInt32);

    // Description:
    // Wait for a connection - Only valid after a Publish call has been made.
    H5FDdsmBoolean GetIsConnected();
    H5FDdsmBoolean GetIsActive();

    H5FDdsmInt32 WaitForConnection();

    // Description:
    H5FDdsmInt32 WaitForUnlock();
    H5FDdsmInt32 GetUnlockStatus();

    // Description:
    // Create a new DSM buffer of type DsmType using a local length of
    // LocalBufferSizeMBytes and the given MpiComm.
    // If using inter communicators, additional options may be specified
    // such as ServerHostname, ServerPort, IsServer, InterCommType, etc.
    H5FDdsmInt32 Create();

    // Description:
    // Destroy the current DSM buffer.
    H5FDdsmInt32 Destroy();

    // Description:
    // Clear the DSM storage.
    H5FDdsmInt32 ClearStorage();

    // Description:
    // Connect to a remote DSM manager (called by client).
    H5FDdsmInt32 Connect(H5FDdsmBoolean persist = H5FD_DSM_FALSE);

    // Description:
    // Disconnect the manager from the remote end (called by client and server).
    H5FDdsmInt32 Disconnect();

    // Description:
    // Make the DSM manager listen for new incoming connection (called by server).
    H5FDdsmInt32 Publish();

    // Description:
    // Stop the listening service (called by server).
    H5FDdsmInt32 Unpublish();

    // Description:
    // Open the DSM from all nodes of the parallel application, 
    // all nodes within a communicator must participate
    // this function must be paired with a matching Close
    // Use H5F_ACC_RDONLY for queries
    // Use H5F_ACC_RDWR   for read/write
    H5FDdsmInt32 OpenDSM(H5FDdsmUInt32 mode, bool serial=false);
    H5FDdsmInt32 OpenDSMSerial(H5FDdsmUInt32 mode);

    // Description:
    // Close the DSM from all nodes of the parallel application, 
    // all nodes within a communicator must participate
    // this function must be paired with a matching Open
    H5FDdsmInt32 CloseDSM();

    // Description:
    // Returns 1 if OpenDSM has been called and the fapl and file handles
    // have been cached. These are released when CloseDSM is called
    // all nodes within a communicator must participate. Returns 0 otherwise.
    H5FDdsmBoolean IsOpenDSM();

    // Description:
    // If the DSM has been opened and handles cached, this returns the cached
    // handle, otherwise H5I_BADID (-1). Use with great care.
    hid_t GetCachedFileHandle();
    hid_t GetCachedFileAccessHandle();

    // Description:
    // If the .dsm_config file exists in the standard location
    // $ENV{H5FD_DSM_CONFIG_PATH}/.dsm_config then the server/port/mode
    // information can be read. This is for use the by a DSM client.
    // DSM servers write their .dsm_config when Publish() is called
    // Returns false if the .dsm_config file is not read.
    H5FDdsmInt32 ReadConfigFile();

    static MPI_Comm GetGlobalMPICommunicator();

#ifdef H5FDdsm_HAVE_STEERING
    inline H5FDdsmSteerer *GetSteerer() { return(Steerer); }

    H5FDdsmInt32 WriteSteeredData();
    H5FDdsmInt32 UpdateSteeredObjects();

    // Description:
    // Set/Get the current given steering command.
    // The command is then passed to the simulation.
    void SetSteeringCommand(H5FDdsmConstString cmd);

    // Description:
    // Set values and associated name for the corresponding int interaction.
    void SetSteeringValues(const char *name, int numberOfElements, int *values);
    H5FDdsmInt32 GetSteeringValues(const char *name, int numberOfElements, int *values);

    // Description:
    // Set values and associated name for the corresponding int interaction.
    void SetSteeringValues(const char *name, int numberOfElements, double *values);
    H5FDdsmInt32 GetSteeringValues(const char *name, int numberOfElements, double *values);

    // Description:
    // Return true if the Interactions group exists, false otherwise.
    H5FDdsmInt32 GetInteractionsGroupPresent();

    // Description:
    // Set/Unset objects.
    void SetDisabledObject(H5FDdsmConstString objectName);
#endif

    // Description:
    // A convenience function used only for debugging, returns a string
    // from an open mode flag.
    static const char *OpenModeString(H5FDdsmUInt32 mode);

  protected:
    //
    // Internal Variables
    //
    H5FDdsmInt32    UpdatePiece;
    H5FDdsmInt32    UpdateNumPieces;
    H5FDdsmUInt32   LocalBufferSizeMBytes;
    //
    static MPI_Comm MpiComm;
    //
    H5FDdsmBufferService *DsmBuffer;
    H5FDdsmComm          *DsmComm;
    //
    H5FDdsmBoolean  IsAutoAllocated;
    H5FDdsmBoolean  IsServer;
    H5FDdsmBoolean  IsStandAlone;
    H5FDdsmBoolean  IsDriverSerial;
    H5FDdsmInt32    DsmType;
    H5FDdsmUInt64   BlockLength;
    H5FDdsmInt32    InterCommType;
    H5FDdsmBoolean  UseStaticInterComm;
    H5FDdsmString   ServerHostName;
    H5FDdsmInt32    ServerPort;
    //
    std::stack<H5FDdsmUInt32> Cache_Stack;      
    hid_t                     Cache_fapl;
    hid_t                     Cache_fileId;
    //
#ifdef H5FDdsm_HAVE_STEERING
    H5FDdsmSteerer *Steerer;
#endif
    //
    H5FDdsmManagerInternals *ManagerInternals;
    //
    friend class H5FDdsmSteerer;
    friend class XdmfHDF;
    //
private:
    H5FDdsmManager(const H5FDdsmManager&);  // Not implemented.
    void operator=(const H5FDdsmManager&);  // Not implemented.
};

#endif // __H5FDdsmManager_h
