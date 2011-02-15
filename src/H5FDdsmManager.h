/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmManager.h

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing 
  1) This copyright notice appears on all copies of source code 
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it 
  must not be reformatted such that the indentation, bracketing or 
  overall style is modified significantly. 

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=========================================================================*/
// .NAME H5FDdsmManager - Create/Expose a DSM buffer to an external application
// .SECTION Description
// Create/Expose a DSM buffer to an external application

#ifndef __H5FDdsmManager_h
#define __H5FDdsmManager_h

#include "H5FDdsmBuffer.h"
#include "H5FDdsmCommSocket.h"
#include "H5FDdsmCommMpi.h"

#ifdef _WIN32
  #include <windows.h>
#else
  extern "C" {
    #include <pthread.h>
  }
#endif

struct H5FDdsmManagerInternals;

class H5FDdsm_EXPORT H5FDdsmManager : public H5FDdsmObject
{
  public:
     H5FDdsmManager();
    ~H5FDdsmManager();

    // Description:
    // Set/Get the MPI Communicator used by this DSM manager
    void SetCommunicator(MPI_Comm comm);
    H5FDdsmGetValueMacro(Communicator,MPI_Comm);

    // Description:
    // Set/Get the size of the buffer to be reserved on this process
    // the DSM total size will be the sum of the local sizes from all processes
    H5FDdsmSetValueMacro(LocalBufferSizeMBytes,H5FDdsmInt64);
    H5FDdsmGetValueMacro(LocalBufferSizeMBytes,H5FDdsmInt64);

    // Description:
    // Set/Get DsmIsServer info
    H5FDdsmSetValueMacro(DsmIsServer, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(DsmIsServer, H5FDdsmBoolean);

    // Description:
    // Set/Get the interprocess communication subsystem
    // Valid values are H5FD_DSM_COMM_MPI, H5FD_DSM_COMM_SOCKET
    H5FDdsmSetValueMacro(DsmCommType, H5FDdsmInt32);
    H5FDdsmGetValueMacro(DsmCommType, H5FDdsmInt32);

    // Description:
    // Set/Get the published host name of our connection.
    // Real value valid after a PublishDSM call has been made.
    H5FDdsmSetStringMacro(ServerHostName);
    H5FDdsmGetStringMacro(ServerHostName);

    // Description:
    // Set/Get the published port of our connection.
    // Real value valid after a PublishDSM call has been made.
    H5FDdsmSetValueMacro(ServerPort, H5FDdsmInt32);
    H5FDdsmGetValueMacro(ServerPort, H5FDdsmInt32);

    // Description:
    // Set/Get the DSM configuration file path.
    // If set correctly before calling PublishDSM, communicator parameters will be
    // saved into the given configuration file.
    H5FDdsmSetStringMacro(DsmConfigFilePath);
    H5FDdsmGetStringMacro(DsmConfigFilePath);

    // Description:
    // Only valid after a AcceptConnection call has been made.
    H5FDdsmInt32 GetAcceptedConnection();

    // Description:
    // Get/Set the update ready flag which triggers the pipeline update.
    H5FDdsmInt32  GetDsmUpdateReady();
    void ClearDsmUpdateReady();
    H5FDdsmInt32 WaitForUpdateReady();

    // Description:
    // Get/Set the data modified flag
    H5FDdsmInt32  GetDsmIsDataModified();
    void ClearDsmIsDataModified();

    // Description:
    // Get/Set the update level flag
    H5FDdsmInt32  GetDsmUpdateLevel();
    void ClearDsmUpdateLevel();

    // Description:
    // Release locks and clean up for next update
    void UpdateFinalize();

    // Description:
    // Set/Get the current given steering command.
    // The command is then passed to the simulation.
    void SetSteeringCommand(H5FDdsmString cmd);

    // Description:
    // Set values and associated name for the corresponding int interaction.
    void SetSteeringValues(const char *name, int numberOfElements, int *values);
    H5FDdsmInt32 GetSteeringValues(const char *name, int numberOfElements, int *values);

    // Description:
    // Set values and associated name for the corresponding int interaction.
    void SetSteeringValues(const char *name, int numberOfElements, double *values);
    H5FDdsmInt32 GetSteeringValues(const char *name, int numberOfElements, double *values);

    // Description:
    // Return true if the Interactions group exists, false otherwise
    H5FDdsmInt32 GetInteractionsGroupPresent();

    // Description:
    // Set/Unset objects
    void SetDisabledObject(H5FDdsmString objectName);

    // Description:
    // When sending, the writer can SetXMLDescriptionSend and it will be transmitted
    // to the receiver. When receiving, GetXMLDescriptionReceive queries the internal DSMBuffer
    // object to see if a string is present
    H5FDdsmSetStringMacro(XMLStringSend);
    H5FDdsmConstString GetXMLStringReceive();
    void   ClearXMLStringReceive();

    H5FDdsmInt32 CreateDSM();
    H5FDdsmInt32 DestroyDSM();
    void   ClearDSM();
    void   ConnectDSM();
    void   DisconnectDSM();
    void   PublishDSM();
    void   UnpublishDSM();
    void   H5Dump();
    void   H5DumpLight();
    void   H5DumpXML();
    void   SendDSMXML();
    void   WriteSteeredData();
    void   UpdateSteeredObjects();

    // Description:
    // If the .dsm_config file exists in the standard location
    // $ENV{H5FD_DSM_CONFIG_PATH}/.dsm_config then the server/port/mode
    // information can be read. This is for use the by a DSM client.
    // DSM servers write their .dsm_config when PublishDSM() is called
    // Returns false if the .dsm_config file is not read
    H5FDdsmInt32   ReadDSMConfigFile();

    H5FDdsmBuffer *GetDSMHandle();

  protected:
    //
    // Internal Variables
    //
    H5FDdsmInt32   UpdatePiece;
    H5FDdsmInt32   UpdateNumPieces;
    H5FDdsmInt64   LocalBufferSizeMBytes;

#ifdef _WIN32
    DWORD          ServiceThread;
    HANDLE         ServiceThreadHandle;
#else
    pthread_t      ServiceThread;
#endif

    MPI_Comm        Communicator;
    H5FDdsmBuffer  *DSMBuffer;
    H5FDdsmComm    *DSMComm;
    //
    H5FDdsmBoolean  DsmIsServer;
    H5FDdsmInt32    DsmCommType;
    H5FDdsmString   ServerHostName;
    H5FDdsmInt32    ServerPort;
    H5FDdsmString   DsmConfigFilePath;
    //
    H5FDdsmString   XMLStringSend;
    //
    H5FDdsmManagerInternals *ManagerInternals;


private:
    H5FDdsmManager(const H5FDdsmManager&);  // Not implemented.
    void operator=(const H5FDdsmManager&);  // Not implemented.
};

#endif // __H5FDdsmManager_h
