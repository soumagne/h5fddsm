/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmIniFile.h

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

  Ini file Code taken from the following with thanks
  CIniFile Class for C++ - A robust cross platform INI file class
  By Todd Davis (http://www.codeproject.com/file/CIniFile.asp)

  Some fixes, tweaks and reformatting : CSCS

=========================================================================*/
#include "H5FDdsmIniFile.h"

//---------------------------------------------------------------------------
H5FDdsmIniFile::H5FDdsmIniFile(void) {}

//---------------------------------------------------------------------------
H5FDdsmIniFile::~H5FDdsmIniFile(void) {}

//---------------------------------------------------------------------------
// A function to trim whitespace from both sides of a given string
void Trim(std::string& str, const std::string & ChrsToTrim = " \t\n\r", int TrimDir = 0)
{
    size_t startIndex = str.find_first_not_of(ChrsToTrim);
    if (startIndex == std::string::npos) {str.erase(); return;}
    if (TrimDir < 2) str = str.substr(startIndex, str.size()-startIndex);
    if (TrimDir!=1) str = str.substr(0, str.find_last_not_of(ChrsToTrim) + 1);
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::Load(string FileName, vector<Record>& content)
{
  string s;                                // Holds the current line from the ini file
  string CurrentSection;                   // Holds the current section name

  ifstream inFile (FileName.c_str());      // Create an input filestream
  if (!inFile.is_open()) return false;     // If the input file doesn't open, then return
  content.clear();                         // Clear the content vector

  string comments = "";                    // A string to store comments in

  while(!std::getline(inFile, s).eof())    // Read until the end of the file
  {
    Trim(s);                               // Trim whitespace from the ends
    if (!s.empty())                         // Make sure its not a blank line
    {
      Record r;                            // Define a new record

      if ((s[0]=='#')||(s[0]==';'))         // Is this a commented line?
      {
        if ((s.find('[')==string::npos)&&  // If there is no [ or =
          (s.find('=')==string::npos))     // Then it's a comment
        {
          comments += s + '\n';            // Add the comment to the current comments string
        } else {
          r.Commented = s[0];              // Save the comment character
          s.erase(s.begin());              // Remove the comment for further processing
          Trim(s);
        }                                  // Remove any more whitespace
      } else r.Commented = ' ';            // else mark it as not being a comment

      if (s.find('[')!=string::npos)        // Is this line a section?
      {    
        s.erase(s.begin());                // Erase the leading bracket
        s.erase(s.find(']'));              // Erase the trailing bracket
        r.Comments = comments;             // Add the comments string (if any)
        comments = "";                     // Clear the comments for re-use
        r.Section = s;                     // Set the Section value
        r.Key = "";                        // Set the Key value
        r.Value = "";                      // Set the Value value
        CurrentSection = s;
      }

      if (s.find('=')!=string::npos)        // Is this line a Key/Value?
      {
        r.Comments = comments;             // Add the comments string (if any)
        comments = "";                     // Clear the comments for re-use
        r.Section = CurrentSection;        // Set the section to the current Section
        r.Key = s.substr(0,s.find('='));   // Set the Key value to everything before the = sign
        r.Value = s.substr(s.find('=')+1); // Set the Value to everything after the = sign
      }
      if (comments == "")                   // Don't add a record yet if its a comment line
        content.push_back(r);              // Add the record to content
    }
  }
  
  inFile.close();                          // Close the file
  return true;
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::Save(string FileName, vector<Record>& content)
{
  ofstream outFile (FileName.c_str());     // Create an output filestream
  if (!outFile.is_open()) return false;    // If the output file doesn't open, then return

  for (int i=0;i<(int)content.size();i++)  // Loop through each vector
  {
    outFile << content[i].Comments;        // Write out the comments
    if (content[i].Key == "")               // Is this a section?
      outFile << content[i].Commented << "[" 
      << content[i].Section << "]" << endl;// Then format the section
    else
      outFile << content[i].Commented << content[i].Key  
      << "=" << content[i].Value << endl;  // Else format a key/value
  }

  outFile.close();                         // Close the file
  return true;
}

bool H5FDdsmIniFile::RecordExists(string KeyName, string SectionName, string FileName)
{
  vector<Record> content;                       // Holds the current record

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));     // Locate the Section/Key

    if (iter == content.end()) return false;                    // The Section/Key was not found
  }
  return true;                                                  // The Section/Key was found
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::SectionExists(string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionIs(SectionName));          // Locate the Section

    if (iter == content.end()) return false;              // The Section was not found
  }
  else
  {
    return false;
  }
  return true;                                  // The Section was found
}

//---------------------------------------------------------------------------
vector<H5FDdsmIniFile::Record> H5FDdsmIniFile::GetRecord(string KeyName, string SectionName, string FileName)
{
  vector<Record> data;                          // Holds the return data
  vector<Record> content;                       // Holds the current record

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));     // Locate the Record

    if (iter == content.end()) return data;                     // The Record was not found

    data.push_back (*iter);                                     // The Record was found
  }
  return data;                                                  // Return the Record
}

//---------------------------------------------------------------------------
string H5FDdsmIniFile::GetValue(string KeyName, string SectionName, string FileName)
{
  vector<Record> content = GetRecord(KeyName,SectionName, FileName);    // Get the Record

  if (!content.empty())                              // Make sure there is a value to return
    return content[0].Value;                        // And return the value

  return "";                                        // No value was found
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::SetValue(string KeyName, string Value, string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    if (!SectionExists(SectionName,FileName))              // If the Section doesn't exist
    {
      Record s = {"",' ',SectionName,"",""};              // Define a new section
      Record r = {"",' ',SectionName,KeyName,Value};      // Define a new record
      content.push_back(s);                               // Add the section
      content.push_back(r);                               // Add the record
      return Save(FileName,content);                      // Save
    }

    if (!RecordExists(KeyName,SectionName,FileName))       // If the Key doesn't exist
    {
      vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionIs(SectionName));          // Locate the Section
      iter++;                                             // Advance just past the section
      Record r = {"",' ',SectionName,KeyName,Value};      // Define a new record
      content.insert(iter,r);                             // Add the record
      return Save(FileName,content);                      // Save
    }

    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName)); // Locate the Record

    iter->Value = Value;                                    // Insert the correct value
    return Save(FileName,content);                          // Save
  }

  return false;                                             // In the event the file does not load
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::AddSection(string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    Record s = {"",' ',SectionName,"",""};          // Define a new section
    content.push_back(s);                           // Add the section
    return Save(FileName,content);                  // Save
  }

  return false;                                     // The file did not open
}

//---------------------------------------------------------------------------
bool H5FDdsmIniFile::Create(string FileName)
{
  vector<Record> content;                          // Create empty content
  return Save(FileName,content);                   // Save
}

