unit MATFileWriterUnit;

//----------------------------------------------------------------------------//
// MATFileWriterUnit - Class to write a Level 5 MAT-File
// by Nicholas Schwarz
//----------------------------------------------------------------------------//
//
// Example usage:
// fileWriter.OpenMATFile('file.mat');
// fileWriter.WriteFileHeader;
// fileWriter.WriteDoubleMatrixHeader('array', 1000);
// fileWriter.WriteDoubleMatrixValues(arr, 1000);
// fileWriter.WriteDoubleMatrixHeader('anotherArray', 1000);
// fileWriter.WriteDoubleMatrixValues(anotherArr, 1000);
// fileWriter.CloseMATFile;
//
// 12/02/09 ... Basic MAT-File writer
//              Only supports 1xN Double matrices
//
//----------------------------------------------------------------------------//

interface

//----------------------------------------------------------------------------//

uses Classes, Dialogs, Math, SysUtils;

//----------------------------------------------------------------------------//

type

  TMATFileWriter = class

    private

      MATFileStream: TFileStream;

      constructor Create();

      procedure WriteArrayFlags;

      procedure WriteArrayName(name: ANSIString);

      procedure WriteDimensionsArray(columns: Integer; rows: Integer);

    protected

    public

      procedure CloseMATFile;

      procedure OpenMATFile(FileName: String);

      procedure WriteFileHeader;

      procedure WriteDoubleMatrixHeader( name: ANSIString;
                                         nRows : Integer;
                                         nCols : Integer);

      procedure WriteDoubleMatrixValues( var arr: Array of Double;
                                         nRows : Integer;
                                         nCols : Integer);

  end;

//----------------------------------------------------------------------------//

implementation

//----------------------------------------------------------------------------//

constructor TMATFileWriter.Create();
begin
end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.CloseMATFile;
begin

  MATFileStream.Free;

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.OpenMATFile(FileName: String);
begin

  MATFileStream := TFileStream.Create(FileName, fmCreate);

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteArrayFlags;
var
  flagsType: Integer;  // Flags type, miUINT32, 6
  flagsSize: Integer;  // Flags size, 8 bytes
  flags: Integer;      // Flags, only mxDOUBLE_CLASS, 6, is set
begin

  // Set type to miUINT32, 6
  flagsType := 6;

  // Write type
  MATFileStream.Write(flagsType, 4);


  // Set flag size to 8 bytes
  flagsSize := 8;

  // Write flag size
  MATFileStream.Write(flagsSize, 4);


  // Set flags to mxDOUBLE_CLASS, 6
  flags := 6;

  // Write flags
  MATFileStream.Write(flags, 4);


  // Set undefined flags to 0
  flags := 0;

  // Write undefined flags
  MATFileStream.Write(flags, 4);

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteArrayName(name: ANSIString);
var
  i: Integer;                               // loop index
  nameArray: array [1..256] of ANSIChar;        // Pointer to name array
  nameArraySize: Integer;                   // Size of name array
  nameType: Integer;                        // Array name type, miINT8, 1
  nameSize: Integer;                        // Array name size
begin

  // Set array name type
  nameType := 1;
  MATFileStream.Write(nameType, 4);

  // Set array name size
  nameSize := Length(name);
  MATFileStream.Write(nameSize, 4);

  // Size of name array that's aligned to 64-bit boundary
  nameArraySize := (Ceil(nameSize / 8) * 8) * SizeOf(ANSIChar);

  // Clear array
  for i := 1 to 256 do
  begin
    nameArray[i] := ' ';
  end;

  // Copy name
  for i := 1 to Length(name) do
  begin
    nameArray[i] := ANSIChar(name[i]);
  end;

  // Write name array
  MATFileStream.Write(nameArray, nameArraySize);

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteDimensionsArray(columns: Integer; rows: Integer);
var
  dimensionsType: Integer;  // Dimensions Array type, miINT32, 5
  dimensionsSize: Integer;  // Bytes in dimensions array, 8
begin

  // Set type and size
  dimensionsType := 5;
  dimensionsSize := 8;

  // Write type and size
  MATFileStream.Write(dimensionsType, 4);
  MATFileStream.Write(dimensionsSize, 4);

  // Write number of rows and number of columns
  MATFileStream.Write(rows, 4);
  MATFileStream.Write(columns, 4);

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteFileHeader;
var
  endianIndicator: SmallInt;              // Endian indicator
  endianTemp: SmallInt;                   // Temp variable for endian indicator
  subsysDataOffset: array[1..8] of Byte;  // subsys data offset - not used
  textArray: array[1..116] of ANSIChar;       // Descriptive text array
  textString: ANSIString;                     // Descriptive text string
  version: SmallInt;                      // MAT-File version
  i: Integer;                             // loop index
  l: Integer;                             // Length of descriptive text string
begin

  // Create descriptive text string
  textString := 'MATLAB 5.0 MAT-file, Platform: PCWIN, ' +
                'Created by: WinFluor, ' +
                'Created on: ' + DateToStr(Date) + ' ' + TimeToStr(Time);

  // Clear descriptive text array
  for i := 1 to 116 do
    textArray[i] := ' ';

  // Copy string to array
  l := Length(textString);
  for i := 1 to l do
    textArray[i] := textString[i];

  // Write descriptive text array to file
  MATFileStream.Write(textArray, 116);


  // Set subsys data offset to zeroes
  for i := 1 to 8 do
    subsysDataOffset[i] := 0;

  // Write subsys data offset array to file
  MATFileStream.Write(subsysDataOffset, 8);


  // Set version to 0x0100
  version := $0100;

  // Write version to file
  MATFileStream.Write(version, 2);


  // Set endian indicator to 'MI'
  // 77 is the UTF-8 code for M
  // 73 is the UTF-8 code for I
  endianIndicator := 0;
  endianTemp := 77;
  endianIndicator := endianIndicator or endianTemp;
  endianIndicator := endianIndicator shl 8;
  endianTemp := 73;
  endianIndicator := endianIndicator or endianTemp;

  // Write endian indicator
  MATFileStream.Write(endianIndicator, 2);

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteDoubleMatrixHeader(
          name: ANSIString;
          nRows : Integer;
          nCols : Integer);
var
  arrayFlagsBytes: Integer;       // Bytes in Array Flags
  arrayNameBytes: Integer;        // Bytes in ArrayName
  dataBytes: Integer;             // Bytes in data section
  dataType: Integer;              // Data type
  dimensionsArrayBytes: Integer;  // Bytes in Dimensions Array
  realType: Integer;              // Type of data, miDOUBLE, 9
  realSize: Integer;              // Bytes that actual data takes up
  totalBytes: Integer;            // Total bytes minus tage size
begin

  // Data element tag

  // Comput number of bytes in each component of data element minus tag size
  arrayFlagsBytes := 16;
  dimensionsArrayBytes := 16;
  arrayNameBytes := 8 + (Ceil(Length(name) / 8) * 8);
  dataBytes := 8 + (nRows*nCols * 8);

  // Compute total number of bytes
  totalBytes := arrayFlagsBytes +
                dimensionsArrayBytes +
                arrayNameBytes +
                dataBytes;

  // Set data type to miMATRIX, 14
  dataType := 14;

  // Write data type
  MATFileStream.Write(dataType, 4);

  // Write number of bytes
  MATFileStream.Write(totalBytes, 4);


  // Array flags
  WriteArrayFlags;


  // Write dimensions array
  WriteDimensionsArray( nCols, nRows);


  // Write array name
  WriteArrayName(name);


  // Data type and size

  // Set data type to miDOUBLE, 9
  realType := 9;
  MATFileStream.Write(realType, 4);

  // Set data size to number of bytes actual data takes up
  realSize := nRows*nCols * 8;
  MATFileStream.Write(realSize, 4);

  // Now, data should be filled in with calls to WriteDoulbeMatrixValues(...)

end;

//----------------------------------------------------------------------------//

procedure TMATFileWriter.WriteDoubleMatrixValues(
          var arr: Array of Double;
          nRows : Integer;
          nCols : Integer);
begin

  // Write double data to file
  MATFileStream.Write(arr, nRows*nCols * 8);

end;

//----------------------------------------------------------------------------//

end.

//----------------------------------------------------------------------------//

