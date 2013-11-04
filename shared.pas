unit Shared;
{ =======================================================================
  Library of shared procedures and functions V1.0 7/1/95                                                       
  (c) J. Dempster, University of Strathclyde 1996-99. All Rights Reserved
  =======================================================================
  21/7/99 CopyStringGrid updated
  28/10/99 ... Now handles both comma and period as decimal separator
  31/7/12 .... String replaced with ANSIString, char with ANSIchar
  29/1/13 .... ReadDouble() and AppendDouble() added }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Spin, Grids, Printers, ClipBrd,
  maths, strutils ;


  function ExtractFloat (
           CBuf : ANSIstring ;
           Default : Single
           ) : extended ;
  function ExtractListOfFloats (
           const CBuf : ANSIstring ;
           var Values : Array of Single ;
           PositiveOnly : Boolean
           ) : Integer ;
  function ExtractInt (
           CBuf : ANSIstring
           ) : LongInt ;
  function VerifyInt(
           text : ANSIstring ;
           LoLimit,HiLimit : LongInt
           ) : ANSIstring ;
  procedure AppendFloat(
            var Dest : array of ANSIChar;
            Keyword : ANSIstring ;
            Value : Extended
            ) ;
  procedure AppendDouble(
            var Dest : array of ANSIChar;
            Keyword : ANSIstring ;
            Value : Double
            ) ;

  procedure ReadFloat(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : Single ) ;
  procedure ReadDouble(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : Double ) ;

  procedure AppendInt(
            var Dest : array of ANSIChar;
            Keyword : ANSIstring ;
            Value : LongInt
            ) ;
  procedure ReadInt(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : LongInt
            ) ;
  procedure AppendLogical(
            var Dest : array of ANSIChar;
            Keyword : ANSIstring ;
            Value : Boolean ) ;
  procedure ReadLogical(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : Boolean
            ) ;
  procedure AppendString(
            var Dest : Array of ANSIChar;
            Keyword,
            Value : ANSIstring
            ) ;
  procedure ReadString(
            const Source : Array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : string
            ) ;

  procedure CopyStringToArray(
            var Dest : array of ANSIChar ;
            Source : ANSIstring
            ) ;
  procedure CopyArrayToString(
            var Dest : ANSIstring ;
            var Source : array of ANSIChar
            ) ;
  function ArrayToString(
         const CharArray : Array of ANSIChar
         ) : ANSIstring ;
  procedure FindParameter(
            const Source : array of ANSIChar ;
            Keyword : ANSIstring ;
            var Parameter : ANSIstring
            ) ;
  Function GetFromEditBox(
           var ed : TEdit ;
           Default, Min, Max : Single ;
           const FormatString, Units : ANSIstring ;
           Scale : single
           ) : Single ;
  procedure GetIntRangeFromEditBox(
            var ed : TEdit ;
            var Lo,Hi : LongInt ;
            Min,Max : LongInt
            ) ;
  Procedure GetRangeFromEditBox(
            const ed : TEdit ;
            var LoValue,HiValue : Single ;
            Min,Max : Single ;
            const FormatString : ANSIstring ;
            const Units : ANSIstring
            ) ;
  function Contains(
           const Target,
           Buf : ANSIstring
           ) : boolean ;

  function ExtractFileNameOnly(
           FilePath : string
           ) : string ;



  const
     MaxSingle = 1E38 ;

implementation




function ExtractFloat (
         CBuf : ANSIstring ;     { ASCII text to be processed }
         Default : Single    { Default value if text is not valid }
         ) : extended ;
{ -------------------------------------------------------------------
  Extract a floating point number from a string which
  may contain additional non-numeric text
  28/10/99 ... Now handles both comma and period as decimal separator
  -------------------------------------------------------------------}

var
   CNum,dsep : ANSIstring ;
   i : integer ;
   Done,NumberFound : Boolean ;
begin
     { Extract number from othr text which may be around it }
     CNum := '' ;
     Done := False ;
     NumberFound := False ;
     i := 1 ;
     repeat

         if CBuf[i] in ['0'..'9', 'E', 'e', '+', '-', '.', ',' ] then begin
            CNum := CNum + CBuf[i] ;
            NumberFound := True ;
            end
         else if NumberFound then Done := True ;
         Inc(i) ;
         if i > Length(CBuf) then Done := True ;
         until Done ;

     { Correct for use of comma/period as decimal separator }
     {$IF CompilerVersion > 7.0} dsep := formatsettings.DECIMALSEPARATOR ;
     {$ELSE} dsep := DECIMALSEPARATOR ;
     {$IFEND}
     if dsep = '.' then CNum := ANSIReplaceText(CNum ,',',dsep);
     if dsep = ',' then CNum := ANSIReplaceText(CNum, '.',dsep);

     { Convert number from ASCII to real }
     try
        if Length(CNum)>0 then Result := StrToFloat( String(CNum) )
                          else Result := Default ;
     except
        on E : EConvertError do Result := Default ;
        end ;
end ;

function ExtractInt ( CBuf : ANSIstring ) : longint ;
{ ---------------------------------------------------
  Extract a 32 bit integer number from a string which
  may contain additional non-numeric text
  ---------------------------------------------------}

Type
    TState = (RemoveLeadingWhiteSpace, ReadNumber) ;
var CNum : ANSIstring ;
    i : integer ;
    Quit : Boolean ;
    State : TState ;

begin
     CNum := '' ;
     i := 1;
     Quit := False ;
     State := RemoveLeadingWhiteSpace ;
     while not Quit do begin

           case State of

                { Ignore all non-numeric characters before number }
                RemoveLeadingWhiteSpace : begin
                   if CBuf[i] in ['0'..'9','+','-'] then State := ReadNumber
                                                    else i := i + 1 ;
                   end ;

                { Copy number into string CNum }
                ReadNumber : begin
                    {End copying when a non-numeric character
                    or the end of the string is encountered }
                    if CBuf[i] in ['0'..'9','E','e','+','-','.'] then begin
                       CNum := CNum + CBuf[i] ;
                       i := i + 1 ;
                       end
                    else Quit := True ;
                    end ;
                else end ;

           if i > Length(CBuf) then Quit := True ;
           end ;
     try


        ExtractInt := StrToInt( CNum ) ;
     except
        ExtractInt := 1 ;
        end ;
     end ;


function VerifyInt( text : ANSIstring ; LoLimit,HiLimit : LongInt ) : ANSIstring ;
{ -------------------------------------------------------------
  Ensure an ASCII edit field contains a value within set limits
  -------------------------------------------------------------}
var
   Value : LongInt ;
begin
     Value := ExtractInt( text ) ;
     if Value < LoLimit then Value := LoLimit ;
     If Value > HiLimit then Value := HiLimit ;
     VerifyInt := IntToStr( Value ) ;
     end ;


function ExtractListOfFloats ( const CBuf : ANSIstring ;
                                var Values : Array of Single ;
                                PositiveOnly : Boolean ) : Integer ;
{ -------------------------------------------------------------
  Extract a series of floating point number from a string which
  may contain additional non-numeric text
  ---------------------------------------}

var
   CNum : ANSIstring ;
   i,nValues : integer ;
   EndOfNumber : Boolean ;
begin
     nValues := 0 ;
     CNum := '' ;
     for i := 1 to length(CBuf) do begin

         { If character is numeric ... add it to number string }
         if PositiveOnly then begin
            { Minus sign is treated as a number separator }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end
         else begin
            { Positive or negative numbers }
            if CBuf[i] in ['0'..'9', 'E', 'e', '.', '-' ] then begin
               CNum := CNum + CBuf[i] ;
               EndOfNumber := False ;
               end
            else EndOfNumber := True ;
            end ;

         { If all characters are finished ... check number }
         if i = length(CBuf) then EndOfNumber := True ;

         if (EndOfNumber) and (Length(CNum) > 0)
            and (nValues <= High(Values)) then begin
              try
                 Values[nValues] := StrToFloat( CNum ) ;
                 CNum := '' ;
                 Inc(nValues) ;
              except
                    on E : EConvertError do CNum := '' ;
                    end ;
              end ;
         end ;
     { Return number of values extracted }
     Result := nValues ;
     end ;


procedure AppendFloat( var Dest : Array of ANSIChar; Keyword : ANSIstring ; Value : Extended ) ;
{ --------------------------------------------------------
  Append a floating point parameter line
  'Keyword' = 'Value' on to end of the header text array
  --------------------------------------------------------}
begin
     CopyStringToArray( Dest, Keyword ) ;
     CopyStringToArray( Dest, format( '%.6g',[Value] ) ) ;
     CopyStringToArray( Dest, #13 + #10 ) ;
     end ;


procedure ReadFloat( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : Single ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractFloat( Parameter, 1. ) ;
     end ;

procedure AppendDouble( var Dest : Array of ANSIChar; Keyword : ANSIstring ; Value : Double ) ;
{ --------------------------------------------------------
  Append a DP floating point parameter line
  'Keyword' = 'Value' on to end of the header text array
  --------------------------------------------------------}
begin
     CopyStringToArray( Dest, Keyword ) ;
     CopyStringToArray( Dest, format( '%.6g',[Value] ) ) ;
     CopyStringToArray( Dest, #13 + #10 ) ;
     end ;

procedure ReadDouble( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : Double ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractFloat( Parameter, 1. ) ;
     end ;


procedure AppendInt( var Dest : Array of ANSIChar; Keyword : ANSIstring ; Value : LongInt ) ;
{ -------------------------------------------------------
  Append a long integer point parameter line
  'Keyword' = 'Value' on to end of the header text array
  ------------------------------------------------------ }
begin
     CopyStringToArray( Dest, Keyword ) ;
     CopyStringToArray( Dest, InttoStr( Value ) ) ;
     CopyStringToArray( Dest, #13 + #10 ) ;
     end ;


procedure ReadInt( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : LongInt ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractInt( Parameter ) ;
     end ;

{ Append a text string parameter line
  'Keyword' = 'Value' on to end of the header text array}

procedure AppendString( var Dest : Array of ANSIChar; Keyword, Value : ANSIstring ) ;
begin
CopyStringToArray( Dest, Keyword ) ;
CopyStringToArray( Dest, Value ) ;
CopyStringToArray( Dest, #13 + #10 ) ;
end ;

procedure ReadString(
          const Source : Array of ANSIChar;
          Keyword : ANSIstring ;
          var Value : string ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := String(Parameter)  ;
     end ;

{ Append a boolean True/False parameter line
  'Keyword' = 'Value' on to end of the header text array}

procedure AppendLogical( var Dest : Array of ANSIChar; Keyword : ANSIstring ; Value : Boolean ) ;
begin
     CopyStringToArray( Dest, Keyword ) ;
     if Value = True then CopyStringToArray( Dest, 'T' )
                     else CopyStringToArray( Dest, 'F' )  ;
     CopyStringToArray( Dest, #13 + #10 ) ;
     end ;

procedure ReadLogical( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : Boolean ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if pos('T',Parameter) > 0 then Value := True
                               else Value := False ;
     end ;

{ Copy a string variable to character array
  NOTE. array MUST have been filled with 0 characters before
        using the function }

procedure CopyStringToArray( var Dest : array of ANSIChar ; Source : ANSIstring ) ;
var
   i,j : Integer ;
begin

     { Find end of character array }
     j := 0 ;
     while (Dest[j] <> chr(0)) and (j < High(Dest) ) do j := j + 1 ;

     if (j + length(Source)) < High(Dest) then
     begin
          for i := 1 to length(Source) do
          begin
               Dest[j] := Source[i] ;
               j := j + 1 ;
               end ;
          end
     else
         ShowMessage( ' Array Full ' ) ;

     end ;

procedure CopyArrayToString(
          var Dest : ANSIstring ;
          var Source : array of ANSIChar ) ;
var
   i : Integer ;
begin
     Dest := '' ;
     for i := 0 to High(Source) do begin
         Dest := Dest + Source[i] ;
         end ;
     end ;

function ArrayToString(
         const CharArray : Array of ANSIChar
         ) : ANSIstring ;
var
   i : Integer ;
   s : ANSIstring ;
begin
     s := '' ;
     for i := 0 to High(CharArray) do begin
         s := s + CharArray[i] ;
         end ;
     Result := s ;
     end ;

procedure FindParameter( const Source : array of ANSIChar ;
                               Keyword : ANSIstring ;
                               var Parameter : ANSIstring ) ;
var
s,k : integer ;
Found : boolean ;
begin

     { Search for the string 'keyword' within the
       array 'Source' }

     s := 0 ;
     k := 1 ;
     Found := False ;
     while (not Found) and (s < High(Source)) do
     begin
          if Source[s] = Keyword[k] then
          begin
               k := k + 1 ;
               if k > length(Keyword) then Found := True
               end
               else k := 1;
         s := s + 1;
         end ;

    { Copy parameter value into string 'Parameter'
      to be returned to calling routine }

    Parameter := '' ;
    if Found then
    begin
        while (Source[s] <> #13) and (s < High(Source)) do
        begin
             Parameter := Parameter + Source[s] ;
             s := s + 1
             end ;
        end ;
    end ;


Function GetFromEditBox( var ed : TEdit ;
                         Default, Min, Max : Single ;
                         const FormatString,Units : ANSIstring ;
                         Scale : single ) : Single ;
{ --------------------------------------------------------------------
  Get a number from an edit box, ensure that it is within valid limits,
  and update the box with the value used.
  ed ... Edit box to get text from
  Default ... value to use if box does not contain valid data
  Min ... Minimum valid value
  Max ... Maximum valid value
  FormatString ... format used to update box
  Units ... units of value
  Scale ... Factor for scaling display units
  --------------------------------------------------------------------}
var
   Value : single ;
begin
     Value := ExtractFloat( ed.text, Default*Scale ) / Scale ;
     if Value < Min then Value := Abs(Value) ;
     if Value < Min then Value := Min ;
     if Value > Max then Value := Max ;
     ed.text := format( FormatString, [Value*Scale] ) + ' ' + Units ;
     Result := Value ;
     end ;


procedure GetIntRangeFromEditBox( var ed : TEdit ; var Lo,Hi : LongInt ;
                                  Min,Max : LongInt ) ;
var
   LoValue,HiValue : single ;
begin
     {if ed.text = '' then ed.text := format( ' %d-%d', [Lo,Hi]) ;}
     GetRangeFromEditBox( ed, LoValue,HiValue, Min, Max,'%.0f-%.0f','' ) ;
     Lo := Trunc( LoValue ) ;
     Hi := Trunc( HiValue ) ;
     end ;


procedure GetRangeFromEditBox( const ed : TEdit ;
                               var LoValue,HiValue : Single ;
                               Min,Max : Single ;
                               const FormatString : ANSIstring ;
                               const Units : ANSIstring ) ;
var
   Values : Array[0..10] of Single ;
   Temp : Single ;
   nValues : Integer ;
begin
     LoValue := Min ;
     HiValue := Max ;
     nValues := ExtractListofFloats( ed.text, Values, True ) ;
     if nValues >=1 then LoValue := Values[0] ;
     if nValues >=2 then HiValue := Values[1] ;
     if LoValue > HiValue then begin
        Temp := LoValue ;
        LoValue := HiValue ;
        HiValue := Temp ;
        end ;
     ed.text := format( FormatString, [LoValue,HiValue] ) + ' ' + Units ;
     end ;


function Contains( const Target,Buf : ANSIstring ) : boolean ;
{ Determine whether the sub-string in 'Target' is contained in 'Buf'
  ... return True if it is. }
begin
     if Pos( UpperCase(Target), UpperCase(Buf) ) > 0 then Contains := True
                                                     else Contains := False ;
     end ;


function ExtractFileNameOnly( FilePath : string ) : string ;
{ -----------------------------------------------------
  Extract file name (without extension) from file path
  ----------------------------------------------------}
var
   FileName : string ;
   FileExt : string[6] ;
begin
     FileName := ExtractFileName(FilePath) ;
     FileExt := ExtractFileExt(FileName) ;
     Delete( FileName,Pos(FileExt,FileName),Length(FileExt) ) ;
     ExtractFileNameOnly := FileName ;
     end ;



end.

