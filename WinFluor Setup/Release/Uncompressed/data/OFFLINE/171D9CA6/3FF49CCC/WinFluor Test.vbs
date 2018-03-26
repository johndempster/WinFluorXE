Rem Script created: 01.09.2005 22:28
Rem By: cecs06
Rem Script language: VBScript
Rem Original Workspace: Default

Set WINFLUOR = CreateObject("WINFLUOR.AUTO")

WinFluor.NewFile "d:\Test File.idr"
WinFluor.ExperimentID = "test ident"
MsgBox WinFluor.FileName
WinFluor.RecordLSM 6.0, 3000, 0.0, 1
MsgBox "Recording started"
WinFluor.ImportImageFile ""


Set WinFLuor = None
