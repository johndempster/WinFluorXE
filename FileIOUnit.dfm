object FileIO: TFileIO
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 480
  Width = 696
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 32
    Top = 32
  end
  object SaveDialog: TSaveDialog
    OnTypeChange = SaveDialogTypeChange
    Left = 88
    Top = 40
  end
  object OpenDialog: TOpenDialog
    OnTypeChange = OpenDialogTypeChange
    Left = 152
    Top = 48
  end
end
