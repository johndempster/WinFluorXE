object Ultima: TUltima
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 362
  Width = 510
  object TCPClient: TTcpClient
    BlockMode = bmNonBlocking
    RemoteHost = '130.159.128.76'
    RemotePort = '1234'
    Left = 32
    Top = 16
  end
  object Timer: TTimer
    Interval = 55
    OnTimer = TimerTimer
    Left = 80
    Top = 16
  end
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 120
    Top = 16
  end
  object XMLDOC: TXMLDocument
    Options = [doNodeAutoCreate, doAutoPrefix, doNamespaceDecl]
    ParseOptions = [poValidateOnParse]
    Left = 176
    Top = 16
    DOMVendorDesc = 'MSXML'
  end
  object XMLState: TXMLDocument
    Options = [doNodeAutoCreate, doAutoPrefix, doNamespaceDecl]
    ParseOptions = [poValidateOnParse]
    Left = 256
    Top = 16
    DOMVendorDesc = 'MSXML'
  end
  object XMLSingleImage: TXMLDocument
    Options = [doNodeAutoCreate, doAutoPrefix, doNamespaceDecl]
    ParseOptions = [poValidateOnParse]
    Left = 328
    Top = 16
    DOMVendorDesc = 'MSXML'
  end
end
