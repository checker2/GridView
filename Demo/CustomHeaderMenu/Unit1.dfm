object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TGridView Custom Header Menu Demo'
  ClientHeight = 325
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GridView1: TGridView
    Left = 0
    Top = 0
    Width = 735
    Height = 325
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        DefaultPopup = False
        DefWidth = 250
      end
      item
        Caption = 'Date modified'
        DefWidth = 140
      end
      item
        Caption = 'Type'
        DefWidth = 120
      end
      item
        Caption = 'Size'
        DefWidth = 80
      end
      item
        Caption = 'Attributes'
        DefWidth = 100
      end>
    ColumnsFullDrag = True
    DefaultHeaderMenu = True
    DoubleBuffered = True
    GridLines = False
    Header.PopupMenu = PopupMenu1
    Images = ImageList1
    RowSelect = True
    ShowCellTips = False
    TabOrder = 0
    OnGetCellImage = GridView1GetCellImage
    OnGetCellText = GridView1GetCellText
    OnHeaderDetailsClick = GridView1HeaderDetailsClick
  end
  object PopupMenu1: TPopupMenu
    Left = 48
    Top = 56
    object SizeColumnToFit1: TMenuItem
      Caption = 'Size &Column To Fit'
      OnClick = SizeColumnToFit1Click
    end
    object SizeAllColumnsToFit1: TMenuItem
      Caption = 'Size &All Columns To Fit'
      OnClick = SizeAllColumnsToFit1Click
    end
  end
  object ImageList1: TImageList
    Left = 48
    Top = 112
    Bitmap = {
      494C010104000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000797979007979
      7900797979007979790079797900797979007979790079797900797979007979
      7900797979007979790079797900000000000000000000000000EDF8FA00BAE3
      EC007EC8DA004CB2CC0064B0C400AED1DC00E8F4F80000000000000000000000
      00000000000000000000000000000000000000000000BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB0000000000000000000000000000000000F3F8FA00DAEF
      F400BDE2EB00A4D7E300B0D6E000D5E6EC00F0F7F90000000000000000000000
      000000000000000000000000000000000000000000000000000086868600F0F0
      F200F1F1F300F1F2F300F2F2F400F3F3F400F3F4F500F4F5F600F5F5F700F6F6
      F700F7F7F800F7F8F9008686860000000000000000008ED7E7005FC3D9005DC0
      D6005EBFD60060BFD60044AFCA004BA6C10056B8D4005EC0D7005DBFD7005CBE
      D60072C7DB00B5E1EC00000000000000000000000000C1C1C100F6F6F700F7F7
      F800F7F7F800F7F7F800F8F8F800F8F8F900F8F9F900F9F9FA00F9F9FA00FAFA
      FA00FAFAFB00C1C1C100000000000000000000000000C5E9F100AEE0EB00ADDE
      E900ADDEE900AEDEE900A0D6E300A4D1DF00A9DAE800ADDEEA00ADDEEA00ACDD
      E900B6E1EB00D9EFF4000000000000000000000000000000000090909000F1F1
      F300F1F2F300F2F2F400F3F3F500F3F4F500F4F5F600F5F5F700F6F6F700F7F7
      F800F7F8F900F8F9FA009090900000000000000000006CCEE10080D4E40077CE
      E0006FC9DD0067C4DA0044AFCA0043A6C3004EB8D7004EC1DE004EC1DE004EC1
      DE0050C1DD0073C7DB00000000000000000000000000C6C6C600F7F7F800F7F7
      F800F7F7F800F8F8F900F8F8F900F8F9F900F9F9FA00F9F9FA00FAFAFA00FAFA
      FB00FAFBFB00C6C6C600000000000000000000000000B4E5EF00BEE8F000BAE5
      EE00B6E3ED00B2E0EB00A0D6E300A0D1E000A5DAEA00A5DFED00A5DFED00A5DF
      ED00A6DFED00B7E1EB00000000000000000000000000000000009D9D9D00F2F2
      F300F2F2F400F3F3F500F4F4F500F4F5F600F5F5F700F6F6F700F7F7F800F8F8
      F900F8F9FA00F9FAFA009D9D9D00000000000000000070D1E40089DBE80080D4
      E40077CEE0006FC9DD0044AFCA0045A8C4004EB8D7004EC1DE004EC1DE004EC1
      DE00D3A549005DBFD700000000000000000000000000CDCDCD00F7F7F800F7F7
      F800F8F8F900F8F8F900F8F9F900F9F9FA00F9F9FA00FAFAFA00FAFAFB00FAFB
      FB00FBFBFB00CDCDCD00000000000000000000000000B6E7F000C3ECF200BEE8
      F000BAE5EE00B6E3ED00A0D6E300A1D2E000A5DAEA00A5DFED00A5DFED00A5DF
      ED00E8D1A300ADDEEA0000000000000000000000000000000000ACACAC00F2F2
      F400F3F3F500F4F4F500F4F5F600F5F6F700F6F6F800F7F7F800F8F8F900F8F9
      FA00F9FAFA00FAFBFB00ACACAC00000000000000000075D4E70092E1EC0089DB
      E80080D4E40077CEE00045B0CA0049A9C40052BAD80052C3DF004EC1DE004EC1
      DE00DDB95D005EC0D800000000000000000000000000D4D4D400F7F7F800F8F8
      F900F8F8F900F8F9F900F9F9FA00F9F9FA00FAFAFA00FAFAFB00FAFBFB00FBFB
      FB00FBFCFC00D4D4D400000000000000000000000000B9E8F200C7EFF400C3EC
      F200BEE8F000BAE5EE00A1D6E300A3D3E000A7DBEA00A7E0EE00A5DFED00A5DF
      ED00EDDBAD00ADDEEA0000000000000000000000000000000000ADADAD00F3F3
      F500F4F4F500F4F5F600F5F6F700F6F7F800F7F7F800F8F8F900F9F9FA00F9FA
      FB00FAFBFB00FBFBFC00ADADAD00000000000000000079D8E9009BE7F00092E1
      EC0089DBE80080D4E40047B2CC004EACC60056BCD9005BC7E10056C5E00052C3
      DF00ECECEC0060C1D900000000000000000000000000D5D5D500F8F8F900F8F8
      F900F8F9F900F9F9FA00F9FAFA00FAFAFA00FAFAFB00FBFBFB00FBFBFC00FBFC
      FC00FCFCFC00D5D5D500000000000000000000000000BBEAF300CCF2F600C7EF
      F400C3ECF200BEE8F000A2D7E400A5D4E100A9DCEB00ACE2EF00A9E1EE00A7E0
      EE00F4F4F400AEDFEB0000000000000000000000000000000000B1B1B100F4F4
      F600F5F5F600F5F6F700F6F7F800F7F8F800F8F8F900F9F9FA00F9FAFB00FAFB
      FB00FBFCFC00FBFCFC00B1B1B10000000000000000007DDBEC00A3ECF4009BE7
      F00092E1EC0089DBE8004AB4CE0054AFC8005CBFDA0066CDE40060CAE3005BC7
      E100ECECEC0062C2DA00000000000000000000000000D7D7D700F8F8F900F9F9
      F900F9F9FA00F9FAFA00FAFAFA00FAFAFB00FBFBFB00FBFBFC00FBFCFC00FCFC
      FC00FCFCFC00D7D7D700000000000000000000000000BDECF400D0F4F800CCF2
      F600C7EFF400C3ECF200A3D8E500A8D6E200ACDEEB00B1E5F000AEE3F000ACE2
      EF00F4F4F400AFDFEB0000000000000000000000000000000000B3B3B300F5F5
      F600F5F6F700F6F7F800F7F8F900F8F9F900F9F9FA00F9FAFB00FAFBFB00FBFC
      FC00FBFCFD00FCFDFD00B3B3B300000000000000000081DEEE00AAF1F700A3EC
      F4009BE7F00092E1EC004EB7D0005BB2C90063C2DC0073D3E8006DD0E60066CD
      E400ECECEC0063C3DB00000000000000000000000000D8D8D800F9F9F900F9F9
      FA00F9FAFA00FAFAFB00FAFBFB00FBFBFB00FBFBFC00FBFCFC00FCFCFC00FCFC
      FD00FCFDFD00D8D8D800000000000000000000000000BFEDF500D3F7FA00D0F4
      F800CCF2F600C7EFF400A5DAE600ACD7E300B0DFEC00B8E8F200B5E6F100B1E5
      F000F4F4F400B0E0EC0000000000000000000000000000000000B5B5B500F6F6
      F700F6F7F800F7F8F900F8F9F900F9F9FA00FAFAFB00FAFBFB00FBFCFC00FCFC
      FD00FCFDFD00FDFEFE00B5B5B500000000000000000084E0F000B0F5F900AAF1
      F700A3ECF400C6F1F60052BAD20061B6CB0069C6DE0082DAEC007AD7EA0073D3
      E8006DD0E60065C5DC00000000000000000000000000D9D9D900F9F9FA00F9FA
      FA00FAFAFB00FAFBFB00FBFBFB00FBFBFC00FBFCFC00FCFCFC00FCFCFD00FCFD
      FD00FDFDFD00D9D9D900000000000000000000000000C0EEF600D6F9FB00D3F7
      FA00D0F4F800E1F7F900A7DBE700AFD9E400B3E1ED00BFEBF400BBEAF300B8E8
      F200B5E6F100B1E1EC0000000000000000000000000000000000B8B8B800F6F7
      F800F7F8F900F8F9F900F9F9FA00FAFAFB00FAFBFC00FBFCFC00FCFCFD00FCFD
      FD00FDFEFE00FDFEFE00B8B8B800000000000000000088E3F200B4F7FB00B0F5
      F900AAF1F700E8FAFC0056BDD40068B9CD0070C9E00090E2F00089DEEE0082DA
      EC006FCCE200A6DDEB00000000000000000000000000DADADA00F9FAFA00FAFA
      FB00FAFBFB00FBFBFB00FBFBFC00FBFCFC00FCFCFC00FCFCFD00FCFDFD00FDFD
      FD00FDFDFD00DADADA00000000000000000000000000C2F0F700D8FAFC00D6F9
      FB00D3F7FA00F2FBFC00A9DDE800B2DBE500B6E3EE00C6EFF600C3EDF500BFEB
      F400B6E4EF00D1ECF30000000000000000000000000000000000BABABA00F7F8
      F900F8F9FA00F9FAFA00FAFAFB00FAFBFC00FBFCFC00FCFDFD00FCFDFD00FCFD
      FD00FDFEFE00FDFEFE00B8B8B80000000000000000008AE5F300B4F7FB00B4F7
      FB00B0F5F900F4FDFE005AC0D7006EBCCE0076CCE1009EE9F30097E5F20090E2
      F0006BC9DF0000000000000000000000000000000000DBDBDB00FAFAFB00FAFB
      FB00FBFBFB00FBFBFC00FBFCFC00FCFCFC00FCFDFD00FCFDFD00FCFDFD00FDFD
      FD00FDFDFD00DADADA00000000000000000000000000C3F1F800D8FAFC00D8FA
      FC00D6F9FB00F8FDFD00ABDEEA00B5DCE500B9E4EF00CDF3F800CAF1F700C6EF
      F600B4E3EE000000000000000000000000000000000000000000BCBCBC00F8F9
      FA00F9FAFA00FAFAFB00FAFBFC00FBFCFC00FCFDFD00FCFDFD00FDFEFE00FDFE
      FE00E5E5E600DFDFE000BABABA0000000000000000008DE6F500B4F7FB00B4F7
      FB00B4F7FB00FCFFFF005FC4DA0074BED0007DCFE300ACEFF700A5ECF5009EE9
      F3006ECAE10000000000000000000000000000000000DCDCDC00FAFBFB00FBFB
      FB00FBFBFC00FBFCFC00FCFCFC00FCFDFD00FCFDFD00FDFDFD00FDFDFD00F1F1
      F100EEEEEE00DBDBDB00000000000000000000000000C5F1F900D8FAFC00D8FA
      FC00D8FAFC00FCFEFE00AEE0EB00B8DDE600BDE6F000D4F6FA00D1F4F900CDF3
      F800B5E3EF000000000000000000000000000000000000000000BDBDBD00F9FA
      FA00FAFBFB00FBFBFC00FBFCFC00FCFDFD00FCFDFD00FDFEFE00FDFEFE00BABA
      BA00B8B8B800B8B8B800BABABA0000000000000000008DE7F500B4F7FB00B4F7
      FB00B4F7FB00FFFFFF0064C8DD0078C0D10081D2E500B7F4FA00B1F2F900ACEF
      F70070CCE20000000000000000000000000000000000DDDDDD00FBFBFB00FBFC
      FC00FCFCFC00FCFCFC00FCFDFD00FCFDFD00FDFDFD00FDFDFD00DBDBDB00DADA
      DA00DADADA00DBDBDB00000000000000000000000000C5F2F900D8FAFC00D8FA
      FC00D8FAFC00FEFEFE00B0E2ED00BADEE700BFE7F100DAF8FB00D7F7FB00D4F6
      FA00B6E4EF000000000000000000000000000000000000000000BFBFBF00FAFB
      FB00FBFBFC00FBFCFC00FCFDFD00FCFDFD00FDFEFE00FDFEFE00FDFEFE00C5C5
      C500F6F6F600BBBBBB000000000000000000000000008DE7F500B4F7FB00B4F7
      FB00FCFFFF00FCFFFF0069CBE0007FC6D500AEEEF600BFF9FC00BBF7FB00B7F4
      FA0072CDE40000000000000000000000000000000000DEDEDE00FBFCFC00FCFC
      FC00FCFCFC00FCFDFD00FCFDFD00FDFDFD00FDFDFD00FDFDFD00E1E1E100F9F9
      F900DBDBDB00F6F6F600000000000000000000000000C5F2F900D8FAFC00D8FA
      FC00FCFEFE00FCFEFE00B3E4EE00BEE1E900D5F5F900DEFBFC00DCFAFC00DAF8
      FB00B7E5F0000000000000000000000000000000000000000000C0C0C000FBFB
      FC00FBFCFC00FCFDFD00FCFDFD00FDFEFE00FDFEFE00FBFCFC00FDFEFE00CACA
      CA00BABABA00000000000000000000000000000000008DE7F500B1F6FB00E9FB
      FD0099EAF40087DFED0079CDDC00BCF5FA00C2FAFD00C2FAFD00C2FAFD00B6F4
      F90086D5E80000000000000000000000000000000000DEDEDE00FCFCFC00FCFC
      FC00FCFDFD00FCFDFD00FDFDFD00FDFDFD00FCFCFC00FDFDFD00E3E3E300DBDB
      DB00F7F7F70000000000000000000000000000000000C5F2F900D7F9FC00F3FC
      FD00CBF3F800C2EEF500BBE5EC00DCF9FB00DFFBFD00DFFBFD00DFFBFD00D9F8
      FB00C1E8F2000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C0000000000000000000000000000000000000000000AFEDF8008EE7F5008DE4
      F40089E0F20088DEF10089DDF10086DBEF0083D9EE0080D7EC007DD5EA008BD8
      EA00C0E9F30000000000000000000000000000000000DEDEDE00DEDEDE00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FAFA
      FA000000000000000000000000000000000000000000D5F4FA00C5F1F800C5F0
      F800C3EEF700C2EDF700C3EDF700C1ECF600C0EBF500BEEAF400BDE9F300C3EA
      F300DEF2F700000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000C001C07F8003C07FC001800380038003
      C001800380038003C001800380038003C001800380038003C001800380038003
      C001800380038003C001800380038003C001800380038003C001800380038003
      C001800780038007C001800780038007C001800780038007C003800780038007
      C007800780078007C00F8007800F800700000000000000000000000000000000
      000000000000}
  end
end
