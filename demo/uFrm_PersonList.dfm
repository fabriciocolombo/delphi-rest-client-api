object Frm_PersonList: TFrm_PersonList
  Left = 425
  Top = 149
  Caption = 'Person List'
  ClientHeight = 299
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    639
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 615
    Height = 249
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'Id'
      end
      item
        Caption = 'Name'
        Width = 230
      end
      item
        Caption = 'Email'
        Width = 203
      end
      item
        Caption = 'Create Date'
        Width = 120
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnAdd: TButton
    Left = 8
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnUpdate: TButton
    Left = 96
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 2
    OnClick = btnUpdateClick
  end
  object btnRemove: TButton
    Left = 184
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnRefresh: TButton
    Left = 273
    Top = 263
    Width = 75
    Height = 25
    Caption = 'Refresh'
    TabOrder = 4
    OnClick = btnRefreshClick
  end
  object btnReset: TButton
    Left = 369
    Top = 263
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 5
    OnClick = btnResetClick
  end
end
