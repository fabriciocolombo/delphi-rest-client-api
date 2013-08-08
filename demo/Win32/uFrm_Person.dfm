object Frm_Person: TFrm_Person
  Left = 441
  Top = 144
  Caption = 'Person'
  ClientHeight = 170
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    417
    170)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 58
    Top = 15
    Width = 9
    Height = 13
    Alignment = taRightJustify
    Caption = 'Id'
  end
  object Label2: TLabel
    Left = 39
    Top = 47
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = 'Name'
  end
  object Label3: TLabel
    Left = 10
    Top = 103
    Width = 57
    Height = 13
    Alignment = taRightJustify
    Caption = 'Create Date'
  end
  object Label4: TLabel
    Left = 42
    Top = 74
    Width = 25
    Height = 13
    Alignment = taRightJustify
    Caption = 'Email'
  end
  object edId: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object edName: TEdit
    Left = 72
    Top = 40
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 72
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Confirm'
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 176
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object edEmail: TEdit
    Left = 72
    Top = 67
    Width = 337
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edCreateDate: TEdit
    Left = 72
    Top = 96
    Width = 121
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
end
