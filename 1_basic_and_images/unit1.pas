unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    image: TBGRABitmap;
    SmallerImage: TBGRACustomBitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
var bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRABlack);
  bmp.FillRect(20, 20, 100, 40, BGRA(255,192,0), dmSet);  //fill an orange rectangle
  bmp.Draw(Canvas, 0, 0, True);                           //render BGRABitmap on the form
  bmp.Free;                                               //free memory

  //image.Draw(Canvas,100,100,True);
  //image.Draw(Canvas,200,200,True);

  SmallerImage.Draw(Canvas,100,100,True);
  SmallerImage.Draw(Canvas,200,200,True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create('cat-unicorn0.png');
  SmallerImage := image.Resample(100, 100);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Image);
  FreeAndNil(SmallerImage);
end;

end.

