unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BGLVirtualScreen, BGRAOpenGL, BGRAClasses, BGRABitmapTypes,
  CastleTimeUtils, CastleLog;

type
  TCircle = record
    Points: array of TPointF;
    Color: TBGRAPixel;
  end;

  TForm1 = class(TForm)
    BGLVirtualScreen1: TBGLVirtualScreen;
    LabelFps: TLabel;
    Timer1: TTimer;
    procedure BGLVirtualScreen1FramesPerSecond(Sender: TObject;
      BGLContext: TBGLContext; FramesPerSecond: integer);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    StartTime: TTimerResult;
    Circles: array of TCircle;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses {BGRABitmapTypes, } Math;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
var
  X: Single;
  I: Integer;
begin
  // BGLContext.Canvas.Fill(CSSWhite); // not necessary to clear (it seems - TODO find where it is cleared by BGRA)
  X := Frac(StartTime.ElapsedTime) * 1000;
  WritelnLog('from start: %f', [X]);
  BGLContext.Canvas.FillRect(X, 10, X + 100, 100, CSSRed);
  for I := 0 to Length(Circles) - 1 do
  begin
    BGLContext.Canvas.Polylines(Circles[I].Points, Circles[I].Color);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  CirclesCount = 100;
  PointsOnCircle = 10000;
  MaxRadius = 200.0; // actual max will be increased by 10
  CircleMiddleX = 200.0;
  CircleMiddleY = 200.0;
var
  I, J: Integer;
  S, C: Single;
begin
  Randomize;

  StartTime := Timer;

  SetLength(Circles, CirclesCount);
  for I := 0 to Length(Circles) - 1 do
  begin
    // random non-white color (because on white bg)
    Circles[I].Color := TBGRAPixel.New(
      Random(100) + 100,
      Random(100) + 100,
      Random(100) + 100
    );
    SetLength(Circles[I].Points, PointsOnCircle);
    for J := 0 to PointsOnCircle - 1 do
    begin
      SinCos(J * 2 * Pi / PointsOnCircle, S, C);
      Circles[I].Points[J].X := S * (10 + MaxRadius * I / CirclesCount) + CircleMiddleX;
      Circles[I].Points[J].Y := C * (10 + MaxRadius * I / CirclesCount) + CircleMiddleY;
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  BGLVirtualScreen1.Invalidate;
end;

procedure TForm1.BGLVirtualScreen1FramesPerSecond(Sender: TObject;
  BGLContext: TBGLContext; FramesPerSecond: integer);
begin
  { Measure time using BGRA OnFramesPerSecond, includes SwapBuffers,
    relies on TCustomOpenGLControl.FrameDiffTimeInMSecs , using GetTickCount.
    Good enough for now.
    TODO: compare with CGE Fps. }
  LabelFps.Caption := Format('FPS: %d', [FramesPerSecond]);
end;

initialization
  InitializeLog;
  Profiler.Enabled := true;
  Profiler.FloatPrecision := 4;
end.

