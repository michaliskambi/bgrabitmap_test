unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BGLVirtualScreen, BGRAOpenGL, BGRAClasses, BGRABitmapTypes,
  BGRABitmap,
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
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    StartTime: TTimerResult;
    Circles: array of TCircle;
    Bmp: TBGLBitmap;
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
  { Not necessary to clear.
    TCustomBGLVirtualScreen.DoOnPaint always calls BGLViewPort before
    drawing. If only TCustomBGLVirtualScreen.Color is not clNone,
    the BGLViewPort is called with 3rd argument (color to clear) and it fills
    the contents. }
  //BGLContext.Canvas.Fill(CSSWhite);

  X := Frac(StartTime.ElapsedTime) * 1000;
  WritelnLog('from start: %f', [X]);
  BGLContext.Canvas.FillRect(X, 10, X + 100, 100, CSSRed);
  for I := 0 to Length(Circles) - 1 do
  begin
    BGLContext.Canvas.Polylines(Circles[I].Points, Circles[I].Color);
  end;

  // create GL resources when context is current
  if Bmp = nil then
    Bmp := TBGLBitmap.Create(256, 256);

  { Note:

    Docs https://wiki.freepascal.org/BGRABitmap_and_OpenGL say:
    "Instead of using TBGRABitmap class, use TBGLBitmap class of BGRAOpenGL unit.
    It is similar..." , https://wiki.freepascal.org/BGRABitmap_and_OpenGL .
    TBGLBitmap *does* descend from TBGRABitmap and TBGRACustomBitmap ,
    so it shares similar API.

    But using TBGLBitmap doesn't mean one is drawing "with OpenGL power".
    E.g. it doesn't override how polylines are drawn.

    Instead, TBGLCustomBitmap.GetTexture just copies the CPU-drawn contents to GPU,
    by calling TBGLCustomTexture.Update which calls TBGLTexture.UpdateOpenGLTexture
    which calls glTexImage2D -- this just loads data from regular memory into GPU,
    so you get no benefit from how you drawn e.g. polylines in that data.
    It's not doing hardware-optimized drawing of polylinesin this case.

    Confirm this by tracing below code:

    - DrawPolyLineAntialias gets called each frame, resulting in
      ComputeWidePolylinePoints.
    - TBGLTexture.UpdateOpenGLTexture and glTexImage2D get called each frame.
    - TBGLCanvas.InternalStartPolyline is not called.
  }
  {$ifdef TRACE_TBGLBitmap_drawing}
  WritelnLog('TBGLBitmap drawing: begin');
  {$endif}

  Bmp.Fill(CSSBlue);
  // draw line, changing each frame in Y, to see the Bmp gets updated
  Bmp.DrawPolyLineAntialias([
    PointF(10,   10 + X / 10),
    PointF(100, 100 + X / 10),
    PointF(100,       X / 10)
  ], CSSYellow, 10);
  BGLContext.Canvas.PutImage(X, 200, Bmp.Texture, CSSWhite);

  {$ifdef TRACE_TBGLBitmap_drawing}
  WritelnLog('TBGLBitmap drawing: end');
  {$endif}
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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Bmp);
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

