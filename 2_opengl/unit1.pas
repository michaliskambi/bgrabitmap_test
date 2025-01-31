unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BGLVirtualScreen, BGRAOpenGL,
  CastleTimeUtils, CastleLog;

type
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

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses BGRABitmapTypes;

procedure TForm1.BGLVirtualScreen1Redraw(Sender: TObject;
  BGLContext: TBGLContext);
var
  X: Single;
begin
  // BGLContext.Canvas.Fill(CSSWhite); // not necessary to clear (it seems - TODO find where it is cleared by BGRA)
  X := Frac(StartTime.ElapsedTime) * 1000;
  WritelnLog('from start: %f', [X]);
  BGLContext.Canvas.FillRect(X, 10, X + 100, 100, CSSRed);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StartTime := Timer;
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

