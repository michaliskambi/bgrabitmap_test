# Test BGRABitmap + OpenGL

- https://wiki.freepascal.org/BGRABitmap_and_OpenGL

- Install BGLControls for `TBGLVirtualScreen` on palette

- For now measure time using BGRA OnFramesPerSecond, includes SwapBuffers,
  relies on TCustomOpenGLControl.FrameDiffTimeInMSecs,
  which is calculated (in UpdateFrameTimeDiff) using GetTickCount.
  Good enough.
