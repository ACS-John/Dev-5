def fnco_w2_margins_btn_ok
  fnreg_write('W-2 - Form 1 Y' ,s$(sio_txtW2f1y))
  fnreg_write('W-2 - Form 2 Y' ,s$(sio_txtW2f2y))
  fnreg_write('W-2 - X'        ,s$(sio_txtW2x)  )
  let ExitMode=QuitOnly
fnend
