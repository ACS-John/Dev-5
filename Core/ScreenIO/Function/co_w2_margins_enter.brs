def fnco_w2_margins_enter
  library 'S:\Core\Library': fnreg_read,fnreg_write
  fnreg_read('W-2 - Form 1 Y',s$(sio_txtW2f1y),'10' )
  fnreg_read('W-2 - Form 2 Y',s$(sio_txtW2f2y),'151')
  fnreg_read('W-2 - X'       ,s$(sio_txtW2x)  ,'12' )
fnend
 