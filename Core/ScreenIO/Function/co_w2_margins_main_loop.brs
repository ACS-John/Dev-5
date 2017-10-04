def fnco_w2_margins_main_loop
  ! fnmainloop ! necessary until next ScreenIO update is in place 1/24/2017
  ! if fkey=0 or (ControlIndex and lwrc$(UserData$(ControlIndex))=lwrc$("btnOK")) then
  !   fnreg_write('W-2 - Form 1 Y' ,s$(sio_txtW2f1y))
  !   fnreg_write('W-2 - Form 2 Y' ,s$(sio_txtW2f2y))
  !   fnreg_write('W-2 - X'        ,s$(sio_txtW2x)  )
  !   exitMode=QuitOnly
  ! 
  ! else if fkey=99 or (ControlIndex and lwrc$(UserData$(ControlIndex))=lwrc$("btnCancel")) then
  !   ExitMode=QuitOnly
  ! end if
  
  if fkey=0 then 
    fnco_w2_margins_btn_ok
  else if fkey=99 then
    ExitMode=QuitOnly
  end if
fnend
