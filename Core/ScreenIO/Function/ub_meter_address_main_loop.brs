! S:\Core\ScreenIO\Function\ub_meter_address_main_loop.brs
! Created on 04/01/2017
!
! r: fnub_meter_address_main_loop - This function ...
!
!
! Fill in the comment above with a description of what this
! particular function does.
!
!
! This is a Main Loop Event Function. This code is run every time
! the user does anything on your screen. Use it to handle special
! user interactions such as custom Fkeys and double clicks.
!/r
def fnub_meter_address_main_loop
  ! if fkey=0 or fkey=200 or fkey=201 then 
  if fkey=0 or fkey=200 or fkey=201 then
     ExitMode=SelectAndQuit
  else if fkey=99 then
    ExitMode=QuitOnly
  end if

fnend
