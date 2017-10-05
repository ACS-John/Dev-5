 ! function\printlistview.brs
 ! Created on 07/03/2014
 !
 ! fnPrintListview - This function prints a listview using Georges functions
 !  To use it, select this function for your pr Button, and set the
 !  pr buttons "UserData" field to match the Control Name for the listview
 !  that you wish to print.
 !
 ! You also may select a title, by specifying it in UserData in the format:
 !  TargetListview|Title
 !
 ! You also may specify column suppression info in the following format:
 !  TargetListview|Title|00000XXX
 !
 !  0 is total
 !  1 is no total
 !  2 is show field even when grouped by it
 !  X is suppress column
 !
 ! If no title is specified, then the Screen Name is used to get the title.
 ! If no column suppression info is specified, then no columns are suppressed
 !
 dim Title$*255
 dim TargetField$*255
 dim TotalSupp$*255
 dim S(1)
 !
 def fnPrintListview
    mat S(0)
    if ~exists(env$("pd")&"temp") then execute "mkdir "&env$("pd")&"temp"
    library env$("pd")&"vol002\RTFLIB.DLL" : fnListPrint

    totalSupp$=Title$=TargetField$=""
    if pos(UserData$(ControlIndex),"|") then
       title$=UserData$(ControlIndex)(pos(UserData$(ControlIndex),"|")+1:999)
       targetField$=UserData$(ControlIndex)(1:pos(UserData$(ControlIndex),"|")-1)
       if pos(Title$,"|") then
          totalSupp$=Title$(pos(Title$,"|")+1:999)
          title$=Title$(1:pos(Title$,"|")-1)
       end if
    else
       title$=uprc$(ScreenIO$(si_ScreenCode)(1:1))&lwrc$(ScreenIO$(si_ScreenCode)(2:999))&" List Print"
       targetField$=UserData$(ControlIndex)
    end if
    
    listviewSubscript=fnFindSubscript(mat Subscripts$,"ctl_",TargetField$)
    if ListviewSubscript then
       fnListPrint(Window,fnListSpec$(ControlSpec$(ListviewSubscript)),"","","",mat S,1,0,totalsupp$)
    else
       msgbox("Couldn't find the target listview. Please ensure a valid listview is selected for the pr button, or contact the programmer who worked on your system to fix this error.")
    end if
 fnend
