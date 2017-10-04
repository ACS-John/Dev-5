 ! function\selectthis.brs
 !
 ! Created on 01/12/2009
 !
 ! This function selects a thing from the target (current fields name) file
 ! and returns it to the current field, while setting it in the matching file
 ! position in whatever the current file is
 !
 def fnSelectThis(controlindex,mat subscripts$,mat controlname$,prefix$,mat s$,mat f$;___,FileSubscript,Return$*255,ScreenSubscript)
    let FileSubscript=fnFindSubscript(mat Subscripts$,prefix$,controlname$(controlIndex))
    screenSubscript=fnFindSubscript(mat Subscripts$,"sio_",controlname$(controlIndex))

    let Return$=fnCallScreen$("["&trim$(controlname$(controlindex))&"]")
    if len(trim$(Return$)) then
       let f$(FileSubscript)=Return$
       s$(ScreenSubscript)=fnReadUnopenedDescription$(trim$(controlname$(controlindex)),Return$)
    end if
 fnend