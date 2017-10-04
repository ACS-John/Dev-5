 ! function\selectfile.brs
 ! Created on 03/28/2009
 !
 ! fnSelectFile - This Function selects a file or a folder
 ! in the ScreenIO system using the BR File Selection Dialog
 !
 !
 def fnSelectFile(controlindex,mat subscripts$,mat controlname$,prefix$,mat s$,mat f$;___,Folder$*255,FileHandle)
    
    let FileSubscript=fnFindSubscript(mat Subscripts$,prefix$,controlname$(controlIndex))
    screenSubscript=fnFindSubscript(mat Subscripts$,"sio_",controlname$(controlIndex))

    let Folder$=trim$(f$(FileSubscript))
    if Folder$="" then let Folder$=".\"
    let Folder$=Folder$(1:pos(Folder$,"\",-1))&"*.*"

    open #(filehandle:=fnGetFileNumber): "name=open:"&Folder$&", recl=80", external, input error Ignore
    if file(filehandle)=0 then
       let folder$=trim$(file$(filehandle))
       close #filehandle:

       if pos(lwrc$(ControlName$(ControlIndex)),"folder") then
          let Folder$=Folder$(1:pos(Folder$,"\",-1))
       end if

       if FileSubscript then let f$(FileSubscript)=folder$
       if ScreenSubscript then s$(ScreenSubscript)=folder$
    end if
 fnend