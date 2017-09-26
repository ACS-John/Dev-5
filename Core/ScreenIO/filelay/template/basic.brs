 ! FileIO Library Basic Addon Template by Gabriel Bakker
 ! Copyright 2012 Gabriel Bakker, Sage AX
 ! Created: 2/6/2012
 !
 ! Use this template library as an example of how
 ! to construct other template libraries.
 !
 ! Every Template must have two library functions,
 !  fnTemplateList and fnRunTemplate.
 !
 !  fnTemplateList(mat TemplateDesc$)
 !  takes an array of descriptions and returns a list of
 !  the available functions in this template library.
 !
 !  fnRunTemplate(Template,FileLay$)
 !  this function takes the given template Index and
 !  file layout and calls your template function.
 !
 !  A template function should read the file layout
 !  and then build a string of code and return it
 !  in the clip board so that a programmer can
 !  paste it into his editor of choice later.
 !
 !
 ! To add to this template library, add your
 !  template description and template label
 !  to the list and gosub statement at the
 !  bottom of this library.
 !
 !  Then write some template code at that label
 !  in a subroutine that sets the code to return
 !  in the "ReturnCode$" variable, or use the
 !  fnReturnCode function.

 dim Keys$(1)*255,KeyDescription$(1)*255,Ssubs$(1),Nsubs$(1),Sspec$(1),Nspec$(1)
 dim Sdescription$(1)*255,Ndescription$(1)*255,Spos(1),Npos(1)
 dim FileName$*255,Prefix$
 dim LongestElement
 
 dim NumberList$
 dim KeyFields$(1)
 dim KeyGiven
 dim KeyLength, Length, Sub

 dim CodeLine$*400


 ! *******************************************
 !             OPEN WITHOUT FILEIO
 ! *******************************************
 OpenWithout: ! Template that creates code to self register
 ! this layout if its not found in the layout folder already

    ! Dim at next largest multiple of 255
    let LongestElement=(int((LongestElement-1)/255)+1)*255

    let FileLay$=Trim$(FileLay$)
    
    let fnReturnCode(' ')
    let fnReturnCode(' ! FileIO Dimensions')
    let fnReturnCode(' dim Form$(1)*255')
    let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Open the file')
    let fnReturnCode(' library "fileio" : fnGetFileNumber')
    let fnReturnCode(' let '&FileLay$&'=fnGetFileNumber(-1,'&str$(udim(mat Keys$))&')')
    let fnReturnCode(' ')
    let fnReturnCode(' mat Form$(max(udim(mat Form$),'&FileLay$&')) ! Make room in forms array')
    let fnReturnCode(' mat '&FileLay$&'('&str$(udim(mat NSubs$))&') : mat '&FileLay$&'$('&str$(udim(mat SSubs$))&')')
    let fnReturnCode(' ')
    let fnReturnCode(' let Form$('&FileLay$&')="'&RawForm$&'"')
    let fnReturnCode(' let Form$('&FileLay$&')=cform$(form$('&FileLay$&'))')

    if udim(mat Keys$) then
       for Index=1 to udim(mat Keys$)
          let Additor=Index-1
          if Additor then
             let fnReturnCode(' open #'&FileLay$&'+'&str$(Additor)&': "name='&FileName$&',kfname='&Keys$(Index)&'",internal,outin,keyed')
          else
             let fnReturnCode(' open #'&FileLay$&': "name='&FileName$&',kfname='&Keys$(Index)&'",internal,outin,keyed')
          end if
       next Index
    else
       let fnReturnCode(' open #'&FileLay$&': "name='&FileName$&'",internal,outin,relative')
    end if
    
    let fnReturnCode(' ')
    let fnReturnCode(' ! Set Subscripts')
    for Index=1 to udim(mat SSubs$)
       let fnReturnCode(' let '&Prefix$&SSubs$(Index)&'='&str$(Index))
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode(' let '&Prefix$&NSubs$(Index)&'='&str$(Index))
    next Index

 return

 ! *******************************************
 !                Conversion
 ! *******************************************
 Conversion: ! Creates conversion functions that can be used
 ! to go between fileio vars and oldskool vars to aid in
 ! implementing fileio in older programs.
 
    let fnReturnCode(' Unpack'&trim$(FileLay$)&': ! This routine unpacks the '&trim$(FileLay$)&' file.')
    let fnReturnCode('    ! from mat f$ and mat f arrays into individual variables')
    let fnReturnCode('    def fnUnpack'&trim$(FileLay$)&'(mat F$, mat F)')

    for Index=1 to udim(mat SSubs$)
       let fnReturnCode('       let '&ssubs$(Index)&'$=f$('&Prefix$&Ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode('       let '&Nsubs$(Index)&'=f('&Prefix$&Nsubs$(Index)&')')
    next Index

    let fnReturnCode ('       let fnUnpack'&trim$(FileLay$)&'=1 ! Return True')
    let fnReturnCode ('    fnend')
    let fnReturnCode (' ')
    let fnReturnCode (' ')
    let fnReturnCode(' Pack'&trim$(FileLay$)&': ! This routine packs the '&trim$(FileLay$)&' file.')
    let fnReturnCode('    ! from individual variables back into mat f$ and mat f arrays')
    let fnReturnCode('    def fnPack'&trim$(FileLay$)&'(mat F$, mat F)')

    for Index=1 to udim(mat SSubs$)
       let fnReturnCode('       let f$('&Prefix$&Ssubs$(Index)&')='&ssubs$(Index)&'$')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode('       let f('&Prefix$&Nsubs$(Index)&')='&Nsubs$(Index))
    next Index

    let fnReturnCode ('       let fnPack'&trim$(FileLay$)&'=1 ! Return True')
    let fnReturnCode ('    fnend')

 return

 ! *******************************************
 !       LISTVIEW/ADD/EDIT Save
 ! *******************************************
 LAESave: ! Creates code that can be pasted
    ! into the Save Buttons Click Event to create
    ! Combination Listview/Add/Edit screens the
    ! easy way.

    ! Get code from making one and trying it.
    !  but it should be something like:
    !    read #Datafile, using form$(DataFile), key=CurrentKey$ : mat F$, mat F
    !    if sio_name then let F$(ac_name)=s$(sio_name)
    !    if sio_add1 then let F$(ac_add1)=s$(sio_add1)
    !    if sio_price then let f(ac_price)=val(s$(sio_price)) conv Ignore
    !    rewrite #Datafile, using form$(datafile) : mat f$, mat f

    let fnReturnCode('    read #DataFile, using form$(DataFile), key=CurrentKey$ : mat F$, mat F')
    let fnReturnCode(' ')
    for Index=1 to udim(mat SSubs$)
       let fnReturnCode('    if sio_field_'&Ssubs$(Index)&' then let f$('&prefix$&ssubs$(Index)&')=s$(sio_field_'&Ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode('    if sio_field_'&Nsubs$(Index)&' then let f('&prefix$&Nsubs$(Index)&')=val(s$(sio_field_'&Nsubs$(Index)&')) conv Ignore')
    next Index
    let fnReturnCode(' ')
    let fnReturnCode('    rewrite #DataFile, using form$(DataFile) : mat F$, mat F')
    let fnReturnCode('    let RepopulateListviews=1')

 return

 ! *******************************************
 !       LISTVIEW/ADD/EDIT Read
 ! *******************************************
 LAERead:  ! Creates code that can be pasted
    ! into the Listview Read Event to create
    ! Combination Listview/Add/Edit screens the
    ! easy way.

    ! Get code from making one and trying it.
    !  but it should be something like:
    !    if sio_name then let s$(sio_name)=F$(ac_name)
    !    if sio_add1 then let s$(sio_Add1)=F$(ac_add1)

    for Index=1 to udim(mat SSubs$)
       let fnReturnCode('    if sio_field_'&Ssubs$(Index)&' then let s$(sio_field_'&Ssubs$(Index)&')=f$('&prefix$&ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode('    if sio_field_'&Nsubs$(Index)&' then let s$(sio_field_'&Nsubs$(Index)&')=str$(f('&prefix$&Nsubs$(Index)&'))')
    next Index

 return

 ! *******************************************
 !                READ LOOP
 ! *******************************************
 ReadLoop: ! Template for a standard Read Loop and dim statement

    ! Dim at next largest multiple of 255
    let LongestElement=(int((LongestElement-1)/255)+1)*255

    let fnReturnCode(' ')
    let fnReturnCode(' ! FileIO Dimensions')
    let fnReturnCode(' dim Form$(1)*255')
    let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Open the file')
    let fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Position to start of file')
    let fnReturnCode(' restore #'&FileLay$&': error IGNORE')
    let fnReturnCode(' ')
    let fnReturnCode(' do until File('&FileLay$&')')
    let fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
    let fnReturnCode('    if File('&FileLay$&')=0 then')
    let fnReturnCode('       ! Found Something')
    let fnReturnCode(' ')
    let fnReturnCode(' ')
    let fnReturnCode(' ')
    let fnReturnCode('    end if')
    let fnReturnCode(' loop')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Close DataFile')
    let fnReturnCode(' close #'&FileLay$&':')
    let fnReturnCode(' ')

 return

 ! *******************************************
 !                READ LOOP KEY
 ! *******************************************
 ReadLoopKey: ! Template for a keyed Read Loop and dim statement

    ! Dim at next largest multiple of 256
    let LongestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       let str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       let fnReturnCode(' ')
       let fnReturnCode(' ! FileIO Dimensions')
       let fnReturnCode(' dim Form$(1)*255')
       let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Variables Used')
       let fnReturnCode(' dim KeyNumber')

       let CodeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let Length=fnLength(SSpec$(Sub))
             let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             let Sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                let Length=fnLength(NSpec$(Sub))
                let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          let CodeLine$=CodeLine$(1:len(CodeLine$)-2)
          let fnReturnCode(CodeLine$)
       end if

       let fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       let fnReturnCode(' ')
       let fnReturnCode(' ! Initialize Key Information Here')
       let fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             let fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       let fnReturnCode(' ')
       let fnReturnCode(' ! Open the file')
       let fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1,KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             let fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       let fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Position by key')
       let fnReturnCode(' restore #'&FileLay$&', search>=rtrm$('&FileLay$&'Key$): nokey IGNORE')
       let fnReturnCode(' ')
       let fnReturnCode(' do until File('&FileLay$&')')
       let fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
       let fnReturnCode('    let This'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode('    if File('&FileLay$&')=0 And rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$))) then')
       let fnReturnCode('       ! Found Something')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode('    end if')
       let fnReturnCode(' loop while rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$)))')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Close DataFile')
       let fnReturnCode(' close #'&FileLay$&':')
       let fnReturnCode(' ')
    end if
 return

 ! *******************************************
 !              READ LOOP ONLY
 ! *******************************************
 ReadLoopOnly: ! Template for a standard Read Loop and dim statement

    ! Dim at next largest multiple of 255
    let LongestElement=(int((LongestElement-1)/255)+1)*255

    let fnReturnCode(' ')
    let fnReturnCode(' ! Position to start of file')
    let fnReturnCode(' restore #'&FileLay$&': error IGNORE')
    let fnReturnCode(' ')
    let fnReturnCode(' do until File('&FileLay$&')')
    let fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
    let fnReturnCode('    if File('&FileLay$&')=0 then')
    let fnReturnCode('       ! Found Something')
    let fnReturnCode(' ')
    let fnReturnCode(' ')
    let fnReturnCode(' ')
    let fnReturnCode('    end if')
    let fnReturnCode(' loop')
    let fnReturnCode(' ')

 return

 ! *******************************************
 !             READ LOOP ONLY KEY
 ! *******************************************
 ReadLoopKeyOnly: ! Template for a keyed Read Loop and dim statement

    ! Dim at next largest multiple of 256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       let str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       let fnReturnCode(' ')
       let fnReturnCode(' ! Variables Used')
       let fnReturnCode(' dim KeyNumber')

       let CodeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let Length=fnLength(SSpec$(Sub))
             let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             let Sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                let Length=fnLength(NSpec$(Sub))
                let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          let CodeLine$=CodeLine$(1:len(CodeLine$)-2)
          let fnReturnCode(CodeLine$)
       end if

       let fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       let fnReturnCode(' ')
       let fnReturnCode(' ! Initialize Key Information Here')
       let fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             let fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       let fnReturnCode(' ')
       let fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             let fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       let fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Position by key')
       let fnReturnCode(' restore #'&FileLay$&', search>=rtrm$('&FileLay$&'Key$): nokey IGNORE')
       let fnReturnCode(' ')
       let fnReturnCode(' do until File('&FileLay$&')')
       let fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
       let fnReturnCode('    let This'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode('    if File('&FileLay$&')=0 And rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$))) then')
       let fnReturnCode('       ! Found Something')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode('    end if')
       let fnReturnCode(' loop while rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$)))')
       let fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                OPEN THE FILE
 ! *******************************************
 OpenFile: ! Template for Opening the file

    ! Dim at next largest multiple of 256
    let LongestElement=(int((LongestElement-1)/256)+1)*256

    let fnReturnCode(' ')
    let fnReturnCode(' ! FileIO Dimensions')
    let fnReturnCode(' dim Form$(1)*255')
    let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Open the file')
    let fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$)')
    let fnReturnCode(' ')
 return


 ! *******************************************
 !               WRITE CODE KEY
 ! *******************************************
 WriteCodeKey: ! Template for a keyed writing and dim statements

    ! Dim at next largest multiple of 256
    let LongestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       let str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       let fnReturnCode(' ')
       let fnReturnCode(' ! FileIO Dimensions')
       let fnReturnCode(' dim Form$(1)*255')
       let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Variables Used')
       let fnReturnCode(' dim KeyNumber')

       let CodeLine$=" dim "
       let KeyLength=0
       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let Length=fnLength(SSpec$(Sub))
             let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             let Sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                let Length=fnLength(NSpec$(Sub))
                let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          let CodeLine$=CodeLine$(1:len(CodeLine$)-2)
          let fnReturnCode(CodeLine$)
       end if

       let fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       let fnReturnCode(' ')
       let fnReturnCode(' ! Initialize Key Information Here')
       let fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             let fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       let fnReturnCode(' ')
       let fnReturnCode(' ! Open the file')
       let fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1,KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             let fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       let fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode(' read #'&FileLay$&', using Form$('&FileLay$&'), key='&FileLay$&'Key$ : Mat '&FileLay$&'$,Mat '&FileLay$&' eof Ignore')
       let fnReturnCode(' if File('&FileLay$&')=0 And '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&') then')
       let fnReturnCode('    ! Found It: Set Values')

       for Index=1 to udim(mat SSubs$)
          let fnReturnCode('    let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
       next Index
       for Index=1 to udim(mat NSubs$)
          let fnReturnCode('    let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
       next Index

       let fnReturnCode(' ')
       let fnReturnCode('    ! Save The Record')
       let fnReturnCode('    rewrite #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
       let fnReturnCode(' end if')
       let fnReturnCode(' ')
       let fnReturnCode(' ! Close DataFile')
       let fnReturnCode(' let fnCloseFile('&FileLay$&',"'&FileLay$&'")')
       let fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                WRITE CODE
 ! *******************************************
 WriteCode: ! Template for writing and dim statements

    ! Dim at next largest multiple of 256
    let LongestElement=(int((LongestElement-1)/256)+1)*256

    let fnReturnCode(' ')
    let fnReturnCode(' ! FileIO Dimensions')
    let fnReturnCode(' dim Form$(1)*255')
    let fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Open the file')
    let fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$)')
    let fnReturnCode(' ')
    let fnReturnCode(' ! Set Values')

    for Index=1 to udim(mat SSubs$)
       let fnReturnCode(' let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode(' let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
    next Index

    let fnReturnCode(' ')
    let fnReturnCode(' ! Save The Record')
    let fnReturnCode(' write #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
    let fnReturnCode(' ')
    let fnReturnCode(' ! Close DataFile')
    let fnReturnCode(' let fnCloseFile('&FileLay$&',"'&FileLay$&'")')
    let fnReturnCode(' ')

 return


 ! *******************************************
 !             WRITE CODE ONLY KEY
 ! *******************************************
 WriteCodeKeyOnly: ! Template for a keyed writing and dim statements

    ! Dim at next largest multiple of 256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       let str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       let fnReturnCode(' ')
       let fnReturnCode(' ! Variables Used')
       let fnReturnCode(' dim KeyNumber')

       let CodeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let Length=fnLength(SSpec$(Sub))
             let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             let Sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                let Length=fnLength(NSpec$(Sub))
                let CodeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          let CodeLine$=CodeLine$(1:len(CodeLine$)-2)
          let fnReturnCode(CodeLine$)
       end if

       let fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       let fnReturnCode(' ')
       let fnReturnCode(' ! Initialize Key Information Here')
       let fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             let fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       let fnReturnCode(' ')
       let fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          let Sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             let fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             let fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       let fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       let fnReturnCode(' ')
       let fnReturnCode(' ')
       let fnReturnCode(' read #'&FileLay$&', using Form$('&FileLay$&'), key='&FileLay$&'Key$ : Mat '&FileLay$&'$,Mat '&FileLay$&' eof Ignore')
       let fnReturnCode(' if File('&FileLay$&')=0 And '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&') then')
       let fnReturnCode('    ! Found It: Set Values')

       for Index=1 to udim(mat SSubs$)
          let fnReturnCode('    let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
       next Index
       for Index=1 to udim(mat NSubs$)
          let fnReturnCode('    let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
       next Index

       let fnReturnCode(' ')
       let fnReturnCode('    ! Save The Record')
       let fnReturnCode('    rewrite #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
       let fnReturnCode(' end if')
       let fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                WRITE CODE ONLY
 ! *******************************************
 WriteCodeOnly: ! Template for writing and dim statements
    let fnReturnCode(' ')
    let fnReturnCode(' ! Set Values')

    for Index=1 to udim(mat SSubs$)
       let fnReturnCode(' let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
    next Index
    for Index=1 to udim(mat NSubs$)
       let fnReturnCode(' let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
    next Index

    let fnReturnCode(' ')
    let fnReturnCode(' ! Save The Record')
    let fnReturnCode(' write #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
    let fnReturnCode(' ')
 return

 ! *******************************************
 !               OPEN FUNCTION
 ! *******************************************
 OpenFunction: ! Standard FileIO Open Function
    let fnReturnCode(' dim LinkageEstablished')
    let fnReturnCode(' def fnEstablishLinkage')
    let fnReturnCode('    if ~LinkageEstablished then')
    let fnReturnCode('       library "fileio" : fnOpenFile,Fnclosefile,Fngetfilenumber,Fnkey$,FnBuildKey$,Fnreadlayoutarrays,Fndoeslayoutexist,Fnreadallkeys,fnReadRelativeDescription$,fnReadRelUnopenedDescription$,fnReadRelUnopenedNumber,fnUpdateFile,fnLog,fnLogArray,fnErrLog,fnReadLayouts,Fnmakeuniquekey$,FnDisplayLength,FnLength,FnReadDescription$,FnReadUnopenedDescription$,fnReadRecordWhere$,fnUniqueKey,fnReadNumber,fnReadUnopenedNumber,fnReadRelativeNumber,fnNotInFile,fnDataCrawler,fnDataEdit')
    let fnReturnCode('       library "fileio" : fnMakeSubProc,fnReadMatchingKeys,fnReadAllNewKeys,fnReadFilterKeys,fnReadEntireLayout,fnReadLayoutHeader,fnReadSubs,fnReadLayoutPath$,fnReadKeyFiles, fnAskCombo$,fnRunProcFile,fnBuildProcFile,fnDataShow')
    let fnReturnCode('       library "screenio" : fnCallScreen$,fnFindSubscript,fnFm$,fnfm,fnDisplayScreen,fnGetUniqueName$,fnIsInputSpec,fnIsOutputSpec,fnDays,fnBR42')
    let fnReturnCode('       let linkageEstablished=1')
    let fnReturnCode('    end if')
    let fnReturnCode(' fnend')
    let fnReturnCode(' !')
    let fnReturnCode(' ! #Auton'&'umber# 99000,10')
    let fnReturnCode(' OPEN: ! ***** Function To Call Library Openfile And Proc Subs')
    let fnReturnCode('       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)')
    let fnReturnCode('          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80')
    let fnReturnCode('          let Fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)')
    let fnReturnCode('          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : let _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index')
    let fnReturnCode('       fnend')
    let fnReturnCode('')
    let fnReturnCode(' Ignore: Continue')
 return

 dim ReturnCode$*20000
 def fnReturnCode(String$*512)
    let ReturnCode$=ReturnCode$&String$&chr$(13)&chr$(10)
 fnend

 dim RawForm$*10000
 
 def library fnRunTemplate(Template,FileLay$;___,Index)
    let fnEstablishLinkage
    let ReturnCode$=""
    let fnReadEntireLayout(FileLay$,Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
    let RawForm$=fnReadForm$(FileLay$)
    let LongestElement=0
    for Index=1 to udim(mat SSpec$)
       let LongestElement=max(LongestElement,fnLength(SSpec$(Index)))
    next Index
    for Index=1 to udim(mat NSpec$)
       let LongestElement=max(LongestElement,fnLength(NSpec$(Index)))
    next Index
    for Index=1 to udim(mat SSubs$)
       let SSubs$(Index)=lwrc$(SSubs$(Index))
    next Index
    for Index=1 to udim(mat NSubs$)
       let NSubs$(Index)=lwrc$(NSubs$(Index))
    next Index
    let Prefix$=trim$(Prefix$)
    gosub RunTemplate

    if len(ReturnCode$) then let SetEnv("CLIPBOARD",ReturnCode$)
 fnend

 RunTemplate: ! Template List gosub statement:
    on Template gosub ReadLoop, ReadLoopKey, ReadLoopOnly, ReadLoopKeyOnly, WriteCode, WriteCodeKey, WriteCodeOnly, WriteCodeKeyOnly, OpenFile, OpenFunction, OpenWithout, Conversion, LAERead, LAESave
 return

 TemplateList: ! Dictionary listing of available functions
    data "Read Loop"
    data "Read Loop Key"
    data "Read Loop Only"
    data "Read Loop Only Key"
    data "Write Code"
    data "Write Code Key"
    data "Write Code Only"
    data "Write Code Only Key"
    data "Open File"
    data "Open Function"
    data "Open w/out Fileio"
    data "Conversion"
    data "Listview/Add/Edit Read"
    data "Listview/Add/Edit Save"

 def library fnTemplateList(mat TemplateDesc$)
    restore TemplateList
    mat TemplateDesc$(14)
    read mat TemplateDesc$
 fnend
 
 dim LinkageEstablished
 def fnEstablishLinkage
    if ~LinkageEstablished then
       library "fileio" : fnOpenFile, fnClose, fnReadEntireLayout, fnGetFileNumber, fnGetKeyElements, fnLength, fnAskCombo$, fnReadForm$
       let linkageEstablished=1
    end if
 fnend
 !
 ! #Autonumber# 99000,10
 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)
          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80
          let Fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)
          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : let _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
       fnend

 Ignore: Continue