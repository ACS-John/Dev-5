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
    longestElement=(int((LongestElement-1)/255)+1)*255

    let FileLay$=Trim$(FileLay$)
    
    fnReturnCode(' ')
    fnReturnCode(' ! FileIO Dimensions')
    fnReturnCode(' dim Form$(1)*255')
    fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    fnReturnCode(' ')
    fnReturnCode(' ! Open the file')
    fnReturnCode(' library "fileio" : fnGetFileNumber')
    fnReturnCode(' let '&FileLay$&'=fnGetFileNumber(-1,'&str$(udim(mat Keys$))&')')
    fnReturnCode(' ')
    fnReturnCode(' mat Form$(max(udim(mat Form$),'&FileLay$&')) ! Make room in forms array')
    fnReturnCode(' mat '&FileLay$&'('&str$(udim(mat NSubs$))&') : mat '&FileLay$&'$('&str$(udim(mat SSubs$))&')')
    fnReturnCode(' ')
    fnReturnCode(' let Form$('&FileLay$&')="'&RawForm$&'"')
    fnReturnCode(' let Form$('&FileLay$&')=cform$(form$('&FileLay$&'))')

    if udim(mat Keys$) then
       for Index=1 to udim(mat Keys$)
          additor=Index-1
          if Additor then
             fnReturnCode(' open #'&FileLay$&'+'&str$(Additor)&': "name='&FileName$&',kfname='&Keys$(Index)&'",internal,outin,keyed')
          else
             fnReturnCode(' open #'&FileLay$&': "name='&FileName$&',kfname='&Keys$(Index)&'",internal,outin,keyed')
          end if
       next Index
    else
       fnReturnCode(' open #'&FileLay$&': "name='&FileName$&'",internal,outin,relative')
    end if
    
    fnReturnCode(' ')
    fnReturnCode(' ! Set Subscripts')
    for Index=1 to udim(mat SSubs$)
       fnReturnCode(' let '&Prefix$&SSubs$(Index)&'='&str$(Index))
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode(' let '&Prefix$&NSubs$(Index)&'='&str$(Index))
    next Index

 return

 ! *******************************************
 !                Conversion
 ! *******************************************
 Conversion: ! Creates conversion functions that can be used
 ! to go between fileio vars and oldskool vars to aid in
 ! implementing fileio in older programs.
 
    fnReturnCode(' Unpack'&trim$(FileLay$)&': ! This routine unpacks the '&trim$(FileLay$)&' file.')
    fnReturnCode('    ! from mat f$ and mat f arrays into individual variables')
    fnReturnCode('    def fnUnpack'&trim$(FileLay$)&'(mat F$, mat F)')

    for Index=1 to udim(mat SSubs$)
       fnReturnCode('       let '&ssubs$(Index)&'$=f$('&Prefix$&Ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode('       let '&Nsubs$(Index)&'=f('&Prefix$&Nsubs$(Index)&')')
    next Index

    fnReturnCode ('       fnUnpack'&trim$(FileLay$)&'=1 ! Return True')
    fnReturnCode ('    fnend')
    fnReturnCode (' ')
    fnReturnCode (' ')
    fnReturnCode(' Pack'&trim$(FileLay$)&': ! This routine packs the '&trim$(FileLay$)&' file.')
    fnReturnCode('    ! from individual variables back into mat f$ and mat f arrays')
    fnReturnCode('    def fnPack'&trim$(FileLay$)&'(mat F$, mat F)')

    for Index=1 to udim(mat SSubs$)
       fnReturnCode('       let f$('&Prefix$&Ssubs$(Index)&')='&ssubs$(Index)&'$')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode('       let f('&Prefix$&Nsubs$(Index)&')='&Nsubs$(Index))
    next Index

    fnReturnCode ('       fnPack'&trim$(FileLay$)&'=1 ! Return True')
    fnReturnCode ('    fnend')

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

    fnReturnCode('    read #DataFile, using form$(DataFile), key=CurrentKey$ : mat F$, mat F')
    fnReturnCode(' ')
    for Index=1 to udim(mat SSubs$)
       fnReturnCode('    if sio_field_'&Ssubs$(Index)&' then let f$('&prefix$&ssubs$(Index)&')=s$(sio_field_'&Ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode('    if sio_field_'&Nsubs$(Index)&' then let f('&prefix$&Nsubs$(Index)&')=val(s$(sio_field_'&Nsubs$(Index)&')) conv Ignore')
    next Index
    fnReturnCode(' ')
    fnReturnCode('    rewrite #DataFile, using form$(DataFile) : mat F$, mat F')
    fnReturnCode('    let RepopulateListviews=1')

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
    !    if sio_name then s$(sio_name)=F$(ac_name)
    !    if sio_add1 then s$(sio_Add1)=F$(ac_add1)

    for Index=1 to udim(mat SSubs$)
       fnReturnCode('    if sio_field_'&Ssubs$(Index)&' then s$(sio_field_'&Ssubs$(Index)&')=f$('&prefix$&ssubs$(Index)&')')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode('    if sio_field_'&Nsubs$(Index)&' then s$(sio_field_'&Nsubs$(Index)&')=str$(f('&prefix$&Nsubs$(Index)&'))')
    next Index

 return

 ! *******************************************
 !                READ LOOP
 ! *******************************************
 ReadLoop: ! Template for a standard Read Loop and dim statement

    ! Dim at next largest multiple of 255
    longestElement=(int((LongestElement-1)/255)+1)*255

    fnReturnCode(' ')
    fnReturnCode(' ! FileIO Dimensions')
    fnReturnCode(' dim Form$(1)*255')
    fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    fnReturnCode(' ')
    fnReturnCode(' ! Open the file')
    fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1)')
    fnReturnCode(' ')
    fnReturnCode(' ! Position to start of file')
    fnReturnCode(' restore #'&FileLay$&': error IGNORE')
    fnReturnCode(' ')
    fnReturnCode(' do until File('&FileLay$&')')
    fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
    fnReturnCode('    if File('&FileLay$&')=0 then')
    fnReturnCode('       ! Found Something')
    fnReturnCode(' ')
    fnReturnCode(' ')
    fnReturnCode(' ')
    fnReturnCode('    end if')
    fnReturnCode(' loop')
    fnReturnCode(' ')
    fnReturnCode(' ! Close DataFile')
    fnReturnCode(' close #'&FileLay$&':')
    fnReturnCode(' ')

 return

 ! *******************************************
 !                READ LOOP KEY
 ! *******************************************
 ReadLoopKey: ! Template for a keyed Read Loop and dim statement

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! FileIO Dimensions')
       fnReturnCode(' dim Form$(1)*255')
       fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          codeLine$=CodeLine$(1:len(CodeLine$)-2)
          fnReturnCode(CodeLine$)
       end if

       fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       fnReturnCode(' ')
       fnReturnCode(' ! Initialize Key Information Here')
       fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       fnReturnCode(' ')
       fnReturnCode(' ! Open the file')
       fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1,KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ! Position by key')
       fnReturnCode(' restore #'&FileLay$&', search>=rtrm$('&FileLay$&'Key$): nokey IGNORE')
       fnReturnCode(' ')
       fnReturnCode(' do until File('&FileLay$&')')
       fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
       fnReturnCode('    let This'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode('    if File('&FileLay$&')=0 And rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$))) then')
       fnReturnCode('       ! Found Something')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode('    end if')
       fnReturnCode(' loop while rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$)))')
       fnReturnCode(' ')
       fnReturnCode(' ! Close DataFile')
       fnReturnCode(' close #'&FileLay$&':')
       fnReturnCode(' ')
    end if
 return

 ! *******************************************
 !              READ LOOP ONLY
 ! *******************************************
 ReadLoopOnly: ! Template for a standard Read Loop and dim statement

    ! Dim at next largest multiple of 255
    longestElement=(int((LongestElement-1)/255)+1)*255

    fnReturnCode(' ')
    fnReturnCode(' ! Position to start of file')
    fnReturnCode(' restore #'&FileLay$&': error IGNORE')
    fnReturnCode(' ')
    fnReturnCode(' do until File('&FileLay$&')')
    fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
    fnReturnCode('    if File('&FileLay$&')=0 then')
    fnReturnCode('       ! Found Something')
    fnReturnCode(' ')
    fnReturnCode(' ')
    fnReturnCode(' ')
    fnReturnCode('    end if')
    fnReturnCode(' loop')
    fnReturnCode(' ')

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
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          codeLine$=CodeLine$(1:len(CodeLine$)-2)
          fnReturnCode(CodeLine$)
       end if

       fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       fnReturnCode(' ')
       fnReturnCode(' ! Initialize Key Information Here')
       fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       fnReturnCode(' ')
       fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ! Position by key')
       fnReturnCode(' restore #'&FileLay$&', search>=rtrm$('&FileLay$&'Key$): nokey IGNORE')
       fnReturnCode(' ')
       fnReturnCode(' do until File('&FileLay$&')')
       fnReturnCode('    read #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$&' eof IGNORE')
       fnReturnCode('    let This'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode('    if File('&FileLay$&')=0 And rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$))) then')
       fnReturnCode('       ! Found Something')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode('    end if')
       fnReturnCode(' loop while rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$)))')
       fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                OPEN THE FILE
 ! *******************************************
 OpenFile: ! Template for Opening the file

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256

    fnReturnCode(' ')
    fnReturnCode(' ! FileIO Dimensions')
    fnReturnCode(' dim Form$(1)*255')
    fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    fnReturnCode(' ')
    fnReturnCode(' ! Open the file')
    fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$)')
    fnReturnCode(' ')
 return


 ! *******************************************
 !               WRITE CODE KEY
 ! *******************************************
 WriteCodeKey: ! Template for a keyed writing and dim statements

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       let NumberList$(Index)=str$(Index)
    next Index

    let KeyGiven=0
    let KeyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! FileIO Dimensions')
       fnReturnCode(' dim Form$(1)*255')
       fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       let KeyLength=0
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          codeLine$=CodeLine$(1:len(CodeLine$)-2)
          fnReturnCode(CodeLine$)
       end if

       fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       fnReturnCode(' ')
       fnReturnCode(' ! Initialize Key Information Here')
       fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       fnReturnCode(' ')
       fnReturnCode(' ! Open the file')
       fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$,1,KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode(' read #'&FileLay$&', using Form$('&FileLay$&'), key='&FileLay$&'Key$ : Mat '&FileLay$&'$,Mat '&FileLay$&' eof Ignore')
       fnReturnCode(' if File('&FileLay$&')=0 And '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&') then')
       fnReturnCode('    ! Found It: Set Values')

       for Index=1 to udim(mat SSubs$)
          fnReturnCode('    let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
       next Index
       for Index=1 to udim(mat NSubs$)
          fnReturnCode('    let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
       next Index

       fnReturnCode(' ')
       fnReturnCode('    ! Save The Record')
       fnReturnCode('    rewrite #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
       fnReturnCode(' end if')
       fnReturnCode(' ')
       fnReturnCode(' ! Close DataFile')
       fnReturnCode(' let fnCloseFile('&FileLay$&',"'&FileLay$&'")')
       fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                WRITE CODE
 ! *******************************************
 WriteCode: ! Template for writing and dim statements

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256

    fnReturnCode(' ')
    fnReturnCode(' ! FileIO Dimensions')
    fnReturnCode(' dim Form$(1)*255')
    fnReturnCode(' dim '&FileLay$&', '&FileLay$&'$(1)*'&str$(LongestElement)&','&FileLay$&'(1)')
    fnReturnCode(' ')
    fnReturnCode(' ! Open the file')
    fnReturnCode(' let '&FileLay$&'=Fnopen("'&FileLay$&'",Mat '&FileLay$&'$,Mat '&FileLay$&',Mat Form$)')
    fnReturnCode(' ')
    fnReturnCode(' ! Set Values')

    for Index=1 to udim(mat SSubs$)
       fnReturnCode(' let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode(' let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
    next Index

    fnReturnCode(' ')
    fnReturnCode(' ! Save The Record')
    fnReturnCode(' write #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
    fnReturnCode(' ')
    fnReturnCode(' ! Close DataFile')
    fnReturnCode(' let fnCloseFile('&FileLay$&',"'&FileLay$&'")')
    fnReturnCode(' ')

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
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       let KeyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             let KeyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                let KeyLength+=Length
             end if
          end if
       next Index
       if len(CodeLine$)>6 then
          codeLine$=CodeLine$(1:len(CodeLine$)-2)
          fnReturnCode(CodeLine$)
       end if

       fnReturnCode(' dim '&FileLay$&'Key$*'&str$(KeyLength)&', This'&FileLay$&'$*'&str$(KeyLength))
       fnReturnCode(' ')
       fnReturnCode(' ! Initialize Key Information Here')
       fnReturnCode(' let KeyNumber='&str$(KeyGiven))
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&KeyFields$(Index)&'$=')
          else
             fnReturnCode(' let '&KeyFields$(Index)&'=')
          end if
       next Index
       fnReturnCode(' ')
       fnReturnCode(' ! Calculate the Key')

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             fnReturnCode(' let '&FileLay$&'$('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index)&'$')
          else
             fnReturnCode(' let '&FileLay$&'('&Prefix$&KeyFields$(Index)&')='&KeyFields$(Index))
          end if
       next Index

       fnReturnCode(' let '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode(' read #'&FileLay$&', using Form$('&FileLay$&'), key='&FileLay$&'Key$ : Mat '&FileLay$&'$,Mat '&FileLay$&' eof Ignore')
       fnReturnCode(' if File('&FileLay$&')=0 And '&FileLay$&'Key$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&') then')
       fnReturnCode('    ! Found It: Set Values')

       for Index=1 to udim(mat SSubs$)
          fnReturnCode('    let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
       next Index
       for Index=1 to udim(mat NSubs$)
          fnReturnCode('    let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
       next Index

       fnReturnCode(' ')
       fnReturnCode('    ! Save The Record')
       fnReturnCode('    rewrite #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
       fnReturnCode(' end if')
       fnReturnCode(' ')
    end if
 return


 ! *******************************************
 !                WRITE CODE ONLY
 ! *******************************************
 WriteCodeOnly: ! Template for writing and dim statements
    fnReturnCode(' ')
    fnReturnCode(' ! Set Values')

    for Index=1 to udim(mat SSubs$)
       fnReturnCode(' let '&FileLay$&"$("&Prefix$&Ssubs$(Index)&')=')
    next Index
    for Index=1 to udim(mat NSubs$)
       fnReturnCode(' let '&FileLay$&"("&Prefix$&Nsubs$(Index)&')=')
    next Index

    fnReturnCode(' ')
    fnReturnCode(' ! Save The Record')
    fnReturnCode(' write #'&FileLay$&', using Form$('&FileLay$&') : Mat '&FileLay$&'$,Mat '&FileLay$)
    fnReturnCode(' ')
 return

 ! *******************************************
 !               OPEN FUNCTION
 ! *******************************************
 OpenFunction: ! Standard FileIO Open Function
    fnReturnCode(' dim LinkageEstablished')
    fnReturnCode(' def fnEstablishLinkage')
    fnReturnCode('    if ~LinkageEstablished then')
    fnReturnCode('       library "fileio" : fnOpenFile,Fnclosefile,Fngetfilenumber,Fnkey$,FnBuildKey$,Fnreadlayoutarrays,Fndoeslayoutexist,Fnreadallkeys,fnReadRelativeDescription$,fnReadRelUnopenedDescription$,fnReadRelUnopenedNumber,fnUpdateFile,fnLog,fnLogArray,fnErrLog,fnReadLayouts,Fnmakeuniquekey$,FnDisplayLength,FnLength,FnReadDescription$,FnReadUnopenedDescription$,fnReadRecordWhere$,fnUniqueKey,fnReadNumber,fnReadUnopenedNumber,fnReadRelativeNumber,fnNotInFile,fnDataCrawler,fnDataEdit')
    fnReturnCode('       library "fileio" : fnMakeSubProc,fnReadMatchingKeys,fnReadAllNewKeys,fnReadFilterKeys,fnReadEntireLayout,fnReadLayoutHeader,fnReadSubs,fnReadLayoutPath$,fnReadKeyFiles, fnAskCombo$,fnRunProcFile,fnBuildProcFile,fnDataShow')
    fnReturnCode('       library "screenio" : fnCallScreen$,fnFindSubscript,fnFm$,fnfm,fnDisplayScreen,fnGetUniqueName$,fnIsInputSpec,fnIsOutputSpec,fnDays,fnBR42')
    fnReturnCode('       linkageEstablished=1')
    fnReturnCode('    end if')
    fnReturnCode(' fnend')
    fnReturnCode(' !')
    fnReturnCode(' ! #Auton'&'umber# 99000,10')
    fnReturnCode(' OPEN: ! ***** Function To Call Library Openfile And Proc Subs')
    fnReturnCode('       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)')
    fnReturnCode('          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80')
    fnReturnCode('          fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)')
    fnReturnCode('          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : let _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index')
    fnReturnCode('       fnend')
    fnReturnCode('')
    fnReturnCode(' Ignore: Continue')
 return

 dim ReturnCode$*20000
 def fnReturnCode(String$*512)
    let ReturnCode$=ReturnCode$&String$&chr$(13)&chr$(10)
 fnend

 dim RawForm$*10000
 
 def library fnRunTemplate(Template,FileLay$;___,Index)
    fnEstablishLinkage
    let ReturnCode$=""
    fnReadEntireLayout(FileLay$,Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
    let RawForm$=fnReadForm$(FileLay$)
    longestElement=0
    for Index=1 to udim(mat SSpec$)
       longestElement=max(LongestElement,fnLength(SSpec$(Index)))
    next Index
    for Index=1 to udim(mat NSpec$)
       longestElement=max(LongestElement,fnLength(NSpec$(Index)))
    next Index
    for Index=1 to udim(mat SSubs$)
       sSubs$(Index)=lwrc$(SSubs$(Index))
    next Index
    for Index=1 to udim(mat NSubs$)
       let NSubs$(Index)=lwrc$(NSubs$(Index))
    next Index
    let Prefix$=trim$(Prefix$)
    gosub RunTemplate

    if len(ReturnCode$) then let setenv("CLIPBOARD",ReturnCode$)
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
       linkageEstablished=1
    end if
 fnend
 !
 ! #Autonumber# 99000,10
 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)
          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80
          fnopen=Fnopenfile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)
          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : let _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
       fnend

 Ignore: Continue