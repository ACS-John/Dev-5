 ! FileIO Library Basic Addon Template by Gabriel Bakker
 ! Copyright 2012 Gabriel Bakker, Sage AX
 ! Created: 2/6/2012
 !
 ! Use this template library as an example of how to construct other template libraries.
 !
 ! Every Template must have two library functions,
 !  fnTemplateList and fnRunTemplate.
 !
 !  fnTemplateList(mat TemplateDesc$)
 !  takes an array of descriptions and returns a list of the available functions in this template library.
 !
 !  fnRunTemplate(Template,FileLay$)
 !  this function takes the given template Index and file layout and calls your template function.
 !
 !  A template function should read the file layout and then build a string of code and return it
 !  in the clip board so that a programmer can  paste it into his editor of choice later.
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

 SelfRegister: ! r: Template that creates code to self register
 ! this layout if its not found in the layout folder already

    ! Dim at next largest multiple of 255
    longestElement=(int((LongestElement-1)/255)+1)*255

    ! Get it from FileIO program


return ! /r
 Conversion: ! r: Creates conversion functions that can be used
 ! to go between fileio vars and oldskool vars to aid in
 ! implementing fileio in older programs.
 

    ! Dim at next largest multiple of 255
    longestElement=(int((LongestElement-1)/255)+1)*255


    ! Get code from Active Program



return ! /r
 ERtn: ! r: Template for a ACS Error Routine
 let fnReturnCode('! <updateable region: ertn>')
 let fnReturnCode('ERTN: ')
 let fnReturnCode('  fnerror(program$,err,line,act$,"xit")')
 let fnReturnCode('  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT')
 let fnReturnCode('  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT')
 let fnReturnCode('  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT')
 let fnReturnCode('ERTN_EXEC_ACT: execute act$ : goto ERTN')
 let fnReturnCode('! </updateable region: ertn>')
 return ! /r

 ReadLoop: ! r: Template for a standard Read Loop and dim statement

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

 return ! /r
 ReadLoopKey: ! r: Template for a keyed Read Loop and dim statement

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       numberList$(Index)=str$(Index)
    next Index

    keyGiven=0
    keyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
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
       keyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             keyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                keyLength+=Length
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
       fnReturnCode(' keyNumber='&str$(KeyGiven))
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
       fnReturnCode('    this'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
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
 return ! /r
 ReadLoopOnly: ! r: Template for a standard Read Loop and dim statement

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

 return ! /r
 ReadLoopKeyOnly: ! r: Template for a keyed Read Loop and dim statement

    ! Dim at next largest multiple of 256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       numberList$(Index)=str$(Index)
    next Index

    keyGiven=0
    keyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       keyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             keyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                keyLength+=Length
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
       fnReturnCode(' keyNumber='&str$(KeyGiven))
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
       fnReturnCode('    this'&FileLay$&'$=fnBuildKey$("'&FileLay$&'",mat '&FileLay$&'$,mat '&FileLay$&',KeyNumber)')
       fnReturnCode('    if File('&FileLay$&')=0 And rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$))) then')
       fnReturnCode('       ! Found Something')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode(' ')
       fnReturnCode('    end if')
       fnReturnCode(' loop while rtrm$('&FileLay$&'Key$)=This'&FileLay$&'$(1:len(rtrm$('&FileLay$&'Key$)))')
       fnReturnCode(' ')
    end if
 return ! /r

 OpenFile: ! r: Template for Opening the file

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
 return ! /r

 WriteCodeKey: ! r: Template for a keyed writing and dim statements

    ! Dim at next largest multiple of 256
    longestElement=(int((LongestElement-1)/256)+1)*256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       numberList$(Index)=str$(Index)
    next Index

    keyGiven=0
    keyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
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
       keyLength=0
       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             keyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                keyLength+=Length
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
       fnReturnCode(' keyNumber='&str$(KeyGiven))
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
return ! /r

 WriteCode: ! r: Template for writing and dim statements

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

return ! /r
 WriteCodeKeyOnly: ! r: Template for a keyed writing and dim statements

    ! Dim at next largest multiple of 256
    mat NumberList$(udim(mat KeyDescription$))
    for Index=1 to udim(mat NumberList$)
       numberList$(Index)=str$(Index)
    next Index

    keyGiven=0
    keyGiven=val(fnAskcombo$(mat NumberList$,"Select Key")) conv Ignore
    if KeyGiven and KeyGiven<=udim(mat KeyDescription$) then
       str2mat(lwrc$(KeyDescription$(KeyGiven)),mat KeyFields$,"/")

       fnReturnCode(' ')
       fnReturnCode(' ! Variables Used')
       fnReturnCode(' dim KeyNumber')

       codeLine$=" dim "
       keyLength=0

       for Index=1 to udim(Mat KeyFields$)
          sub=srch(mat SSubs$,trim$(KeyFields$(Index)))
          if Sub>0 then
             length=fnLength(SSpec$(Sub))
             codeLine$=CodeLine$&Trim$(KeyFields$(Index))&"$*"&str$(Length)&", "
             keyLength+=Length
          else
             sub=srch(Mat NSubs$,trim$(KeyFields$(Index)))
             if Sub>0 then
                length=fnLength(NSpec$(Sub))
                codeLine$=CodeLine$&Trim$(KeyFields$(Index))&", "
                keyLength+=Length
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
       fnReturnCode(' keyNumber='&str$(KeyGiven))
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
return ! /r
 WriteCodeOnly: ! r: Template for writing and dim statements
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
return ! /r

 OpenFunction: ! r: Standard FileIO Open Function
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
    fnReturnCode('          fnopen=fnOpenFile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)')
    fnReturnCode('          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index')
    fnReturnCode('       fnend')
    fnReturnCode('')
    fnReturnCode(' Ignore: Continue')
return ! /r

 dim ReturnCode$*20000
 def fnReturnCode(String$*512)
    returnCode$=ReturnCode$&String$&chr$(13)&chr$(10)
 fnend
def library fnRunTemplate(Template,FileLay$;___,Index)
  fnEstablishLinkage
  returnCode$=""
  fnReadEntireLayout(FileLay$,Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
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
     nSubs$(Index)=lwrc$(NSubs$(Index))
  next Index
  prefix$=trim$(Prefix$)
  gosub RunTemplate

  if len(ReturnCode$) then let setenv("CLIPBOARD",ReturnCode$)
fnend
 RunTemplate: ! r: Template List gosub statement:
    on Template gosub ReadLoop, ReadLoopKey, ReadLoopOnly, ReadLoopKeyOnly, WriteCode, WriteCodeKey, WriteCodeOnly, WriteCodeKeyOnly, OpenFile, OpenFunction, SelfRegister, Conversion, ERtn
 return ! /r

 TemplateList: ! r: Dictionary listing of available functions
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
    data "Self Register"
    data "Conversion"
    data "ERtn"
! /r
 def library fnTemplateList(mat TemplateDesc$)
    restore TemplateList
    mat TemplateDesc$(11)
    read mat TemplateDesc$
 fnend
  dim LinkageEstablished
 def fnEstablishLinkage
    if ~LinkageEstablished then
       library "S:\Core\FileIO\fileio.br" : fnOpenFile, fnClose, fnReadEntireLayout, fnGetFileNumber, fnGetKeyElements, fnLength, fnAskCombo$
       linkageEstablished=1
    end if
 fnend
 !
 ! #Autonumber# 99000,10
 OPEN: ! ***** Function To Call Library Openfile And Proc Subs
       def Fnopen(Filename$*255, Mat F$, Mat F, Mat Form$; Inputonly, Keynum, Dont_Sort_Subs, Path$*255, Mat Descr$, Mat Field_Widths,Supress_Prompt,Ignore_Errors,___,Index)
          dim _FileIOSubs$(1)*800, _Loadedsubs$(1)*80
          fnopen=fnOpenFile(Filename$, Mat F$, Mat F, Mat Form$, Inputonly, Keynum, Dont_Sort_Subs, Path$, Mat Descr$, Mat Field_Widths, Mat _FileIOSubs$, Supress_Prompt,Ignore_Errors,Program$)
          if Srch(_Loadedsubs$,Uprc$(Filename$))<=0 then : mat _Loadedsubs$(Udim(_Loadedsubs$)+1) : _Loadedsubs$(Udim(_Loadedsubs$))=Uprc$(Filename$) : for Index=1 to Udim(Mat _Fileiosubs$) : execute (_Fileiosubs$(Index)) : next Index
       fnend
 Ignore: Continue