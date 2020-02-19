00010 ! Replace S:\Core\brs2br
00020 !
00030   library 'S:\Core\Library': fngetdir,fnerror
00040   on error goto Ertn
00050 !
00060   dim dir$*255,filename$(999)*60
00070 !
00080   pr ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
00090   pr ' * WARNING:  This program is about to convert all (*.brs) source'
00100   pr ' *           code files into (*.br) working program files.      '
00110   pr ' *                (including subdirectories)                    '
00120   pr ' *           Continue [Enter]             Cancel[Esc]           '
00130   pr ' *                                                              '
00140   pr ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
00150   input fields "22,1,Cu 1,AE,N": warn$
00160   if cmdkey=5 or cmdkey=99 or warn$="Q" or warn$="X" or warn$="N" then !:
          goto XIT
00170 !
00180   rpl$='Save'
00190   if uprc$(rpl$)<>uprc$("SAVE") and uprc$(rpl$)<>uprc$("Replace") then !:
          rpl$="Replace" ! default to Replace unless correctly specified
00200   if dir$="" then !:
          dir$="\\DISKSTATION\public\ACS\acs.402\" !:
          pr "Directory not set default used (Default Dir: "&dir$&" )"
00210   if dir$(len(dir$):len(dir$))="\" then dir$=dir$(1:len(dir$)-1)
00220   if exists(dir$)=0 then !:
          pr "Directory not found..." !:
          goto XIT
00230   fngetdir(dir$,mat filename$)
00240 ! __
00250   for tmp=1 to udim(filename$)
00260     if rtrm$(filename$(tmp))="" then goto L280
00270   next tmp
00280 L280: mat filename$(tmp)
00290   pr 'Files Found = '&str$(tmp)
00300 ! __
00310   open #20: "Name=Proc."&wsid$&",Size=0,Replace",display,output 
00320 ! __
00330   for j=1 to udim(filename$)
00340     if filename$(j)="" then goto AFT_LOOP1
00350     tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j)))) !:
          if tmpa$<>uprc$(".brs") then goto LOOP1_NEXT
00360     pr #20: ' ! Begin process of file ( '&filename$(j)&' ) '
00370     pr #20: ' ! '
00380     pr #20: "Load "&dir$&"\"&filename$(j)&",Source"
00390     pr #20: ' ! '
00400     pr #20: rpl$&" "&dir$&"\"&filename$(j)(1:len(filename$(j))-1) !:
          ! removes the "s" from the ext changing .brs ext to .br
00410     pr #20: ' ! '
00420     pr #20: ' ! Completed update from source: '&filename$(j)
00430     pr #20: ' ! '
00440 LOOP1_NEXT: ! 
00450   next j
00460 ! __
00470 AFT_LOOP1: ! 
00480   mat filename$(999)
00490   pr #20: " ! Now you've done it.  You've gone and updated all your"
00500   pr #20: "Load S:\Core\br2brs"
00510   pr #20: " ! Now you've done it.  You've gone and updated all your"
00520   pr #20: " ! Now you've done it.  You've gone and updated all your"
00530   pr #20: " ! program files (*.br) from source code files (*.brs)"
00540   close #20: 
00550   chain "proc=proc."&wsid$
00560 !
00570 XIT: stop 
00580 !
00590 ! <Updateable Region: ERTN>
00600 ERTN: fnerror(program$,err,line,act$,"xit")
00610   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00630   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00640 ERTN_EXEC_ACT: execute act$ : goto ERTN
00650 ! /region
00660 !
