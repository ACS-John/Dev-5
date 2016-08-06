00010 ! Replace R:\Core\brs2br
00020 ! ______________________________________________________________________
00030   library 'R:\Core\Library': fngetdir,fnerror
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim dir$*255,filename$(999)*60
00070 ! ______________________________________________________________________
00080   print ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
00090   print ' * WARNING:  This program is about to convert all (*.brs) source'
00100   print ' *           code files into (*.br) working program files.      '
00110   print ' *                (including subdirectories)                    '
00120   print ' *           Continue [Enter]             Cancel[Esc]           '
00130   print ' *                                                              '
00140   print ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
00150   input fields "22,1,Cu 1,AE,N": warn$
00160   if cmdkey=5 or cmdkey=99 or warn$="Q" or warn$="X" or warn$="N" then !:
          goto XIT
00170 ! ______________________________________________________________________
00180   let rpl$='Save'
00190   if uprc$(rpl$)<>uprc$("SAVE") and uprc$(rpl$)<>uprc$("Replace") then !:
          let rpl$="Replace" ! default to Replace unless correctly specified
00200   if dir$="" then !:
          let dir$="\\DISKSTATION\public\ACS\acs.402\" !:
          print "Directory not set default used (Default Dir: "&dir$&" )"
00210   if dir$(len(dir$):len(dir$))="\" then let dir$=dir$(1:len(dir$)-1)
00220   if exists(dir$)=0 then !:
          print "Directory not found..." !:
          goto XIT
00230   let fngetdir(dir$,mat filename$)
00240 ! __
00250   for tmp=1 to udim(filename$)
00260     if rtrm$(filename$(tmp))="" then goto L280
00270   next tmp
00280 L280: mat filename$(tmp)
00290   print 'Files Found = '&str$(tmp)
00300 ! __
00310   open #20: "Name=Proc."&wsid$&",Size=0,Replace",display,output 
00320 ! __
00330   for j=1 to udim(filename$)
00340     if filename$(j)="" then goto AFT_LOOP1
00350     let tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j)))) !:
          if tmpa$<>uprc$(".brs") then goto LOOP1_NEXT
00360     print #20: ' ! Begin process of file ( '&filename$(j)&' ) '
00370     print #20: ' ! '
00380     print #20: "Load "&dir$&"\"&filename$(j)&",Source"
00390     print #20: ' ! '
00400     print #20: rpl$&" "&dir$&"\"&filename$(j)(1:len(filename$(j))-1) !:
          ! removes the "s" from the ext changing .brs ext to .br
00410     print #20: ' ! '
00420     print #20: ' ! Completed update from source: '&filename$(j)
00430     print #20: ' ! '
00440 LOOP1_NEXT: ! 
00450   next j
00460 ! __
00470 AFT_LOOP1: ! 
00480   mat filename$(999)
00490   print #20: " ! Now you've done it.  You've gone and updated all your"
00500   print #20: "Load R:\Core\br2brs"
00510   print #20: " ! Now you've done it.  You've gone and updated all your"
00520   print #20: " ! Now you've done it.  You've gone and updated all your"
00530   print #20: " ! program files (*.br) from source code files (*.brs)"
00540   close #20: 
00550   chain "proc=proc."&wsid$
00560 ! ______________________________________________________________________
00570 XIT: stop 
00580 ! ______________________________________________________________________
00590 ! <Updateable Region: ERTN>
00600 ERTN: let fnerror(cap$,err,line,act$,"xit")
00610   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00630   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00640 ERTN_EXEC_ACT: execute act$ : goto ERTN
00650 ! /region
00660 ! ______________________________________________________________________
