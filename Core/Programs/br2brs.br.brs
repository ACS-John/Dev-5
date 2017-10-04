00010 ! Replace S:\Core\br2brs
00020 ! creates a brs file for every br file
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fngetdir,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dir$*255,filename$(999)*255
00080 ! ______________________________________________________________________
00090   if dir$="" then !:
          let dir$="\\DISKSTATION\public\ACS\acs.402" !:
          pr "Directory not set default used (Default Dir: "&dir$&" )"
00100   if dir$(len(dir$):len(dir$))="\" then let dir$=dir$(1:len(dir$)-1)
00110   let option$="/s /a-d /on" ! widows xp ... /s   - include sub dirs. !:
        ! .                                       /a-d - exclude dir. names !:
        ! .                                       /on  - order by name
00120   fngetdir(dir$,mat filename$,option$)
00130 ! __
00140   for tmp=1 to udim(filename$)
00150     if rtrm$(filename$(tmp))="" then goto L170
00160   next tmp
00170 L170: mat filename$(tmp)
00180   pr ' =)   Files Found = '&str$(tmp)
00190 ! __
00200   open #20: "Name=Proc."&wsid$&",Size=0,Replace",display,output 
00210 ! __
00220   for j=1 to udim(filename$)
00230 VALID_FILE_TEST: ! 
00240     if filename$(j)="" then goto AFT_LOOP1
00250     let tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j)))) !:
          if tmpa$=uprc$(".FIX") then goto VALID_FILE_TEST_PASS else !:
            if tmpa$=uprc$(".CNV") then goto VALID_FILE_TEST_PASS else !:
              if tmpa$=uprc$(".br") then goto VALID_FILE_TEST_PASS
00260     let tmpa$=uprc$(filename$(j)(len(filename$(j))-2:len(filename$(j)))) !:
          if tmpa$=uprc$(".br") then goto VALID_FILE_TEST_PASS
00270 VALID_FILE_TEST_FAIL: ! 
00280     goto LOOP1_NEXT
00290 VALID_FILE_TEST_PASS: ! 
00300     pr #20: "Load "&filename$(j)
00310     pr #20: "List >"&filename$(j)&"s" ! ...br + s = ...brs
00320     pr #20: " ! file created: "&filename$(j)&"s"
00330 LOOP1_NEXT: ! 
00340   next j
00350 ! __
00360 AFT_LOOP1: ! 
00370   mat filename$(999)
00380   pr #20: "load S:\Core\br2brs"
00390   pr #20: " ! 'Now all your *.br files have little *.brs parallels'"
00400   close #20: 
00410   chain "proc=proc."&wsid$
00420 ! ______________________________________________________________________
00430 XIT: stop 
00440 ! ______________________________________________________________________
00450 ! <Updateable Region: ERTN>
00460 ERTN: let fnerror(program$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 ! ______________________________________________________________________
