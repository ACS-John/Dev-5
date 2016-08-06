00010 ! Replace R:\Core\ReadINI.br
00020 ! ______________________________________________________________________
10000   def library fnread_program_property(key$*80,&value$; section$*80,inifile$)
10020 ! ______________________________________________________________________
10040 ! r: ** Documentation **
10060 ! Key$       Part before = sign, used like a variable name
10080 !            Not Case Sensitive, will be Trimmed
10100 ! Value$     Part after = sign, used like a variable value
10120 !            Case will be maintained, but it will be Trimmed
10140 ! Section$   specifies that you're looking for the key within the
10160 !            specified section (''=ignore sections, use 1st key found)
10180 !            example ini line= [Screen Settings]
10200 !            example section$ value="screen settings"
18000 ! /r
18020     library 'R:\Core\Library': fnprg,fngethandle,fnerror,fnpause,fnprogram_ini_filename$
18040     on error goto ERTN
18060 ! ________________________________________________
18080 ! ** dimensions and varible explanations **
18100     dim temp$*100
18120     dim current_section$*80 ! currently reading in this section
18140     dim current_key$*80 ! currently reading this key
18160     dim myinifile$*256,prg$*256
18180 ! ________________________________________________
18200 ! ** set constants and format passed values **
18220     let prg$=env$('Core_Program_Current')
18240     let len_prg=len(prg$)
18260     if lwrc$(prg$(len_prg-2:len_prg))='.br' then let prg$(len_prg-2:len_prg)='' ! remove the .br ending, if it is there
20240     if lwrc$(trim$(prg$))=lwrc$("acspr\checkfile") then let prg$="acspr\checkhistory"
20260 ! let udf$=env$('temp')&'\'
20280     let inifile$=trim$(inifile$)
20300     let section$=lwrc$(trim$(section$))
20320     let key$=lwrc$(trim$(key$))
20340 ! 
40000     if inifile$='' then 
40020       let myinifile$=fnprogram_ini_filename$(prg$) ! udf$&'ini\'&prg$&'.ini'
40040     else 
40060       let myinifile$=inifile$
40080     end if 
40100     if ~exists(myinifile$) then 
40120       let myinifile$="R:\Core\Default\Program.ini"
40140     end if 
50000     open #inifile:=fngethandle: "Name="&myinifile$&",Shr",display,input 
50020 READ_INIFILE: ! 
50040     linput #inifile: temp$ eof XF
50060 ! 
50080     if temp$(1:1)="[" then 
50100       let current_section$=lwrc$(temp$(2:len(temp$)-1))
50120     end if 
50140 ! 
50160     if section$<>'' and section$<>current_section$ then 
50180       goto READ_INIFILE ! section was specified and you're in the wrong one
50200     end if 
50220 ! 
50240     let current_key$=lwrc$(trim$(temp$(1:pos(temp$,'=')-1)))
50260 ! 
50280     if current_key$=key$ then 
50300       let value$=trim$(temp$(pos(temp$,'=')+1:len(temp$)))
50340     else 
50360       goto READ_INIFILE ! wrong key, try again
50380     end if 
64020     goto XF
64040 ! ______________________________________________________________________
66000 XIT: ! 
66020     print bell;'you had an error in fnread_program_property (READINI.br)'
66040     print 'you are missing the setting for '&key$
66060     let fnpause
66080     goto XF
68000 ! <Updateable Region: ERTN>
68020 ERTN: let fnerror(cap$,err,line,act$,"xit")
68040     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
68060     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
68080     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
68100 ERTN_EXEC_ACT: execute act$ : goto ERTN
68120 ! /region
70000 IGNORE: continue 
70020 XF: ! 
70040     close #inifile: ioerr ignore
90000   fnend 
