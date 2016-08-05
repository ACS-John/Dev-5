20000 ! Replace R:\Core\WriteINI.br
20200 ! ______________________________________________________________________
20400   def library fnwriteini(key$*80,value$*80; section$*80,inifile$*256,inifile,temp_filenumber)
20600 ! ______________________________________________________________________
20800 ! ** Documentation **
21000 ! Key$       Part before = sign, used like a varible name
21200 !            Not Case Sensitive, will be Trimmed
21400 ! Value$     Part after = sign, used like a varible value
21600 !            Case will be maintained, but it will be Trimmed
21800 ! Section$   speifies that you're looking for the key within the
22000 !            specified section (''=ignore sections, use 1st key found)
22200 !            example ini line= [Screen Settings]
22400 !            example section$ value="screen settings"
22600 ! inifile    filenumber to use if other than 99
22800 ! ______________________________________________________________________
23000     library 'R:\Core\Library': fnpause,fnprg,fngethandle
23200     on error goto ERTN
23400 ! ________________________________________________
23600 ! ** dimensions and varible explanations **
23800     dim temp$*100,prg$*256,udf$*256
24000     dim current_section$*80 ! currently reading in this section
24200     dim current_key$*80 ! currently reading this key
24400     let true=1 : let false=-1
24600 ! ________________________________________________
24800 ! ** set constants and format passed values **
25000     let inifile$=trim$(inifile$)
25200     let section$=lwrc$(trim$(section$))
25400     let key$=lwrc$(trim$(key$))
25600     let done_my_thing=false
25800     if inifile$='' then let udf$=env$('temp')&'\' : let fnprg(prg$,get=1) : let inifile$=udf$&'ini\'&prg$&'.ini' ! let inifile$='Data\Win6.ini'
26000 ! if inifile=0 then let inifile=99
26200 ! if temp_filenumber=0 then let temp_filenumber=99
26400     open #inifile:=fngethandle: "Name="&inifile$&",Shr",display,input ioerr OPEN_INI_ERR
26600     open #temp_filenumber:=fngethandle: "Name="&inifile$&".tmp,RecL=256,Replace",display,output 
26800     do 
27000       linput #inifile: temp$ eof INIFILE_EOF
27200       if ~done_my_thing=true then 
27400 ! 
27600         if temp$(1:1)="[" then let current_section$=lwrc$(temp$(2:len(temp$)-1))
27800 ! 
28000         if section$<>'' and section$=current_section$ then let in_the_right_section=true
28200 ! 
28400         let current_key$=lwrc$(trim$(temp$(1:pos(temp$,'=')-1)))
28600 ! 
28800         if current_key$=key$ then let in_the_right_key=true
29000         if in_the_right_section=true and in_the_right_key=true then 
29200           let temp$=current_key$&"="&value$
29400           let done_my_thing=true
29600         end if  ! in_the_right_section=true and in_the_right_key=true
29800 ! 
30000       end if  ! ~done_my_thing=true
30200       print #temp_filenumber: temp$
30400     loop 
30500 INIFILE_EOF: ! 
30600     close #temp_filenumber: 
30800     close #inifile: 
31000     execute "copy "&inifile$&".tmp "&inifile$
31100     execute "free "&inifile$&".tmp"
31200     goto XF
31400 ! ______________________________________________________________________
31600 FAILURE: ! 
31800     print "FAILURE" ! XXX
32000     goto XIT
32200 ! ______________________________________________________________________
32400 OPEN_INI_ERR: ! 
32600     if err=4152 then 
32800       open #inifile: "Name="&inifile$&",RecL=256,new",display,output 
33000       let temp$=key$&"="&value$
33200       print #inifile: temp$
33400       goto XF
33600     else 
33800       goto ERTN
34000     end if  ! err=4152
34200     goto XF
34400 ! ______________________________________________________________________
34600 XIT: ! 
34800     print 'you had an error in READINI.br and choose to quit'
35000     print 'you are missing the setting for '&key$
35200     let fnpause
35400     goto XF
35600 ! ______________________________________________________________________
35800 ERTN: ! 
36000     print "ERROR "&str$(err)
36200     print "LINE  "&str$(line)
36400     let fnpause
36600 ! 
36800 ! 
37000 ! ______________________________________________________________________
37200 XF: ! 
37400     close #inifile: ioerr XF_NOW
37500 XF_NOW: ! 
37600   fnend 
