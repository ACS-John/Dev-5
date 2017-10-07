10100   pr border: 'Import Source'
10200   execute 'con gui off'
10300   fnupdatesource
10400   def fndatetime
10500     dim tm$*8
10600     tm$=time$
10700     fndatetime=val(date$("CCYYMMDD")&tm$(1:2)&tm$(4:5))
10800   fnend 
10900   def fnfiledatetime(filename$*255)
11000     dim infoline$*255,hh$*2
11100     execute "sy -M dir /N "&filename$&" >"&os_filename$('S:\(import)\fileinfo')
11200     open #fileinfo:=21: "Name=S:\(import)\fileinfo",display,input 
11300     do 
11400       linput #fileinfo: infoline$ eof NODATE
11500       if infoline$(3:3)="/" then goto PARSEDATE
11600     loop 
11700 PARSEDATE: ! 
11800     hh=val(infoline$(13:14))
11900     if infoline$(19:20)="PM" then hh+=12
12000     if hh<10 then hh$="0"&str$(hh) else hh$=str$(hh)
12100     fnfiledatetime=val(infoline$(7:10)&infoline$(1:2)&infoline$(4:5)&hh$&infoline$(16:17))
12200     goto GOTDATE
12300 NODATE: ! 
12400     fnfiledatetime=190001010800
12500 GOTDATE: ! 
12600     close #fileinfo,free: ioerr ignore
12700   fnend 
12800   def fninitupdate(&lastcompile)
12900     dim lasttime$*256
13100     if lasttime$="" then lastcompile=190001010800 else lastcompile=val(lasttime$)
13200     curtime=fndatetime
13500     if not exists("S:\(import)") then execute "sy -M md "&os_filename$("S:\(import)")
13600     open #proc_file:=1: 'Name=S:\(import)\compile.prc,RecL=1024,Replace',display,output 
13700   fnend 
13800   def fnupdatesource
13900     dim filename$*255,msr_file$*255
14000     fninitupdate(lastcompile)
14100     execute "sy -M sortfiles -D . -C "".br.brs|.br""" ioerr ROLLBACK
14200     open #dirfile:=20: "Name=S:\(import)\brsfiles",display,input 
14250     pr #proc_file: 'Let Scr_Freeze'
14300     do 
14400       linput #dirfile: filename$ eof DONE
14500       pr #proc_file: 'Load "'&filename$&'",Source'
14520       parameter$=fn_build_parameter$(filename$)
14600       if exists(filename$(1:len(filename$)-4)) then 
14700         pr #proc_file: 'Replace "'&filename$(1:len(filename$)-4)&'"'&parameter$
14800       else 
14900         pr #proc_file: 'Save "'&filename$(1:len(filename$)-4)&'"'&parameter$
15000       end if 
15100       pr #proc_file: ''
15200     loop 
15250     pr #proc_file: 'let Scr_Thaw'
15300     goto DONE
15400 ROLLBACK: ! 
15600 DONE: ! 
15700     if env$("AfterRecompile")="" then 
15800       pr #proc_file: "Sy"
15900     else 
16000       pr #proc_file: 'chain "'&env$("AfterRecompile")&'"'
16100     end if 
16200     close #dirfile: ioerr ignore
16300     msr_file$=file$(proc_file)
16400     close #proc_file: 
16500     execute "subproc "&msr_file$
16600   fnend 
16700   goto ENDPROGRAM
22000 def fn_build_parameter$(filename$*256)
22020   bp_gets_object=0
22040   filename$=lwrc$(filename$)
22060   if pos(filename$,'_s1.brs')>0 then   ! only files that end with _s1.brs can not be distributed as object
22080     bp_gets_object=0  
22100   else if pos(filename$,'Core\client.br')>0 then 
22120     bp_gets_object=1
22140   else if pos(filename$,'Core\programs\update.br')>0 then 
22160     bp_gets_object=1
22180   end if
22200   if bp_gets_object then
22220     fn_build_parameter$=',object'
22240   end if
22260 fnend
26800 IGNORE: continue 
26900 ENDPROGRAM: ! 
30000 def fnLexiLineNum
30010     dim String$*4000 ! #Autonumber# 10,10
30015     dim Infile$*255
30020     dim Const$(1)*800
30030     dim Constname$(1)*30
30040     dim Currentselect$*400
30050     dim Currentcase$(1)*400
30060     dim AfterString$*4000
30070     let Increment=1
30080     let Labelincrement=10
30090     mat Constname$(0)
30100     mat Const$(0)
30110     open #1: "name="&Infile$, display, input
30120     open #2: "name="&Outfile$&", recl=800, replace", display, output
30130  READLINE: linput #1: String$ eof DONEREADING
30135     if not SkipNextOne and (ltrm$(String$)(1:1)="!" and pos(String$,"!")>3) then let String$(1:4)=" ! ."&ltrm$(string$(1:4))
30140     for Constindex=1 to Udim(Mat Const$)
30150        if (Constantposition:=Pos(Uprc$(String$),Uprc$(Constname$(Constindex)))) then
30160           let String$=String$(1:Constantposition-1) & Const$(Constindex) & String$(Constantposition+Len(Constname$(Constindex)):Len(String$))
30170        END IF  ! end if
30180     next Constindex
30190     if (Constantposition:=Pos(Uprc$(String$),"#DEFINE#")) then
30200        let Constantposition+=8
30210        if (Constnamestartpos:=Pos(String$,"[[",Constantposition)) then
30220           if (Constnameendpos:=Pos(String$,"]]",Constnamestartpos)) then
30230              let Constnameendpos+=1
30240              mat Const$(Constindex:=(Udim(Mat Const$)+1))
30250              mat Constname$(Constindex)
30260              let Constname$(Constindex)=String$(Constnamestartpos:Constnameendpos)
30270              let Const$(Constindex)=Trim$(String$(Constnameendpos+2:Len(String$)))
30280              if Const$(Constindex)(1:1)="=" then ! If Equals, Then Ignore It
30290                 let Const$(Constindex)=Trim$(Const$(Constindex)(2:Len(Const$(Constindex))))
30300              END IF  ! end if
30310              if Const$(Constindex)(1:1)='"' And Const$(Constindex)(Len(Const$(Constindex)):Len(Const$(Constindex)))='"' then
30320                 let Const$(Constindex)=Const$(Constindex)(2:Len(Const$(Constindex))-1) ! Remove Quotes If Both Are Present
30330              END IF  ! end if
30340           END IF  ! end if
30350        END IF  ! end if
30360     END IF  ! end if
30370     if (Selectposition:=Pos(Uprc$(String$),"#SELECT#")) then
30380        if (Caseposition:=Pos(Uprc$(String$),"#CASE#",Selectposition)) then
30390           let Currentselect$=String$(Selectposition+8:Caseposition-1)
30400           let Caseindex=0
30410           let Currentcasechunk=Caseposition+6
30420           do
30430              let Caseindex+=1
30440              mat Currentcase$(Caseindex)
30450              if (Nextcasechunk:=Pos(String$,"#",Currentcasechunk)) then
30460                 let Currentcase$(Caseindex)=String$(Currentcasechunk:Nextcasechunk-1)
30470                 let Currentcasechunk=Nextcasechunk+1
30480              else
30490                 let Currentcase$(Caseindex)=String$(Currentcasechunk:Len(String$))
30500              END IF  ! end if
30510           loop While Nextcasechunk
30520           let Afterstring$=" THEN  ! " & String$(SelectPosition:Len(String$))
30530           let String$=String$(1:SelectPosition-1) & "IF "
30540           for Caseindex=1 to Udim(Mat Currentcase$)
30550              if Caseindex>1 then
30560                 let String$=String$ & " or "
30570              END IF  ! end if
30580              let String$=String$ & Trim$(Currentselect$) & " = " & Trim$(Currentcase$(Caseindex))
30590           next Caseindex
30600           let String$ = String$ & Afterstring$
30610        END IF  ! end if
30620     else if (Caseposition:=Pos(Uprc$(String$),"#CASE#")) then
30630        if Len(Trim$(Currentselect$)) then
30640           let Caseindex=0
30650           let Currentcasechunk=Caseposition+6
30660           do
30670              let Caseindex+=1
30680              mat Currentcase$(Caseindex)
30690              if (Nextcasechunk:=Pos(String$,"#",Currentcasechunk)) then
30700                 let Currentcase$(Caseindex)=String$(Currentcasechunk:Nextcasechunk-1)
30710                 let Currentcasechunk=Nextcasechunk+1
30720              else
30730                 let Currentcase$(Caseindex)=String$(Currentcasechunk:Len(String$))
30740              END IF  ! end if
30750           loop While Nextcasechunk
30760           let Afterstring$=" THEN  ! " & String$(Caseposition:Len(String$))
30770           let String$=String$(1:Caseposition-1) & "ELSE IF "
30780           for Caseindex=1 to Udim(Mat Currentcase$)
30790              if Caseindex>1 then
30800                 let String$=String$ & " or "
30810              END IF  ! end if
30820              let String$=String$ & Trim$(Currentselect$) & " = " & Trim$(Currentcase$(Caseindex))
30830           next Caseindex
30840           let String$ = String$ & Afterstring$
30850        END IF  ! end if
30860     else if (Caseposition:=Pos(Uprc$(String$),"#CASE ELSE#")) then
30870        if Len(Trim$(Currentselect$)) then
30880           let String$ = String$(1:Caseposition-1) & "ELSE " & String$(Caseposition+11:Len(String$)) & " ! " & String$(Caseposition:Len(String$))
30890        END IF  ! end if
30900     else if (Endposition:=Pos(Uprc$(String$),"#END SELECT#")) then
30910        let String$ = String$(1:EndPosition-1) & "END IF" & String$(EndPosition+12:len(String$)) & "  ! " & String$(EndPosition:len(String$))
30920        let Currentselect$ = ""
30930     END IF  ! end if
30940     if (Newnumber:=Pos(Uprc$(String$),"#AUTONUMBER#")) then
30950        let Temp=0
30960        let Temp=Val(String$(Newnumber+12:Newincrement:=Pos(String$,",",Newnumber+12))) conv BADAUTONUMBER
30970        if Temp=0 then goto BADAUTONUMBER
30980        let Newlinecount=Temp
30990        if Newlinecount<=Linecount then print "AUTONUMBER ERROR IN "&Str$(Lastlinecount)&" TO "&Str$(Newlinecount)&" AUTONUMBER SECTION" : close #1: : close #2: : execute ("*FREE "&Outfile$) : print Bell : pause : execute ("SYSTEM")
31000        let Lastlinecount=Linecount=Newlinecount
31010        let Increment=Val(String$(Newincrement+1:4000)) conv BADAUTONUMBER
31020        let Linecount-=Increment ! Decrement So Next Increment Is Correct
31030     END IF  ! end if
31040     if (Ltrm$(Uprc$(String$))(1:1)="L") And (Newnumber:=Pos(Ltrm$(Uprc$(String$))(1:7),":")) then
31050        let Newlinecount=Val(Ltrm$(Uprc$(String$))(2:Newnumber-1)) conv BADAUTONUMBER
31060        if (Newlinecount>Linecount) then
31070           let Linecount=Newlinecount
31080           if Mod(Linecount,Labelincrement)=0 then let Increment=Labelincrement
31090           let Linecount-=Increment ! Decrement So Next Num Is Correct
31100        else
31110           let Increment=Max(Int(Increment/2),2) ! Cut Incr In Half To Catch Up
31120        END IF  ! end if
31130     END IF  ! end if
31140  BADAUTONUMBER: ! Ignore Line Number Information
31150     let X=0
31160     let X = Val(String$(1:5)) conv ADDLINENUMBER
31170     if X>0 then goto PRINTLINE
31180  ADDLINENUMBER: !
31190     if Not Skipnextone then
31200        if Trim$(String$)="" then
31210           let String$=Cnvrt$("PIC(#####)",(Linecount:=Linecount+Increment)) & "  !"
31220        else
31230           let String$=Cnvrt$("PIC(#####)",(Linecount:=Linecount+Increment)) & " " & String$
31240        END IF  ! end if
31250     else
31260        let String$="      "&String$
31270        let Skipnextone=0
31280     END IF  ! end if
31290  PRINTLINE: if Trim$(String$)(Len(Trim$(String$))-1:Len(Trim$(String$))) = "!:" then let Skipnextone=1
31300     print #2: String$
31310     goto READLINE
31320  DONEREADING: close #2:
31330     close #1: