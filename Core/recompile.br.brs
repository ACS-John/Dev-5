10100 pr border: 'Import Source'
10200 execute 'con gui off'
10300 fn_updateSource
10400 def fn_dateTime
10500   dim tm$*8
10600   tm$=time$
10700   fn_dateTime=val(date$("CCYYMMDD")&tm$(1:2)&tm$(4:5))
10800 fnend
10900 def fn_fileDateTime(filename$*255)
11000   dim infoline$*255,hh$*2
11100   execute "sy -M dir /N "&filename$&" >"&os_filename$('S:\(import)\fileinfo')
11200   open #fileinfo:=21: "Name=S:\(import)\fileinfo",display,input
11300   do
11400     linput #fileinfo: infoline$ eof NODATE
11500     if infoline$(3:3)="/" then goto PARSEDATE
11600   loop
11700   PARSEDATE: !
11800   hh=val(infoline$(13:14))
11900   if infoline$(19:20)="PM" then hh+=12
12000   if hh<10 then hh$="0"&str$(hh) else hh$=str$(hh)
12100   fn_fileDateTime=val(infoline$(7:10)&infoline$(1:2)&infoline$(4:5)&hh$&infoline$(16:17))
12200   goto GOTDATE
12300   NODATE: !
12400   fn_fileDateTime=190001010800
12500   GOTDATE: !
12600   close #fileinfo,free: ioerr ignore
12700 fnend
12800 def fn_initUpdate(&lastcompile)
12900   dim lasttime$*256
13100   if lasttime$="" then lastcompile=190001010800 else lastcompile=val(lasttime$)
13200   curtime=fn_dateTime
13500   if ~exists("S:\(import)") then execute "sy -M md "&os_filename$("S:\(import)")
13600   open #proc_file:=1: 'Name=S:\(import)\compile.prc,RecL=1024,Replace',display,output
13700 fnend
13800 def fn_updateSource
13900   dim filename$*255,msr_file$*255
14000   fn_initUpdate(lastcompile)
14100   execute "sy -M sortfiles -D . -C "".br.brs|.br""" ioerr DONE
14200   open #dirfile:=20: "Name=S:\(import)\brsfiles",display,input
14250   pr #proc_file: 'Scr_Freeze'
14300   do
14400     linput #dirfile: filename$ eof DONE
14410     if fn_hasLineNumbers(filename$) then
14500       pr #proc_file: 'Load "'&filename$&'",Source'
14520       parameter$=fn_build_parameter$(filename$)
14600       if exists(filename$(1:len(filename$)-4)) then
14700         pr #proc_file: 'Replace "'&filename$(1:len(filename$)-4)&'"'&parameter$
14800       else
14900         pr #proc_file: 'Save "'&filename$(1:len(filename$)-4)&'"'&parameter$
15000       end if
15020     else
15050       exe  'sy ""C:\ACS\Util\Lexi\ConvStoO.cmd" "'&filename$&'"'
15080     end if
15100     pr #proc_file: ''
15200   loop
15250   pr #proc_file: 'Scr_Thaw'
15300   goto DONE
15600   DONE: !
15700   if env$("AfterRecompile")="" then
15800     pr #proc_file: "Sy"
15900   else
16000     pr #proc_file: 'chain "'&env$("AfterRecompile")&'"'
16100   end if
16200   close #dirfile: ioerr ignore
16300   msr_file$=file$(proc_file)
16400   close #proc_file:
16500   execute "subproc "&msr_file$
16600 fnend
16700 goto ENDPROGRAM
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
30000 def fnLexiLineNum(lextFileIn$*256,lextFileOut$*256)
30010   dim string$*4000
30020   dim const$(1)*800
30030   dim constantName$(1)*30
30040   dim currentSelect$*400
30050   dim currentCase$(1)*400
30060   dim afterString$*4000
30070   increment=1
30080   labelIncrement=10
30090   mat constantName$(0)
30100   mat const$(0)
30110   open #1: "name="&lextFileIn$, display, input
30120   open #2: "name="&lextFileOut$&",recl=800,replace",display,output
30130   do
30132     linput #1: string$ eof LexiDoneReading
30135     if ~skipNextOne and (ltrm$(string$)(1:1)="!" and pos(string$,"!")>3) then string$(1:4)=" ! ."&ltrm$(string$(1:4))
30140     for constIndex=1 to udim(Mat const$)
30150       if (constantPosition:=pos(Uprc$(string$),Uprc$(constantName$(constIndex)))) then
30160         string$=string$(1:constantPosition-1)&const$(constIndex)&string$(constantPosition+len(constantName$(constIndex)):len(string$))
30170       end if
30180     next constIndex
30190     if (constantPosition:=pos(Uprc$(string$),"#DEFINE#")) then
30200       constantPosition+=8
30210       if (constNameStartPos:=pos(string$,"[[",constantPosition)) then
30220         if (constNameEndPos:=pos(string$,"]]",constNameStartPos)) then
30230           constNameEndPos+=1
30240           mat const$(constIndex:=(udim(Mat const$)+1))
30250           mat constantName$(constIndex)
30260           constantName$(constIndex)=string$(constNameStartPos:constNameEndPos)
30270           const$(constIndex)=trim$(string$(constNameEndPos+2:len(string$)))
30280           if const$(constIndex)(1:1)="=" then ! If Equals, Then Ignore It
30290              const$(constIndex)=trim$(const$(constIndex)(2:len(const$(constIndex))))
30300           end if
30310           if const$(constIndex)(1:1)='"' and const$(constIndex)(len(const$(constIndex)):len(const$(constIndex)))='"' then
30320             const$(constIndex)=const$(constIndex)(2:len(const$(constIndex))-1) ! Remove Quotes If Both Are Present
30330           end if
30340         end if
30350       end if
30360     end if
30370     if (selectPosition:=pos(lwrc$(string$),"#select#")) then
30380       if (casePosition:=pos(lwrc$(string$),"#case#",selectPosition)) then
30390         currentSelect$=string$(selectPosition+8:casePosition-1)
30400         caseIndex=0
30410         currentCaseChunk=casePosition+6
30420         do
30430           caseIndex+=1
30440           mat currentCase$(caseIndex)
30450           if (nextCaseChunk:=pos(string$,"#",currentCaseChunk)) then
30460             currentCase$(caseIndex)=string$(currentCaseChunk:nextCaseChunk-1)
30470             currentCaseChunk=nextCaseChunk+1
30480           else
30490             currentCase$(caseIndex)=string$(currentCaseChunk:len(string$))
30500           end if
30510         loop while nextCaseChunk
30520         afterString$=" then  ! "&string$(selectPosition:len(string$))
30530         string$=string$(1:selectPosition-1)&"if "
30540         for caseIndex=1 to udim(Mat currentCase$)
30550           if caseIndex>1 then
30560             string$=string$&" or "
30570           end if
30580           string$=string$&trim$(currentSelect$)&"="&trim$(currentCase$(caseIndex))
30590         next caseIndex
30600         string$=string$&afterString$
30610       end if
30620     else if (casePosition:=pos(lwrc$(string$),"#case#")) then
30630        if len(trim$(currentSelect$)) then
30640           caseIndex=0
30650           currentCaseChunk=casePosition+6
30660           do
30670              caseIndex+=1
30680              mat currentCase$(caseIndex)
30690              if (nextCaseChunk:=pos(string$,"#",currentCaseChunk)) then
30700                 currentCase$(caseIndex)=string$(currentCaseChunk:nextCaseChunk-1)
30710                 currentCaseChunk=nextCaseChunk+1
30720              else
30730                 currentCase$(caseIndex)=string$(currentCaseChunk:len(string$))
30740              end if
30750           loop while nextCaseChunk
30760           afterString$=" then  ! "&string$(casePosition:len(string$))
30770           string$=string$(1:casePosition-1)&"else if "
30780           for caseIndex=1 to udim(Mat currentCase$)
30790              if caseIndex>1 then
30800                 string$=string$&" or "
30810              end if
30820              string$=string$&trim$(currentSelect$)&"="&trim$(currentCase$(caseIndex))
30830           next caseIndex
30840           string$=string$&afterString$
30850        end if
30860     else if (casePosition:=pos(lwrc$(string$),"#case else#")) then
30870        if len(trim$(currentSelect$)) then
30880           string$=string$(1:casePosition-1)&"else "&string$(casePosition+11:len(string$))&" ! "&string$(casePosition:len(string$))
30890        end if
30900     else if (Endposition:=pos(lwrc$(string$),"#end select#")) then
30910        string$=string$(1:EndPosition-1)&"end if"&string$(EndPosition+12:len(string$))&"  ! "&string$(EndPosition:len(string$))
30920        currentSelect$=""
30930     end if
30940     if (newNumber:=pos(lwrc$(string$),"#autonumber#")) then
30950       temp=0
30960       temp=val(string$(newNumber+12:newIncrement:=pos(string$,",",newNumber+12))) conv LexiBadAutoNumber
30970       if temp=0 then goto LexiBadAutoNumber
30980       newLineCount=temp
30990       if newLineCount<=lineCount then print "autonumber error in "&Str$(lastLineCount)&" to "&Str$(newLineCount)&" autonumber section" : close #1: : close #2: : execute ("*Free "&lextFileOut$) : print Bell : pause : execute ("System")
31000       lastLineCount=lineCount=newLineCount
31010       increment=val(string$(newIncrement+1:4000)) conv LexiBadAutoNumber
31020       lineCount-=increment ! Decrement So Next increment Is Correct
31030     end if
31040     if (ltrm$(lwrc$(string$))(1:1)="l") and (newNumber:=pos(ltrm$(string$)(1:7),":")) then
31050        newLineCount=val(ltrm$(string$)(2:newNumber-1)) conv LexiBadAutoNumber
31060        if (newLineCount>lineCount) then
31070           lineCount=newLineCount
31080           if mod(lineCount,labelIncrement)=0 then increment=labelIncrement
31090           lineCount-=increment ! Decrement So Next Num Is Correct
31100        else
31110           increment=max(int(increment/2),2) ! Cut Incr In Half To Catch Up
31120        end if
31130     end if
31140     LexiBadAutoNumber: ! Ignore Line Number Information
31150     x=0
31160     x=val(string$(1:5)) conv LexiAddLineNumber
31170     if x>0 then goto LexiPrintLine
31180     LexiAddLineNumber: !
31190     if ~skipNextOne then
31200       if trim$(string$)="" then
31210         string$=cnvrt$("pic(#####)",(lineCount:=lineCount+increment))&"  !"
31220       else
31230         string$=cnvrt$("pic(#####)",(lineCount:=lineCount+increment))&" "&string$
31240       end if
31250     else
31260       string$="      "&string$
31270       skipNextOne=0
31280     end if
31290     LexiPrintLine: !
31292     if trim$(string$)(len(trim$(string$))-1:len(trim$(string$)))="!:" then skipNextOne=1
31300     print #2: string$
31310   loop
31320   LexiDoneReading: !
31322   close #2:
31330   close #1:
32000 fnend
34000 def fn_hasLineNumbers(filename$*256)
34020   filename$=trim$(filename$)
34040   ext$=filename$(pos(filename$,'.',-1):len(filename$)) !  get file identifier
34080   if lwrc$(ext$)='.brs' or lwrc$(ext$)='.wbs' then
34100     open #hTmp:=3: 'name='&filename$,display,input
34120     dim hlnLine$*2048
34140     linput #hTmp: hlnLine$ eof HlnEof
34160     close #hTmp:
34180     !
34200     hlnTest1=0
34220     hlnTest1=val(hlnLine$(1:5)) conv ignore
34240     if hlnTest1>0 then goto HlnYesLineNumbers
34260     !
34280     hlnPosSpace=pos(hlnLine$,' ')
34300     if hlnPosSpace>0 then
34320       hlnTest1=val(hlnLine$(1:hlnPosSpace-1)) conv ignore
34340       if hlnTest1>0 then goto HlnYesLineNumbers
34360     end if
34380     !
34400     goto HlnNoLineNumbers
34420     HlnYesLineNumbers: !
34440       hasLineNumbersReturn=1
34460     goto HlnFinis
34480     HlnNoLineNumbers: !
34500       hasLineNumbersReturn=0
34520     goto HlnFinis
34540     HlnEof: !
34560     pr bell;'HlnEof file analyzed (filename$='&filename$&') had no lines (EoF).' 
34580     pause
34600     goto HlnFinis
34620   end if
34640     HlnFinis: !
34660   fn_hasLineNumbers=hasLineNumbersReturn
34680 fnend
