20000 ! Replace S:\acsUB\ubUnPen
20020 ! Undo Penalty Calculation
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fnlbl,fnxit,fntxt,fnacs,fnerror,fnwait,fntos,fncmdset,fntop,fnAutomatedSavePoint,fnget_services
20080   on error goto ERTN
20100 ! 
20120   dim o(2),z$*10,g(12),adr(2),e$(4)*30,cap$*128,txt$*40,tg(11),pen(10)
20140   dim servicename$(10)*20,servicecode$(10)*2,tax_code$(10)*1,pencolumn(10)
20160   dim penatly$(10)*1,gb(10),extra(23),extra$(11)*30,a(7)
20180 ! 
20200   let fntop(program$,cap$="Undo Penalty Calculation")
20220 ! ______________________________________________________________________
20240   let fntos(sn$="ubUnPen") 
20260   let mylen=26 : let mypos=mylen+2
20280   let fnlbl(1,1,"Penalty Date:",mylen,1)
20300   let fntxt(1,mypos,10,0,0,"3") 
20320   let resp$(1)=""
20340   let fnlbl(1,40,"")
20360   let fncmdset(2) 
20380   let fnacs(sn$,win,mat resp$,ckey)
20400   let ubpendat=val(srep$(resp$(1),'/',''))
20420   if ckey=5 then goto XIT
20440   fnAutomatedSavePoint('before')
20460   fnget_services(mat servicename$,mat service$,mat tax_code$,mat penalty$)
20480   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
20500   open #2: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno')&",Shr",internal,outin,keyed 
20520 ! 
20540 do
20560   L280: !
20580   read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof XIT
20600   if tdate=ubpendat and tcode=2 then 
20620     read #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb nokey L280
20640     let bal=bal-tamount
20660     for j=1 to 10
20680       if uprc$(penalty$(j))="Y" then let gb(j)=gb(j)-tg(j) ! subtract penalty breakdown from balance breakdown
20700     next j
20720     rewrite #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb
20740     delete #2: 
20760   end if
20780 loop 
20800 XIT: let fnxit
20820 ! <Updateable Region: ERTN>
20840 ERTN: let fnerror(program$,err,line,act$,"NO")
20860   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20900   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
20920 ERTN_EXEC_ACT: execute act$ : goto ERTN
20940 ! /region
