20000 ! Replace S:\acsUB\ubUnPen
20020 ! Undo Penalty Calculation
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fnLbl,fnxit,fnTxt,fnAcs,fnerror,fnwait,fnTos,fnCmdSet,fntop,fnAutomatedSavePoint,fnget_services
20080   on error goto ERTN
20100 ! 
20120   dim o(2),z$*10,g(12),adr(2),e$(4)*30,cap$*128,txt$*40,tg(11),pen(10)
20140   dim serviceName$(10)*20,serviceCode$(10)*2,tax_code$(10)*1,pencolumn(10)
20160   dim penatly$(10)*1,gb(10),extra(23),extra$(11)*30,a(7)
20180 ! 
20200   fntop(program$,cap$="Undo Penalty Calculation")
20220 ! ______________________________________________________________________
20240   fnTos(sn$="ubUnPen") 
20260   mylen=26 : mypos=mylen+2
20280   fnLbl(1,1,"Penalty Date:",mylen,1)
20300   fnTxt(1,mypos,10,0,0,"3") 
20320   resp$(1)=""
20340   fnLbl(1,40,"")
20360   fnCmdSet(2) 
20380   fnAcs(sn$,win,mat resp$,ckey)
20400   ubpendat=val(srep$(resp$(1),'/',''))
20420   if ckey=5 then goto XIT
20440   fnAutomatedSavePoint('before')
20460   fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$)
20480   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
20500   open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",internal,outIn,keyed 
20520 ! 
20540 do
20560   L280: !
20580   read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof XIT
20600   if tdate=ubpendat and tcode=2 then 
20620     read #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb nokey L280
20640     bal=bal-tamount
20660     for j=1 to 10
20680       if uprc$(penalty$(j))="Y" then gb(j)=gb(j)-tg(j) ! subtract penalty breakdown from balance breakdown
20700     next j
20720     rewrite #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb
20740     delete #2: 
20760   end if
20780 loop 
20800 XIT: fnxit
20820 ! <Updateable Region: ERTN>
20840 ERTN: fnerror(program$,err,line,act$,"NO")
20860   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20900   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20920 ERTN_EXEC_ACT: execute act$ : goto ERTN
20940 ! /region
