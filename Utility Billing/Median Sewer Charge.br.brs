20000 ! formerly S:\acsUB\ubmeans.br
20020 ! ______________________________________________________________________
20040   library 'S:\Core\Library': fnAcs,fnopenprn,fncloseprn,fnerror,fnmsgbox,fnTxt,fnLbl,fnTos,fnxit,fnCmdSet,fntop
20060 ! ______________________________________________________________________
20080   on error goto Ertn
20100 ! ______________________________________________________________________
20120   dim cd1(8),x(13),txt$*60,message$(5)*80,message$*60,tg(11)
20140 ! ______________________________________________________________________
20160   fntop(program$)
20180 ! ______________________________________________________________________
20200   open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
20220   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
20240   open #5: "Name=[Q]\UBmstr\MEANs.h[cno],RecL=22,REPLACE",internal,output 
20260   read #1,using L490: x$,a2 eof DONE
20280   restore #2,key>=x$&"         ": nokey L230
20300 L170: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L230
20320   if p$<>x$ then goto L230           ! history record must belong to this customer
20340   if tcode<>1 then goto L170 ! charge transaction
20360   j=j+1 
20380   if j>8 then goto L230
20400   resp$(j)=str$(tdate)
20420   goto L170
20440 L230: restore #1: 
20460 ! ______________________________________________________________________
20480 SCR1: ! 
20500   fnTos(sn$:='means-1') 
20520   mylen=62 : mypos=50
20540   txt$="Billing Dates for Months to be Considered:" 
20560   fnLbl(1,1,txt$,mylen,1)
20580   for j=1 to 8 
20600     fnTxt(j+1,mypos,10,0,0,"3") 
20620     resp$(j)="" 
20640   next j
20660   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
20680   if ckey=5 then goto XIT
20700   for j=1 to 8
20720 L320: x=pos(resp$(j),"/",1)
20740     if x>0 then resp$(j)(x:x)="": goto L320
20760   next j
20780   for j=1 to 8 
20800     cd1(j)=val(resp$(j)) conv SCR1 
20820   next j
20840   if cd1(1)=0 then 
20860     mat message$(1): mytype=0 
20880     message$(1)="You must enter at least one date!" 
20900     fnmsgbox(mat message$,resp$,'',mytype) 
20920     goto SCR1
20940   end if
20960 ! ______________________________________________________________________
20980 SCR2: ! 
21000   sn$="Means-2" 
21020   fnTos(sn$)
21040   txt$="Sewer code to analyze:" 
21060   fnLbl(1,1,txt$,22,1)
21080   fnTxt(1,24,2,2,0,"20") 
21100   resp$(1)=""
21120   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
21140   if ckey=5 then goto SCR1
21160   sewcode=val(resp$(1)) conv SCR2
21180   if sewcode=0 then 
21200     mat message$(1): mytype=0 
21220     message$(1)="You must enter at least one date!" 
21240     fnmsgbox(mat message$,resp$,'',mytype) 
21260     goto SCR2
21280   end if
21300   fnopenprn
21320 L470: read #1,using L490: x$,a2 eof DONE
21340   if a2<>sewcode then goto L470 ! only average certain rate codes
21360 L490: form pos 1,c 10,pos 145,pd 2,pos 1822,n 9
21380   t1=t2=t3=x=0
21400   mat x=(0)
21420   restore #2,key>=x$&"         ": nokey L470
21440 L530: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L620
21460 L540: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
21480   if p$<>x$ then goto L620
21500   if tcode<>1 then goto L530 ! only charge transactions
21520   for j1=1 to 8
21540     if cd1(j1)=tdate then 
21560       t1=t1+1
21580       t2=t2+wu 
21600       x=x+1
21620       x(x)=wu 
21640       goto L530
21660     end if
21680   next j1
21700   goto L530
21720 ! ______________________________________________________________________
21740 L620: if t1>0 then t3=int(t2/t1) else t3=0
21760   write #5,using "Form POS 1,C 10,N 12.2": x$,t3
21780   goto L470
21800 ! ______________________________________________________________________
21820 DONE: ! 
21840   gosub PRINT_REPORT
21860   fncloseprn
21880 XIT: fnxit
21900 ! ______________________________________________________________________
21920 ! ______________________________________________________________________
21940 ! <Updateable Region: ERTN>
21960 ERTN: fnerror(program$,err,line,act$,"NO")
21980   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
22000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
22020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
22040 ERTN_EXEC_ACT: execute act$ : goto ERTN
22060 ! /region
22080 ! ______________________________________________________________________
22100 PRINT_REPORT: ! 
22120   close #1: ioerr L820
22140 L820: close #2: ioerr L830
22160 L830: close #5: ioerr L840
22180 L840: execute "Index [Q]\UBmstr\MEANs.h[cno],[Q]\UBmstr\MEANIDX.h[cno] 11,12,REPLACE,DupKeys -n"
22200   open #5: "Name=[Q]\UBmstr\MEANs.h[cno],KFName=[Q]\UBmstr\MEANIDX.h[cno],Shr",internal,outIn,keyed 
22220   gosub HEADER
22240   means=int(lrec(5)/2)
22260 L880: read #5,using "Form POS 1,C 10,N 12.2": z$,t3 eof L940
22280   x=x+1
22300   pr #255,using L910: z$,t3 pageoflow L950
22320 L910: form pos 11,c 10,x 3,n 12.2,skip 1
22340   if means=x then pr #255: "This should be the halfway point"
22360   goto L880
22380 L940: return 
22400 L950: pr #255: newpage
22420   gosub HEADER
22440 continue 
22460 HEADER: ! r:
22480   pr #255,using L1000: date$,env$('cnam'),time$,"Median Sewer Charge"
22500   L1000: form skip 2,pos 1,c 10,pos 20,cc 40,skip 1,pos 1,c 10,pos 20,cc 40,skip 2
22520 return ! /r
