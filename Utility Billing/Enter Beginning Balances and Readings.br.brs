00010 ! Replace S:\acsUB\ubCorAmt
00020 ! -- Enter Beginning Balances and Readings
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit, fnerror,fnLbl,fnTos,fnTxt, fnAcs,fncmbact,fnmsgbox,fndate_mmddyy_to_ccyymmdd,fnCmdSet,fntop,fnCmdKey,fnget_services
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim z$*10,d(15),adr(2),p$*10,txt$*80,resp$(20)*80,txt$(6)*80
00080   dim o(2),srv$(10)*20,in1(19),gb(10),e$*30,tg(11),g(12)
00090 ! ______________________________________________________________________
00110   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],NoShr",internal,outIn,keyed 
00120   open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr,USE,RecL=102,KPs=1,KLn=19",internal,outIn,keyed 
00130   fnget_services(mat srv$)
00140   for j=1 to udim(srv$)
00150     srv$(j)=trim$(srv$(j)) !:
          if srv$(j)<>"" then srv$(j)=srv$(j)&":" : services+=1
00160   next j
00180   fntop(program$)
00190   goto MENU1
00200 ! ______________________________________________________________________
00210 MENU1: ! 
00220   fnTos(sn$:="ubCorAmt-1") !:
        mylen=48 !:
        mypos=mylen+2
00230   fnLbl(1,1,"Starting Account ([All] for first run):",mylen,1)
00240   fncmbact(1,mypos,1) !:
        resp$(1)="[All]"
00250   fnLbl(2,1,"Summary Transaction Date (mmddyy):",mylen,1)
00260   fnTxt(2,mypos,8,0,0,"1") !:
        resp$(2)=str$(n)
00270   fnLbl(4,1,"This program requires exclusive use of the Customer File.",90,2) !:
        fnLbl(5,1,"Please make sure no one else is in Utility Billing",90,2) !:
        fnLbl(6,1,"while you use this program.",90,2)
00280   fnCmdSet(2)
00290   fnAcs(sn$,0,mat resp$,ckey)
00300   if ckey=5 then goto XIT
00310   n=val(resp$(2))
00320   hz$=z$=lpad$(trim$(resp$(1)(1:10)),10)
00330 ! ______________________________________________________________________
00340   if trim$(uprc$(hz$))=uprc$("[All]") then !:
          goto READ_CUSTOMER else !:
          read #1,using L380,key=z$: z$,e$,mat d,bal,mat gb nokey MENU1 !:
          goto SKIP_READ_CUSTOMER
00350 ! ______________________________________________________________________
00360 READ_CUSTOMER: ! 
00370   read #1,using L380: z$,e$,mat d,bal,mat gb eof DONE
00380 L380: form pos 1,c 10,pos 41,c 30,pos 217,15*pd 5,pos 292,pd 4.2,pos 388,10*pd 5.2
00390 SKIP_READ_CUSTOMER: ! 
00400   goto SCREEN2
00410 ! ______________________________________________________________________
00420 SCREEN2: ! 
00430   sn$="ubCorAmt-2" !:
        fnTos(sn$)
00440   mylen=15 !:
        for j=1 to 10 ! UDIM(SRV$) !:
          mylen=max(len(trim$(srv$(j))),mylen) !:
        next j !:
        mypos=mylen+2 !:
        mypos2=mypos+12 ! kj
00450   txt$="Account:" !:
        fnLbl(1,1,txt$,mylen,1)
00460   fnTxt(1,mypos,10,0,1,"",1) !:
        resp$(1)=z$
00470   txt$="Balance:" !:
        fnLbl(2,1,txt$,mylen,1)
00480   fnTxt(2,mypos,9,0,1,"10") !:
        resp$(2)=str$(bal)
00490   txt$="Balance" : fnLbl(4,mypos,txt$,10,2,1) !:
        txt$="Reading" : fnLbl(4,mypos2,txt$,10,2,1)
00500   respc=2 !:
        resp_line=4 !:
        for j=1 to 10 ! UDIM(SRV$) kj
00510     if trim$(srv$(j))<>"" then !:
            respc+=1 !:
            resp_line+=1 !:
            fnLbl(resp_line,1,srv$(j),mylen,1) !:
            fnTxt(resp_line,mypos,9,0,1,"10") !:
            resp$(respc)=str$(gb(j))
00520     if trim$(srv$(j))<>"" and (j=1) then !:
            fnTxt(resp_line,mypos2,11,0,1,"20") !:
            respc+=1 : resp$(respc)=str$(d(1))
00530     if (trim$(srv$(j))="Electric:" or trim$(srv$(j))="Lawn Meter:") and (j=3) then !:
            fnTxt(resp_line,mypos2,11,0,1,"20") !:
            respc+=1 : resp$(respc)=str$(d(5))
00540     if trim$(srv$(j))="Gas:" and (j=4) then !:
            fnTxt(resp_line,mypos2,11,0,1,"20") !:
            respc+=1 : resp$(respc)=str$(d(9))
00550   next j
00560   respc+=1 : resp$(respc)=e$ !:
        fnTxt(1,40,30,0,0,"",1)
00570   fnCmdKey("&Next",1,1,0,"Save and display next account in account order." ) !:
        fnCmdKey("&Select Account",2,0,0,"Select another account." ) !:
        fnCmdKey("E&xit",5,0,1,"Returns to menu")
00580   fnAcs(sn$,0,mat resp$,ckey)
00590   if ckey=5 then goto XIT
00600   bal=val(resp$(2))
00610   respc=2 !:
        for j=1 to 10 ! udIM(SRV$)  kj
00620     if trim$(srv$(j))<>"" then !:
            respc+=1 : gb(j)=val(resp$(respc)) !:
            tg(j)=val(resp$(respc)) conv L670
00630 ! If BAL<0 Then tG(J)=-VAL(RESP$(RESPC)) Else tG(J)=VAL(RESP$(RESPC))
00640     if trim$(srv$(j))="Water:" and (j=1) then !:
            respc+=1 : d(1)=val(resp$(respc)): wr=d(1)
00650     if trim$(srv$(j))="Electric:" and (j=3) then !:
            respc+=1 : d(5)=val(resp$(respc)): er=d(5)
00660     if trim$(srv$(j))="Gas:" and (j=4) then !:
            respc+=1 : d(9)=val(resp$(respc)): gr=d(9)
00670 L670: next j
00680 ! set T1 to total allocations !:
        t1=0 !:
        for j=1 to udim(gb) !:
          t1+=gb(j) !:
        next j
00690   if t1<>bal then !:
          gosub T1_NOT_EQUAL_BAL : goto SCREEN2 else !:
          goto WRITE_TRANS
00700 ! ______________________________________________________________________
00710 T1_NOT_EQUAL_BAL: ! 
00720   txt$(1)="Total Allocations do not equal Current Balance" !:
        txt$(2)="" !:
        txt$(3)=cnvrt$("pic(--------#.##)",bal)&"  Current Balance  " !:
        txt$(4)=cnvrt$("pic(--------#.##)",t1)&"  Total Allocations" !:
        txt$(5)="------------" !:
        txt$(6)=cnvrt$("pic(--------#.##)",bal-t1)&"  Difference"
00730   fnmsgbox(mat txt$,resp$,'',48)
00740   return 
00750 ! ______________________________________________________________________
00760 WRITE_TRANS: ! 
00770   gosub DEL_HIST
00780   if bal<0 then tcode=4 else tcode=5 ! SHOW AS CREDIT MEMO IF NEGATIVE BAL OWED, ELSE DEBIT MEMO
00790   tdate=n
00800   tdate=fndate_mmddyy_to_ccyymmdd(tdate)
00810   tg(11)=sum(tg)
00820   if bal=0 then goto L860
00830   write #2,using L840: z$,tdate,tcode,abs(bal),mat tg,wr,wu,er,eu,gr,gu,bal,pcode
00840 L840: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00850   mat g=(0)
00860 L860: rewrite #1,using L870: mat d,bal,mat g,mat adr,mat gb
00870 L870: form pos 217,15*pd 5,pos 292,pd 4.2,pos 300,12*pd 4.2,pos 348,2*pd 3,pos 388,10*pd 5.2
00880   if ckey=2 then goto MENU1 ! enter next Account
00890   goto READ_CUSTOMER
00900 ! ______________________________________________________________________
00910 DONE: ! 
00920   close #1: 
00930   close #2: 
00940   goto XIT
00950 ! ______________________________________________________________________
00960 ! If ERR=4152 Then Goto 420
00970 ! <Updateable Region: ERTN>
00980 ERTN: fnerror(program$,err,line,act$,"xit")
00990   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01010   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01020 ERTN_EXEC_ACT: execute act$ : goto ERTN
01030 ! /region
01040 ! ______________________________________________________________________
01050 XIT: fnxit
01060 ! ______________________________________________________________________
01070 DEL_HIST: ! Delete History
01080   restore #2,key>=z$&"         ": nokey L1140
01090 L1090: read #2,using L1100: p$,tdate eof L1140
01100 L1100: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01110   if p$<>z$ then goto L1140 ! not same account
01120   if p$=z$ then delete #2: 
01130   goto L1090
01140 L1140: return 
