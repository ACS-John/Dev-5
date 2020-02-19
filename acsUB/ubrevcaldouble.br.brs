00010 ! Replace S:\acsUB\UBRevCaldouble
00020 !
00030   library 'S:\Core\Library': fnxit, fncno, fnopenprn, fncloseprn, fnLbl, fnTxt,fncmbact,fncmbrt2,fnAcs, fnTos, fnerror,fnwait,fndate_mmddyy_to_ccyymmdd,fnLastBillingDate,fnCmdSet,fnChk,fntop
00040   on error goto Ertn
00050 !
00060   dim x$*10,p$*10,reqz$*10,reqz12$*2,sr$*1,gb(10)
00070   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
00080   dim cap$*128,txt$*200,tg(11),key$*19
00090   dim bt1(14,2),badr(2),resp$(5)*60
00100 !
00110   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00120   fncno(cno) !:
        ! 
00130   fntop("S:\acsUB\UBRevCal",cap$="Reverse Calculation")
00140   fnLastBillingDate(d1)
00150   if d1=0 then d1=val(date$(4:5)&date$(7:8)&date$(1:2))
00160 !
00170   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
00180   open #11: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,outIn,keyed 
00190   open #12: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx3.h[cno],Shr",internal,outIn,keyed 
00200   open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed 
00210   gosub BUD1
00220 ASK1: ! 
00230   x=6
00240   close #111: ioerr L250
00250 L250: fnTos(sn$="ubrevcal") !:
        respc=0
00260   fnLbl(1,1,"You may limit the customers to reverse by changing the options below.",73,2)
00270   fnLbl(2,1,"You may only reverse calculations for the most recent Billing Date!",73,2)
00280   fnLbl(4,1,"Account:",27,1)
00290   fncmbact(4,29,1) !:
        resp$(respc+=1)=""
00300   fnLbl(5,1,"Current Billing Date:",27,1)
00310   fnTxt(5,29,8,0,0,"1") !:
        resp$(respc+=1)=str$(d1)
00320   fnLbl(6,1,"Previous Billing Date:",27,1)
00330   fnTxt(6,29,8,0,0,"1") !:
        resp$(respc+=1)=""
00340   fnLbl(7,1,"Route Number:",27,1)
00350   fncmbrt2(7,29) !:
        resp$(respc+=1)="1"
00360   fnChk(8,29,"Print Status Report") !:
        resp$(respc+=1)="True"
00370   fnCmdSet(2) !:
        fnAcs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00390   if resp$(1)="[All]" then resp$(1)=""
00400   if resp$(4)="[All]" then resp$(4)=""
00410   if trim$(reqz$)="" and trim$(holdreqz$)<>"" then goto XIT !:
          ! if they ever select a customer and then accidently take f1 to !:
          ! continue, it will stop instead of reversing everyone else in file
00420   reqz$=lpad$(rtrm$(resp$(1)(1:10)),10) !:
        reqf=val(resp$(2)) !:
        olddat=val(resp$(3)) !:
        reqz12$=resp$(4)
00430   if uprc$(resp$(5))=uprc$("True") then sr$="Y" else sr$="N"
00440   if sr$="Y" then let fnopenprn
00450   if sr$="Y" and secondpass<>1 then gosub SRHDR
00460   secondpass=1
00470 L470: form pos 5,c 10,x 5,pic(zz/zz/zz)
00480 L480: if rtrm$(reqz$)<> "" then !:
          read #1,using L830,key=reqz$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 nokey ASK1 else !:
          goto L510
00490   if f<>reqf then goto ASK1 !:
          ! must have current billing date
00500   goto L600
00510 L510: read #1,using L830: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 eof XIT
00520   if rtrm$(reqz12$)<>"" and str$(route)<>lpad$(rtrm$(reqz12$),2) then !:
          goto L510
00530 ! If TRIM$(Z$)="210008.02" Then Pause
00540 ! If REQF<>0 AND F<>REQF Then Goto 510
00550   x=fndate_mmddyy_to_ccyymmdd(reqf)
00560   key$=z$&cnvrt$("n 8",x)&"1"
00570   wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
00580   read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L510 ! read history to pull new mat g
00590   for j=1 to 11: g(j)=tg(j): next j ! set array g in customer to array tg in matching transaction
00600 L600: if sr$="Y" then !:
          pr #255,using L470: z$,f pageoflow SRPGOF
00610   for j=1 to 9 : gb(j)=gb(j)-g(j) : next j !:
        ! subtract out current bill from breakdown except for penalty
00620   bal=bal-g(11)
00630   x=fndate_mmddyy_to_ccyymmdd(olddat)
00640   key$=z$&cnvrt$("n 8",x)&"1"
00650   wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
00660   read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L670 ! read previous months history to pull old readings and usages
00670 L670: d(1)=d(2) ! set current water reading to last month
00680   d(2)=wr ! set prior reading to month before last
00690   d(4)=d(4)-d(3) ! subtract out current usage from year to date
00700   d(3)=wu ! set usage to amount in history
00710   d(5)=d(6) ! set current electric reading to last month
00720   d(6)=er ! set prior reading to month before last
00730   d(8)=d(8)-d(7) ! subtract out current usage from year to date
00740   d(7)=eu ! set usage to amount in history
00750   d(9)=d(10) ! set current gas reading to last month
00760   d(10)=gr ! set prior reading to month before last
00770   d(12)=d(12)-d(11) ! subtract out current usage from year to date
00780   d(11)=gu ! set usage to amount in history
00790   f=0 ! set billing date to zero
00800   extra3=extra4: extra4=0
00810   mat g=(0) ! SET ALL LAST TIME BILL TO ZERO
00820   rewrite #1,using L830: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4
00830 L830: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pd 3,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
00840   x=fndate_mmddyy_to_ccyymmdd(reqf)
00850   key$=z$&cnvrt$("n 8",x)&"1"
00860   read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
00870 L870: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00880   delete #2: 
00890   x=fndate_mmddyy_to_ccyymmdd(reqf)
00900   key$=z$&cnvrt$("n 8",0)&" "
00910 L910: read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
00920   if p$<>z$ then goto L950
00930   holdtbal=tbal
00935   read #2,using L870: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L480
00940   goto L910
00950 L950: rewrite #1,using L960,key=z$: holdtbal
00960 L960: form pos 292,pd 4.2
00970   goto L510
00980   form pos 15,pd 4
00990   if bud1=1 then gosub BUD2
01000   if rtrm$(reqz$)<>"" then !:
          holdreqz$=reqz$ !:
          reqz$="" !:
          goto ASK1 else !:
          goto L510
01010 !
01020 XIT: ! 
01030   if sr$<>"Y" then goto L1050
01040   fncloseprn
01050 L1050: fnxit
01060 !
01070 SRHDR: ! 
01080   pg+=1
01090   pr #255: "Reverse Calculation Status Report"
01100   pr #255: "Page "&str$(pg)
01110   pr #255: ""
01120   pr #255: "All accounts listed have been reversed."
01130   pr #255: ""
01140   pr #255: "Account           Billing Date"
01150   pr #255: "_______________   ____________"
01160   return 
01170 !
01180 SRPGOF: ! 
01190   pr #255: newpage
01200   gosub SRHDR
01210   continue 
01220 BUD1: bud1=0
01230   open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L1260
01240   open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative 
01250   bud1=1
01260 L1260: return 
01270 !
01280 BUD2: ! 
01290   bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
01300   if bud1=0 then goto L1400
01310   read #81,using L1320,key=z$: x$,mat ba,mat badr nokey L1400
01320 L1320: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
01330   ta1=badr(1)
01340 L1340: if ta1=0 then goto L1400
01350   read #82,using L1360,rec=ta1: x$,mat bt1,nba noRec L1400
01360 L1360: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
01370   if bt1(1,1)=n then !:
          mat bt1=(0) !:
          rewrite #82,using L1380,rec=ta1: mat bt1
01380 L1380: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
01390   ta1=nba: goto L1340
01400 L1400: return 
01410 !
01420 ! <Updateable Region: ERTN>
01430 ERTN: fnerror(program$,err,line,act$,"NO")
01440   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01470 ERTN_EXEC_ACT: execute act$ : goto ERTN
01480 ! /region
