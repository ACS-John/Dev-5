00010 ! Replace S:\acsUB\UBRevCal
00015 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fnxit,fncno,fnopenprn,fncloseprn,fnlbl,fntxt,fncmbact,fncmbrt2,fnacs, fntos, fnerror,fnwait,fndate_mmddyy_to_ccyymmdd,fnLastBillingDate,fncmdset,fnchk,fntop,fnmsgbox
00025   on error goto ERTN
00030 ! ______________________________________________________________________
00035   dim x$*10,p$*10,reqz$*10,reqz12$*5,sr$*1,gb(10)
00040   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
00045   dim cap$*128,txt$*200,tg(11),key$*19
00050   dim bt1(14,2),badr(2),resp$(5)*60
00055 ! ______________________________________________________________________
00060   fncno(cno)
00065 !  : !
00070   fntop("S:\acsUB\UBRevCal",cap$="Reverse Calculation")
00075 ! ______________________________________________________________________
00080   goto ALLOW_PROGRAM ! if env$('client')="Ash Grove" then goto ALLOW_PROGRAM
00085   dim _msg$(4)*80
00095   _msg$(1)="This program has been removed due to heavy misuse."
00100   _msg$(2)="To recalculate bills simply use Enter Readings and Charges"
00105   _msg$(3)="with the same billing date as used previously."
00110   _msg$(4)="If you feel you need to use this legacy program, please contact ACS."
00115   fnmsgbox(mat _msg$,resp$(1),cap$,16)
00120   goto XIT
00125 ALLOW_PROGRAM: ! 
00130 ! ______________________________________________________________________
00135   fnLastBillingDate(d1)
00140   if d1=0 then d1=val(date$(4:5)&date$(7:8)&date$(1:2))
00145 ! ______________________________________________________________________
00170   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   open #11: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00190   open #12: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndx3.h"&str$(cno)&",Shr",internal,outin,keyed 
00200   open #2: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00210   fn_bud1
00220 ASK1: ! 
00230   x=6
00240   close #111: ioerr L250
00250 L250: fntos(sn$="ubrevcal")
00252   respc=0
00260   fnlbl(1,1,"You may limit the customers to reverse by changing the options below.",73,2)
00270   fnlbl(2,1,"You may only reverse calculations for the most recent Billing Date!",73,2)
00280   fnlbl(4,1,"Account:",27,1)
00290   fncmbact(4,29,1)
00292   resp$(respc+=1)="[All]"
00300   fnlbl(5,1,"Current Billing Date:",27,1)
00310   fntxt(5,29,8,0,0,"1")
00312   resp$(respc+=1)=str$(d1)
00320   fnlbl(6,1,"Previous Billing Date:",27,1)
00330   fntxt(6,29,8,0,0,"1")
00332   resp$(respc+=1)=""
00340   fnlbl(7,1,"Route Number:",27,1)
00350   fncmbrt2(7,29)
00352   resp$(respc+=1)="[All]"
00360   fnchk(8,29,"Print Status Report")
00362   resp$(respc+=1)="True"
00370   fncmdset(2)
00372   fnacs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00410   if trim$(reqz$)="" and trim$(holdreqz$)<>"" then goto XIT ! if they ever select a customer and then accidently take f1 to continue, it will stop instead of reversing everyone else in file
00420   reqz$=lpad$(rtrm$(resp$(1)(1:10)),10)
00421   if trim$(reqz$)='[All]' then reqz$=''
00422   reqf=val(resp$(2))
00423   olddat=val(resp$(3))
00424   reqz12$=resp$(4)
00425   if reqz12$="[All]" then reqz12$=""
00430   if uprc$(resp$(5))=uprc$("True") then sr$="Y" else sr$="N"
00440   if sr$="Y" then let fnopenprn
00450   if sr$="Y" and secondpass<>1 then let fn_srhdr
00460   secondpass=1
00470 L470: form pos 5,c 10,x 5,pic(zz/zz/zz)
00480 L480: ! 
00482   if rtrm$(reqz$)<> "" then 
00483     read #1,using L770,key=reqz$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 nokey ASK1
00484   else 
00485     goto CUSTOMER_READ
00486   end if 
00490   if f<>reqf then goto ASK1 ! must have current billing date
00500   goto L550
00509 CUSTOMER_READ: ! 
00510   read #1,using L770: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 eof FINIS
00520   if trim$(reqz12$)<>"" and route<>val(reqz12$) then goto CUSTOMER_READ
00530 ! If TRIM$(Z$)="210008.02" Then Pause
00540   if reqf<>0 and f<>reqf then goto CUSTOMER_READ
00550 L550: ! 
00551   if sr$="Y" then 
00552     pr #255,using L470: z$,f pageoflow SRPGOF
00554   end if 
00560   for j=1 to 9 : gb(j)=gb(j)-g(j): bal=bal-g(j): next j ! subtract out current bill from breakdown
00570 ! bal=bal-g(11)  moved above 06/01/12
00580   x=fndate_mmddyy_to_ccyymmdd(olddat)
00590   key$=z$&cnvrt$("n 8",x)&"1"
00600   wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
00610   read #2,using L810,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L620 ! read previous months history to pull old readings and usages
00620 L620: ! 
00622   d(1)=d(2) ! set current water reading to last month
00630   d(2)=wr ! set prior reading to month before last
00640   d(4)=d(4)-d(3) ! subtract out current usage from year to date
00650   d(3)=wu ! set usage to amount in history
00660   d(5)=d(6) ! set current electric reading to last month
00670   d(6)=er ! set prior reading to month before last
00680   d(8)=d(8)-d(7) ! subtract out current usage from year to date
00690   d(7)=eu ! set usage to amount in history
00700   d(9)=d(10) ! set current gas reading to last month
00710   d(10)=gr ! set prior reading to month before last
00720   d(12)=d(12)-d(11) ! subtract out current usage from year to date
00730   d(11)=gu ! set usage to amount in history
00740   f=0 !  f=olddat   ! set billing date to zero
00745   extra3=extra4 : extra4=0
00750   mat g=(0) ! SET ALL LAST TIME BILL TO ZERO
00760   rewrite #1,using L770: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4
00770 L770: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pd 3,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
00780   x=fndate_mmddyy_to_ccyymmdd(reqf)
00790   key$=z$&cnvrt$("n 8",x)&"1"
00800   read #2,using L810,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
00810 L810: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00820   delete #2: 
00830   goto L480
00850   if bud1=1 then let fn_bud2
00860   if rtrm$(reqz$)<>"" then 
00861     holdreqz$=reqz$
00862     reqz$=""
00863     goto ASK1
00864   else 
00866     goto L480
00869   end if 
00870 ! ______________________________________________________________________
00880 FINIS: ! 
00890   if sr$<>"Y" then goto XIT
00900   fncloseprn
00910 XIT: fnxit
00920 ! ______________________________________________________________________
00930   def fn_srhdr
00940     pg+=1
00950     pr #255: "Reverse Calculation Status Report"
00960     pr #255: "Page "&str$(pg)
00970     pr #255: ""
00980     pr #255: "All accounts listed have been reversed."
00990     pr #255: ""
01000     pr #255: "Account           Billing Date"
01010     pr #255: "_______________   ____________"
01020   fnend 
01030 ! ______________________________________________________________________
01040 SRPGOF: ! 
01050   pr #255: newpage
01060   fn_srhdr
01070   continue 
01080   def fn_bud1
01082     bud1=0
01090     open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1120
01100     open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&",Shr",internal,outin,relative 
01110     bud1=1
01120 L1120: ! 
01122   fnend 
01130 ! ______________________________________________________________________
01140   def fn_bud2
01150     bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
01160     if bud1=0 then goto L1260
01170     read #81,using L1180,key=z$: x$,mat ba,mat badr nokey L1260
01180 L1180: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
01190     ta1=badr(1)
01200 L1200: if ta1=0 then goto L1260
01210     read #82,using L1220,rec=ta1: x$,mat bt1,nba norec L1260
01220 L1220: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
01230     if bt1(1,1)=n then 
01232       mat bt1=(0)
01234       rewrite #82,using L1240,rec=ta1: mat bt1
01238     end if 
01240 L1240: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
01250     ta1=nba: goto L1200
01260 L1260: ! 
01262   fnend 
01270 ! ______________________________________________________________________
01280 ! <Updateable Region: ERTN>
01290 ERTN: fnerror(program$,err,line,act$,"NO")
01300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01330 ERTN_EXEC_ACT: execute act$ : goto ERTN
01340 ! /region
