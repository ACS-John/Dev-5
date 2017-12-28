00010 ! Replace S:\acsUB\UBowed
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnopenprn,fncloseprn,fnerror,fndat,fncno,fnwin3,fnwait,fnAcs,fnTxt,fnLbl,fnTos,fnFra,fnxit,fnCmdSet,fntop,fnChk
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim dat$*20,message$*40,resp$(5)*20
00070   dim z$*10,e$*30,g(12),adr(2),o(2),e(5),rt(10,3)
00080   dim cap$*128,firstday(3),lastday(3),month(4)
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Balance Breakdown Aged by Month")
00130   fndat(dat$,1)
00140 ! 
00150 ! ______________________________________________________________________
00160   fnTos(sn$="ubowed")
00162   respc=0 : mylen=29 : mypos=mylen+2 : frac=0
00170   fnFra(1,1,3,mypos+14,"Aging Dates","Use the last day of each month for your aging dates.") : fraaging=frac+=1
00180   fnLbl(1,1,"Last Day of Current Month:",mylen,1,0,fraaging)
00190   fnTxt(1,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) !:
        resp$(respc+=1)=''
00200   fnLbl(2,1,"Last Day of Last Month:",mylen,1,0,fraaging)
00210   fnTxt(2,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) !:
        resp$(respc+=1)=''
00220   fnLbl(3,1,"Last Day of Third Month:",mylen,1,0,fraaging)
00230   fnTxt(3,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) !:
        resp$(respc+=1)=''
00240   fnLbl(7,1,"Report Heading Dage:",mylen,1)
00250   fnTxt(7,mypos,20) !:
        resp$(respc+=1)=dat$
00260   fnChk(9,mypos+10,"Skip customers with credit balance:",1) !:
        resp$(respc+=1)="False"
00270   fnCmdSet(3) !:
        fnAcs(sn$,0,mat resp$,ckey,1)
00280   if ckey=5 then goto XIT
00290   for j=1 to 3
00300 ! x=POS(RESP$(J),"/",1)
00310 ! If X>0 Then rESP$(J)(X:X)="": Goto 300
00320     lastday(j)=val(resp$(j))
00330     firstday(j)=(val(resp$(j)(1:6))*100)+1
00340   next j
00350   dat$=resp$(4) !:
        fndat(dat$,2)
00360   if resp$(5)="True" then skipcr=1
00370 ! ______________________________________________________________________
00380 PRINTING: ! 
00390   on fkey 5 goto DONE
00400   fnopenprn
00410   gosub HEADER
00430   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00440   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00450 READ_CUSTOMER: ! 
00460 L460: read #1,using 'Form POS 1,C 10,POS 41,C 30,POS 155,PD 2,POS 292,PD 4.2,POS 296,PD 4,POS 300,12*PD 4.2,POS 348': z$,e$,a7,bal,f,mat g eof L640
00470   if bal=0 then goto L460
00480   if bal<0 and skipcr=1 then goto READ_CUSTOMER
00490   if bal<0 then mat month=(0): month(1)=bal: goto L510
00500   gosub READ_TRANS
00510 L510: if sum(month)=(0) and bal<>0 then month(4)=bal
00520   pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': z$,e$(1:20),month(1),month(2),month(3),month(4),bal pageoflow PGOF
00530   totcolumn1+=month(1) : totcolumn2+=month(2) !:
        totcolumn3+=month(3) : totcolumn4+=month(4) : totbal+=bal
00540   goto READ_CUSTOMER
00550 ! ______________________________________________________________________
00560 HEADER: ! 
00570   pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
00580   pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
00590   pr #255: "\qc  {\f181 \fs22 \b "&trim$(dat$)&"}"
00600   pr #255,using 'Form POS 1,C 82,C 9': "\ql "&date$,"Page "&str$(p2+=1)
00610   pr #255: "{\ul Account No}  {\ul Customer Name}        {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(1)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(2)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(3)))(1:10)&"} {\ul      Older} {\ul    Balance}"
00620   return 
00630 ! ______________________________________________________________________
00640 L640: pr #255: "                                 __________ __________ __________ __________ __________"
00650   pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': "","",totcolumn1,totcolumn2,totcolumn3,totcolumn4,totbal pageoflow PGOF
00660   pr #255: "                                 {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           }"
00670 DONE: close #1: ioerr L680
00680 L680: close #2: ioerr L690
00690 L690: fncloseprn
00700 XIT: fnxit
00710 ! ______________________________________________________________________
00720 PGOF: pr #255: newpage
00730   gosub HEADER
00740   continue 
00750 ! ______________________________________________________________________
00760 READ_TRANS: ! 
00770   sortreq=0 : mat month=(0)
00780   restore #2,key>=z$&"         ": nokey L990
00790 L790: read #2,using 'Form POS 1,C 10,N 8,N 1,PD 4.2': p$,tdate,tcode,tamount eof L990
00800   if p$<>z$ then goto L880
00810   if tcode =3 or tcode=4 then goto L790 ! skip collections or cm
00820   for j=1 to 3
00830     if tdate<firstday(3) then goto L790 ! older than we want to analyze
00840     if tdate>=firstday(j) and tdate<=lastday(j) then !:
            month(j)+=tamount : goto L790
00850     if tdate>lastday(1) then !:
            month(1)+=tamount : goto L790 !:
            ! if trans dated after the last day of the first month, !:
            ! just go ahead and put it the first month
00860   next j
00870   goto L790
00880 L880: tempbal=bal
00890   if month(1)=tempbal then tempbal=0: month: month(2)=month(3)=month(4)=0: goto L990
00900   if month(1)<tempbal then tempbal=tempbal-month(1): goto L920
00910   if month(1)>tempbal then month(1)=tempbal: month(2)=month(3)=month(4)=0: goto L990
00920 L920: if month(2)=tempbal then tempbal=0: month(3)=month(4)=0: goto L990
00930   if month(2)<tempbal then tempbal=tempbal-month(2): goto L950
00940   if month(2)>tempbal then month(2)=tempbal: month(3)=month(4)=0: goto L990
00950 L950: if month(3)=tempbal then tempbal=0: month(4)=0: goto L990
00960   if month(3)<tempbal then tempbal=tempbal-month(3): goto L980
00970   if month(3)>tempbal then month(3)=tempbal: month(4)=0: goto L990
00980 L980: month(4)=tempbal
00990 L990: return 
01000 ! ______________________________________________________________________
01010 ! <Updateable Region: ERTN>
01020 ERTN: fnerror(program$,err,line,act$,"xit")
01030   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01050   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01060 ERTN_EXEC_ACT: execute act$ : goto ERTN
01070 ! /region
01080 ! ______________________________________________________________________
