00010 ! Replace S:\acsUB\UBColPrn
00020 ! -- Cash Receipts Journal
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fntos,fnopenprn,fncloseprn,fnerror,fncno,fndat,fnwait,fndate_mmddyy_to_ccyymmdd,fnxit,fncmdset,fnchk,fntop,fnacs,fnopt,fncmdset,fnerror,fnmsgbox
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,scr1$(10)*30,alloc(10),nam$*30,o(2),route(200)
00080   dim r(20,4),hd1$*255,cap$*128,servicename$(10)*20,tg(11),resp$(7)*40
00081   dim ml$(3)*90
00090 ! ______________________________________________________________________
00100   fntop("S:\acsUB\UBColPrn",cap$="Cash Receipts Journal")
00110   fncno(cno,cnam$)
00120 ! 
00130   fndat(dat$,1)
00140   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using "Form POS 1,10*C 20",rec=1: mat servicename$ !:
        close #20: 
00150   gosub SCREEN1
00160   hd1$="{\ul  Account  }  {\ul    Total}    {\ul    Date   }"
00170   for j=1 to 10
00180     x2=pos(trim$(servicename$(j))," ",1) !:
          if x2>0 then servicename$(j)=servicename$(j)(1:2)&"-"&servicename$(j)(x2+1:len(servicename$(j))) ! if service name two words long, use part of both
00190     if trim$(servicename$(j))<>"" then !:
            scr1$(sz1+=1)=servicename$(j) !:
            hd1$=hd1$&"  {\ul "&lpad$(rtrm$(servicename$(j)(1:7)),7)&"}"
00200   next j
00210   hd1$=hd1$&"  {\ul Customer Name               }"
00220   mat scr1$(sz1)
00230   mat alloc(sz1)
00240   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00250   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubtrdt.h"&str$(cno)&",Shr",internal,input,keyed 
00260 ! ______________________________________________________________________
00270   on fkey 5 goto XIT
00280   fnopenprn
00290   gosub HDR
00300   goto L430
00310 ! ______________________________________________________________________
00320 HDR: ! 
00330 ! need date$,time$
00340   pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
00350   pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
00360   if ld1<>0 and hd1<>0 then !:
          pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",ld1)& "  To "&cnvrt$("pic(zzzz/zz/zz)",hd1)&"}"
00370   pr #255: ""
00380   pr #255: "\ql "&hd1$
00390   return 
00400 ! ______________________________________________________________________
00430 L430: read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PRTOTALS
00450   if ld1<>0 and tdate<ld1 then goto L430
00460   if hd1<>0 and tdate>hd1 then goto L430
00470   if tamount=0 then goto L430
00480   if tcode<3 or tcode>5 then goto L430 !:
          ! don't pr charges or penalties
00490   if tcode=3 then ti2=1 ! REG.COLLECTION
00500   if tcode=4 then ti2=2 ! CREDIT MEMO
00510   if tcode=5 then ti2=3 ! DEBIT MEMO
00520   if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
00530   r(1,ti2+1)+=tamount
00540   x=0
00550   for j=1 to 10
00560     if trim$(servicename$(j))="" then goto L600
00570     alloc(x+=1)=tg(j)
00580     if ti2=3 then r(x+3,1)-=tg(j) else r(x+3,1)+=tg(j)
00590     r(x+3,ti2+1)+=tg(j)
00600 L600: next j
00610   c$=" "
00620   if tcode=4 then c$="CM" else !:
          if tcode=5 then c$="DM"
00630   if ti1$="True" then !:
          pr #255,using 'Form POS 1,C 10,N 10.2,C 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 9.2': p$,tamount,c$,tdate,mat alloc pageoflow PGOF
00641   if sum(alloc)<>tamount then goto L642 else goto L655
00642 L642: mat ml$(3) !:
        ml$(1)="The breakdown on a collection transation dated "&str$(tdate)& " for customer # "&p$ !:
        ml$(2)="does not balance.  Your totals will be off by "& trim$(cnvrt$("pic($$$,$$$.## cr)",tamount-sum(alloc)))&"." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00655 L655: if resp$="Cancel" then goto XIT
00660   goto L430
00670 ! ______________________________________________________________________
00680 PGOF: pr #255: newpage
00690   gosub HDR
00700   continue 
00710 ! ______________________________________________________________________
00720 PRTOTALS: ! !:
        pr #255: "" !:
        pr #255: "    ************ Totals ************"
00730   pr #255: tab(34);"{\ul       Total}  {\ul    Reg.Col}  {\ul   Cr.Memos}  {\ul   Db.Memos}"
00740   for j=1 to sz1
00750     pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': scr1$(j),r(j+3,1),r(j+3,2),r(j+3,3),r(j+3,4) pageoflow PGOF
00760   next j
00770   pr #255: ""
00780   pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': "Total      ",r(1,1),r(1,2),r(1,3),r(1,4)
00850   fncloseprn
00860 XIT: fnxit
00870 ! ______________________________________________________________________
00880 SCREEN1: ! 
00890   fntos(sn$="UBColPrn") !:
        mylen=33 : mypos=mylen+2
00900   fnlbl(1,1,"Report Heading Date:",mylen,1)
00910   fntxt(1,mypos,20) !:
        resp$(1)=dat$
00920   fnlbl(2,1,"Starting Date (blank for all):",mylen,1)
00930   fntxt(2,mypos,10,0,1,"3",0,"First day of the period to be printed. (ccyymmdd format)") !:
        resp$(2)=str$(ld1)
00940   fnlbl(3,1,"Ending Date (blank for all):",mylen,1)
00950   fntxt(3,mypos,10,0,1,"3",0,"Last day of the period to be printed. (ccyymmdd format)") !:
        resp$(3)=str$(hd1)
00960   fnchk(4,mypos,"Include Details:",1) !:
        resp$(4)="True"
00980   fncmdset(3)
00990   fnacs(sn$,win,mat resp$,ck)
00992   if ck=5 then goto XIT
01000   dat$=resp$(1) !:
        ld1=val(resp$(2)) !:
        hd1=val(resp$(3)) !:
        ti1$=resp$(4)
01030   fndat(dat$,2)
01040   return 
01050 ! ______________________________________________________________________
01060 ! <Updateable Region: ERTN>
01070 ERTN: fnerror(program$,err,line,act$,"xit")
01080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01090   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01110 ERTN_EXEC_ACT: execute act$ : goto ERTN
01120 ! /region
01130 ! ______________________________________________________________________
