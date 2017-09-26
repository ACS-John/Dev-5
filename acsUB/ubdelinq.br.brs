00010 ! Replace S:\acsUB\UBdelinq
00020 ! Past Due Balance Breakdown
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos,fnopenprn,fncloseprn,fnerror,fnwait,fnchk,fnxit,fncno,fnd1,fncmdset,fntop,fncreg_read,fnget_services
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim z$*10,e$*30,g(12),cap$*128,resp$(10)*60
00080   dim servicename$(10)*20,hdr$*230,detail(11),t(11),gb(10),a$*20
00090   dim service$(10)*2,tax_code$(10)*1,penalty$(10)*1
00095   dim srv$(10)*2
00100 ! ______________________________________________________________________
00120   let fnd1(lbill)
00130   let fntop(program$,cap$="Past Due Balance Breakdown")
00140 ! need to build headings from this information
00150   fnget_services(mat servicename$, mat srv$, mat tax_code$,mat penalty$)
00152   fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
00154   fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
00155   if trim$(servicename$(3))<>"Electric" and trim$(srv$(3))="EL" then let needelecused=1
00156   if trim$(servicename$(4))<>"Gas" and trim$(srv$(4))="GA" then let needgasused=1
00160   let x=0
00170   let hdr$="{\ul Account No}  {\ul Customer Name }       "
00180   for j=1 to 10
00181     if j=3 and needelecused=1 then goto L200
00182     if j=4 and needgasused=1 then goto L200
00190     if trim$(servicename$(j))<>"" then !:
            let hdr$=hdr$&" {\ul "&lpad$((trim$(servicename$(j)(1:9))),9)&"}" !:
            let services+=1
00200 L200: next j
00210   mat detail(services+1)
00220   mat t(services+1)
00230 ! ______________________________________________________________________
00240 MENU1: ! 
00250   let fntos(sn$="ubdelinq")
00260   let fnlbl(1,1,"As of Date:",19,1)
00270   let fntxt(1,21,8,8,0,"1001") !:
        if d1<>0 then let resp$(1)=str$(d1) else let resp$(1)=date$("mmddyy")
00280   let fnlbl(2,1,"Last Billing Date:",19,1)
00290   let fntxt(2,21,8,0,0,"1001") !:
        let resp$(2)=str$(lbill)
00300   let fnchk(3,1,"Skip customers who only owe current bill") !:
        let resp$(3)="False"
00310   let fnchk(4,1,"Skip customers with credit balance") !:
        let resp$(4)="False"
00320   let fnchk(5,1,"Only show past due amounts (not current month)") !:
        let resp$(5)="True"
00330   let fnchk(6,1,"Skip accounts with Zero balances") !:
        let resp$(6)="True"
00340   let fncmdset(3)
00350 L350: let fnacs(sn$,0,mat resp$,ckey)
00360   if ckey=5 then goto XIT
00370   let d1=val(resp$(1)) !:
        let lbill=val(resp$(2))
00380   if uprc$(resp$(3))=uprc$("True") then let skipcurrent=1
00390   if uprc$(resp$(4))=uprc$("True") then let skipcredits=1
00400   if uprc$(resp$(5))=uprc$("True") then let pastdueonly=1
00410   if uprc$(resp$(6))=uprc$("True") then let skipzero=1
00420   if lbill<10100 or lbill>123199 then goto L350
00430   if d1<10100 or d1>123199 then goto L350
00440   let d7=int(d1/10000)
00450   let d6=d7*10000
00460   let d5=int((d1-d6)/100)
00470   let d8=d1-(d7*10000+d5*100)
00480   if d7<1 or d7>12 then goto L350
00490   if d5<1 or d5>31 then goto L350
00500 ! ______________________________________________________________________
00510   on fkey 5 goto DONE
00520   let fnopenprn
00530   gosub HEADER
00550   let v=bkno1
00560   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00570 L570: read #1,using L580: z$,e$,f,bal,mat g,mat gb eof DONE
00580 L580: form pos 1,c 10,pos 41,c 30,pos 296,pd 4,pos 292,pd 4.2,pos 300,12*pd 4.2,pos 388,10*pd 5.2
00590   if f<>lbill then mat g=(0)
00600   if skipcurrent=1 and bal-g(11)=<0 then goto L570 !:
          ! skip anyone who only owes last times bill
00610   if skipzero=1 and bal=0 then goto L570 !:
          ! skip Zero Balance Accounts
00620   if skipcredits=1 and bal<=0 then goto L570 !:
          ! skip credit balance accounts
00630   if pastdueonly=1 then goto L640 else goto L690
00640 L640: for j=1 to 10 ! subtract current bill out of balance breakdown
00650     if penalty$(j)="Y" then goto L680 ! skip penalties
00660     if gb(j)=0 and j<10 then let g(j+1)=g(j+1)+g(j) : let g(j)=0 ! try to prevent negative amounts in columns
00670     let gb(j)=gb(j)-g(j)
00680 L680: next j
00690 L690: let x=0
00700   if pastdueonly=1 and skipzero=1 and sum(gb)=0 then goto L570 ! if only owe the current bill then consider it zero for the skipzero test and pastdueonly test
00710   for j=1 to 10
00714     if j=3 and needelecused=1 then goto L730
00715     if j=4 and needgasused=1 then goto L730
00720     if trim$(servicename$(j))<>"" then let detail(x+=1)=gb(j)
00730 L730: next j
00740   let detail(x+1)=sum(gb)
00750   print #255,using L760: z$,e$(1:20),mat detail pageoflow L990
00760 L760: form pos 1,c 12,c 21,11*n 10.2
00770   mat t=t+detail
00780   goto L570
00790 ! ______________________________________________________________________
00800 HEADER: ! 
00810   print #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
00820   print #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
00830   print #255: "\qc {\f181 \fs18 \b As of "&cnvrt$("pic(zz/zz/zz)",d1)&"}"
00840   let pagetab=41+services*10
00850   print #255,using L860: "\ql "&date$,"Page "&str$(p2+=1)
00860 L860: form pos 1,c 20,pos pagetab,c 10
00870   print #255: hdr$&" {\ul     Total}"
00880   return 
00890 ! ______________________________________________________________________
00900 PRINT_TOTALS: ! 
00910   print #255: "" !:
        print #255: "" !:
        print #255,using L760: "","****** Grand Totals",mat t
00920   return 
00930 ! ______________________________________________________________________
00940 DONE: close #1: ioerr L960
00950   gosub PRINT_TOTALS
00960 L960: let fncloseprn
00970 XIT: let fnxit
00980 ! ______________________________________________________________________
00990 L990: print #255: newpage
01000   gosub HEADER
01010   continue 
01020 ! ______________________________________________________________________
01030 ! <Updateable Region: ERTN>
01040 ERTN: let fnerror(program$,err,line,act$,"xit")
01050   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01070   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01080 ERTN_EXEC_ACT: execute act$ : goto ERTN
01090 ! /region
01100 ! ______________________________________________________________________
