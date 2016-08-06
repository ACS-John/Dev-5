00010 ! Replace R:\acsGL\acGLClos
00020 ! Close Books at Year End
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit,fnsearch,fnerror,fncno,fngl_number_use_dept,fngld2,fntos,fnlbl,fncmdset,fnacs,fntxt,fnqgl,fnagl$,fnopt,fnfra,fncreg_read,fncreg_write
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim acno$*12,bc(13),bp(13),wrd2$(2)*54,cap$*128,bud(13)
00080   dim resp$(10)*80
00090 ! ______________________________________________________________________
00100   let right=1 : let center=2
00110   let fntop(program$,cap$="Close Books at Year End")
00120   let fncno(cno)
00140   open #20: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00142   read #20,using 'Form Pos 150,2*N 1,Pos 296,n 2,Pos 384,N 2',rec=1: use_dept,use_sub,lmu,nap
00144   close #20: 
00150 SCR1: ! 
00160   let fntos(sn$="CloseYear1") !:
        let lc=0 : let mylen=30 : let mypos=mylen+2 : let width=0
00170   let fnlbl(lc+=1,1,"* * *   Warning   * * *",width,center)
00180   let fnlbl(lc+=1,1,"This program is to be used only at the end of the",width,center)
00190   let fnlbl(lc+=1,1,"year, after all reports have been processed.",width,center)
00200   let fnlbl(lc+=1,1,"Enter CLOSE to continue:",mylen,right)
00210   let fntxt(lc,mypos,5) !:
        let resp$(1)=""
00220   let fnlbl(lc+=2,1,"Year being closed:",mylen,1)
00230   let fntxt(lc,mypos,2,0,1,"30",0,"Enter the two digit code for the year you are closing.") !:
        let resp$(2)=""
00240   let fncmdset(2)
00250   let fnacs(sn$,0,mat resp$,ckey)
00260   if ckey=5 then goto XIT
00270   let pas$=resp$(1)
00280   let yr$=cnvrt$("pic(##)",val(resp$(2)))
00290   if lwrc$(pas$)<>lwrc$("CLOSE") then goto SCR1
00300 ! 
00310   if fngl_number_use_dept=0 then goto L420
00320 ! 
00330   let fntos(sn$="CloseYear2") !:
        let lc=0 : let mylen=30 : let mypos=mylen+3 : let width=0
00340   let fnfra(1,1,4,70,"Method of Closing","You must indicate if you will be closing to one equity account or to multiple accounts.",0)
00350   let fnopt(1,3,"Close each fund to a separate account",0,1) !:
        let resp$(rc+=1)="False"
00360   let fnopt(2,3,"Close all departments to one retained earnings (equity) account",0,1) !:
        let resp$(rc+=1)="False"
00370   let fncmdset(2)
00380   let fnacs(sn$,0,mat resp$,ckey)
00390   if ckey=5 then goto XIT
00400   if resp$(1)="True" then let dn1=0 else let dn1=1
00410 ! ______________________________________________________________________
00420 L420: open #1: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\GLINDEX.h"&str$(cno)&",Shr",internal,outin,keyed 
00430   open #11: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\glIndx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00431 ! 
00432   if ~exists("Q:\GLmstr\budgetinfo.h"&str$(cno)) then 
00433     open #12: "Name=Q:\GLmstr\budgetinfo.h"&str$(cno)&",KFName=Q:\GLmstr\budindx.h"&str$(cno)&",Use,RecL=28,KPs=1,KLn=14,Shr",internal,outin,keyed 
00434     close #12: 
00435   end if 
00436   execute "Index Q:\GLmstr\budgetinfo.h"&str$(cno)&",Q:\GLmstr\budindx.h"&str$(cno)&",1,14,Replace,DupKeys" ioerr ignore
00440   open #12: "Name=Q:\GLmstr\budgetinfo.h"&str$(cno)&",KFName=Q:\GLmstr\budindx.h"&str$(cno)&",Shr",internal,outin,keyed 
00442 ! 
00450   open #1: "Name=Q:\GLmstr\acprcks.h"&str$(cno)&",SIZE=0,RecL=110,Replace",internal,output ioerr ignore
00460   open #13: "Name=Q:\GLmstr\PRmstr.h"&str$(cno)&",KFName=Q:\GLmstr\PRINDEX.h"&str$(cno),internal,outin,keyed ioerr SCR2
00470 L470: read #13,using 'Form POS 271,2*N 5': n1,n2 eof L500
00480   rewrite #13,using 'Form POS 271,2*N 5': 0,0
00490   goto L470
00500 L500: close #13: 
00510 SCR2: ! 
00520   let t5=0
00530   let fntos(sn$='CloseYear3')
00532   let lc=0 : let mylen=30 : let mypos=mylen+2 : let width=80
00540   let fnlbl(lc+=1,1,"Enter the Last Retained Earnings Account or Equity Account.",width,center)
00550   let fnlbl(lc+=1,1,"The account that dividend, income, and expenses will be closed to.",width,center)
00560   let lc+=1 ! let fnlbl(lc+=1,1,"",width,center)
00570   if fngl_number_use_dept=0 or dn1=1 then 
00572     let fnlbl(lc+=1,1,"All accounts after this ",width,center)
00573   else 
00574     let fnlbl(lc+=1,1,"All Accounts for this Cost Center after this ",width,center)
00575   end if 
00580   let fnlbl(lc+=1,1,"be reset with zero balances.",width,center)
00590   let fnlbl(lc+=1,1,"account will be reset with zero balances.",width,center)
00600   let fnlbl(lc+=1,1,"Enter Account Number:",mylen,right)
00602   let fnqgl(lc,mypos)
00603   let resp$(1)=''
00604 ! if dn1 then
00606 !   fncreg_read("last retained earnings account - fund "&str$(val(glnumber$(1:3))),resp$(1)) ! resp$(1)=''
00608 ! else 
00610 !   let fncreg_read("last retained earnings account - no fund ",resp$(1))
00612 ! end if
00620   let fncmdset(11)
00630   let fnacs(sn$,0,mat resp$,ckey)
00640   if ckey=5 then goto XIT
00641   let glnumber$=fnagl$(resp$(1))
00642   if dn1 then 
00644     let fncreg_write("last retained earnings account - fund "&str$(val(glnumber$(1:3))),resp$(1))
00646   else 
00648     let fncreg_write("last retained earnings account - no fund",resp$(1))
00650   end if 
00660   read #1,using L670,key=glnumber$: dno$,ano$,sno$ nokey SCR2
00670 L670: form pos 1,c 3,c 6,c 3
00680   let acno$=glnumber$(1:3)&"         "
00690 ! ______________________________________________________________________
00700 ! 
00710 ! fnwait -  "Closing Books..."
00720 ! ______________________________________________________________________
00730   read #1,using L760,key>=acno$: acno$,bb,cb,mat bc,mat bp,mat bud nokey SCR2
00740   goto L770
00750 L750: read #1,using L760: acno$,bb,cb,mat bc,mat bp, mat bud eof L940
00760 L760: form pos 1,c 12,pos 81,41*pd 6.2
00770 L770: if fngl_number_use_dept=0 or dn1=1 then goto L790
00780   if glnumber$(1:3)><acno$(1:3) then let dno=ano=sno=0: goto SCR2
00790 L790: if acno$><glnumber$ then goto L830
00800   let cb=-t5
00810 ! LET BC(NAP)=CB   ! SET RETAINED BALANCE IN HISTORY AFTER CLOSING
00820   if nap=0 or nap>13 then let nap=12
00830 L830: let pbp=bp(nap)
00840   mat bp=bc
00850   mat bc=(0)
00860   let bb=cb
00870   let t5=t5+cb
00880   if acno$>glnumber$ then write #12,using "form pos 1,c 12,c 2,2*pd 6.2": acno$,yr$,cb,sum(bud) ! create a budget history record
00890   if acno$<=glnumber$ then goto L910
00900   let cb=bb=0
00910 L910: rewrite #1,using L920: acno$,bb,cb,mat bc,mat bp,mat bud,pbp
00920 L920: form pos 1,c 12,pos 81,41*pd 6.2,pos 327,pd 6.2
00930   goto L750
00940 L940: if fngl_number_use_dept=0 or dn1=1 then goto L960
00950   let dno=ano=sno=0 : goto SCR2
00960 L960: close #1: 
00970   goto XIT
00980 ! ______________________________________________________________________
00990 XIT: let fnxit
00995 IGNORE: continue 
01000 ! ______________________________________________________________________
01010 ! <Updateable Region: ERTN>
01020 ERTN: let fnerror(cap$,err,line,act$,"xit")
01030   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01050   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01060 ERTN_EXEC_ACT: execute act$ : goto ERTN
01070 ! /region
01080 ! ______________________________________________________________________
