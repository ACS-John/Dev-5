00010 ! Replace S:\acsCL\ckLstV
00020 ! Check Listing by Vendor (Transaction List: sort by Vendor)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fncno,fntos,fnlbl,fntxt,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ________Dim(s) by file________________________________________________
00070   dim cnam$*40,dat$*20 ! CNO
00080   dim tr$(5)*35 ! TRMstr
00090   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,ta(2) ! PayMstr
00100   dim cap$*128
00110 ! __Get Info from CNO File______________________________________________
00120   let fncno(cno,cnam$)
00130   let fntop(program$, cap$="Check Listing by Payee")
00140   let right=1
00150   open #trmstr=22: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #paymstr:=1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00170 ! ______________________________________________________________________
00180   let fntos(sn$="cklstv") !:
        let respc=0 : let mylen=25 : let mypos=mylen+2
00190   let fnlbl(1,40,"",1,1)
00200   let fnlbl(1,1,"Beginning Date:",mylen,right)
00210   let fntxt(1,mypos,8,0,1,"ccyymmdd") !:
        let resp$(respc+=1)=""
00220   let fnlbl(2,1,"Ending Date:",mylen,right)
00230   let fntxt(2,mypos,8,0,1,"ccyymmdd") !:
        let resp$(respc+=1)=""
00240   let fncmdset(3)
00250   let fnacs(sn$,0,mat resp$,ckey)
00260   if ckey=5 then goto XIT
00270   let date1=val(resp$(1)) !:
        let date2=val(resp$(2))
00280   let fnopenprn
00290   gosub HDR
00300   goto BODY
00310 ! ______________________________________________________________________
00320 HDR: ! Page Heading
00330   pr #255,using 'Form POS 1,CC 80': cnam$
00340   pr #255,using 'Form POS 1,CC 80': "Company Number "&str$(cno)
00350   pr #255,using 'Form POS 1,CC 80': "Check Listing By Vendor"
00360   pr #255,using 'Form Pos 1,Cc 80': "For the Date Range Starting "&cnvrt$("pic(zzzz/zz/zz)",date1)&" and Ending "&cnvrt$("pic(zzzz/zz/zz)",date2)
00370   pr #255: ""
00380   pr #255,using 'Form Pos 1,C 80': "Chk/Ref# Date   Amount      Payee No Name/Description"
00390   pr #255,using 'Form Pos 1,C 80': "________ ______ ___________ ________ ___________________________________"
00400   return 
00410 ! ______________________________________________________________________
00420 PGOF: ! 
00430   pr #255: newpage
00440   gosub HDR
00450   continue 
00460 ! ______________________________________________________________________
00470 BODY: ! 
00480   read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof LAS !:
        let tr$(3)=str$(tr3)
00490   if date1<>0 and fndate_mmddyy_to_ccyymmdd(val(tr$(2)))<date1 then goto BODY
00500   if date2<>0 and fndate_mmddyy_to_ccyymmdd(val(tr$(2)))>date2 then goto BODY
00510   if tcde<>1 then goto BODY ! checks only
00520   if tr$(4)<>vn$ and vn$<>"" then gosub TOTALS
00530   if tr$(4)<>vn$ then gosub SUBHEADING
00540   pr #255,using 'Form POS 1,C 8,X 1,C 6,N 12.2,X 1,C 8,X 1,C 35': tr$(1),tr$(2),val(tr$(3)),tr$(4) pageoflow PGOF
00550   let total1+=val(tr$(3))
00560   goto BODY
00570 ! ______________________________________________________________________
00580 SUBHEADING: ! 
00590   let ytdp=typ=ta(1)=ta(2)=0 : let vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=""
00600   read #paymstr,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12',key=tr$(4): vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$ nokey PR_NOKEY
00610   pr #255,using 'Form POS 1,C 80': "Vendor: "&trim$(vn$)&". "&nam$ pageoflow PGOF : goto PAST_PR_NOKEY
00620 PR_NOKEY: ! 
00630   pr #255,using 'Form POS 1,C 80': "Vendor: "&trim$(tr$(4))&". (This Vendor has been Deleted)" pageoflow PGOF
00640 PAST_PR_NOKEY: ! 
00650   let total1=0
00660   return 
00670 ! ______________________________________________________________________
00680 TOTALS: ! 
00690   pr #255,using 'Form POS 1,C 10,N 17.2': "Totals:",total1 !:
        pr #255: ""
00700   let total1=0
00710   return 
00720 ! ______________________________________________________________________
00730 LAS: ! 
00740   gosub TOTALS
00750   let fncloseprn
00760   goto XIT
00770 ! ______________________________________________________________________
00780 XIT: let fnxit
00790 ! ______________________________________________________________________
00800 ! <Updateable Region: ERTN>
00810 ERTN: let fnerror(program$,err,line,act$,"xit")
00820   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00830   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00840   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00850 ERTN_EXEC_ACT: execute act$ : goto ERTN
00860 ! /region
00870 ! ______________________________________________________________________
