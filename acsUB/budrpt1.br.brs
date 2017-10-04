00010 ! Replace S:\acsUB\BudRpt1
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwait,fnget_services,fnopenprn,fncloseprn, fndate_mmddyy_to_ccyymmdd, fnxit,fnacs,fntos,fnlbl,fntxt,fncmdset
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim ba(13),bt1(14,2),badr(2),n$*25,txt$*40
00070   dim t1(11),t2(11),t3(11),cap$*128,message$*40,servicename$(10)*20,service$(10)*2,hdr$*255,underline$*255,budget$*255
00100   fntop(program$, cap$="Worksheet")
00110 ! ______________________________________________________________________
00120   fnget_services(mat servicename$,mat service$)
00130   let hdr$="{\ul  Date   }"
00140   let underline$="          "
00150   for j=1 to 10
00160     if trim$(servicename$(j))<>"" then !:
            let hdr$=hdr$&"  {\ul "&lpad$(rtrm$(servicename$(j)(1:8)),8)&"}" !:
            let underline$=underline$&"{\ul         }  " !:
            let totserv=totserv+1
00170   next j
00180   let hdr$=hdr$&"  {\ul Net Bill}" !:
        let underline$=underline$&"{\ul         }  " !:
        let totserv1=totserv+2 !:
        mat t1(totserv1) : mat t2(totserv1) : mat t3(totserv1)
00190 ! ______________________________________________________________________
00200 BUD1: ! INITILIZE BUDGET FILE
00210   bud1=bg1=bg2=0
00220   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00230   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00240   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
00250   bud1=1
00260   sn$="BudRpt1" !:
        fntos(sn$) !:
        let mylen=32 : let mypos=mylen+2
00270   let txt$="Starting Date (blank for all):" !:
        fnlbl(1,1,txt$,mylen,1)
00280   fntxt(1,mypos,8,0,0,"1") !:
        let resp$(1)=""
00290   let txt$="Ending Date (blank for all):" !:
        fnlbl(2,1,txt$,mylen,1)
00300   fntxt(2,mypos,8,0,0,"1") !:
        let resp$(2)=""
00310   fncmdset(3)
00320   fnacs(sn$,0,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   let d1=val(resp$(1)) conv L350
00350 L350: let d2=val(resp$(2)) conv L360
00360 L360: ! ______________________________________________________________________
00370   on fkey 5 goto XIT
00380 ! On Pageoflow Goto NEWPGE
00390   fnopenprn
00400   gosub HEADING
00410   goto READ_BUDMSTR
00420 ! ______________________________________________________________________
00430 READ_BUDMSTR: ! 
00440   read #81,using L490: z$,mat ba,mat badr eof DONE
00450   if env$('client')="Findlay" then ba(8)=0 ! don't show the penalty budget on form
00460   let totba=totalbudget=totactual=0
00470   if ba(12)>0 then let totba=ba(12): goto L490 ! if net bill in budget, use it
00480   for j=2 to 11: let totba=totba+ba(j): next j
00490 L490: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00500   read #1,using L510,key=z$: n$,bal nokey READ_BUDMSTR
00510 L510: form pos 26,c 25,pos 292,pd 4.2
00520   pr #255,using "Form POS 1,C 12,C 25": z$,n$ pageoflow NEWPGE !:
        pr #255: "" pageoflow NEWPGE
00530   pr #255: hdr$ pageoflow NEWPGE
00540   let ta1=badr(1)
00550   mat badr=(0)
00560   bt1=btdue=0
00570 L570: if ta1=0 then goto L810
00580   read #82,using L600,rec=ta1: x$,mat bt1,nba norec L810
00590   if sum(bt1)=0 then goto L800 ! skip any blank records
00600 L600: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
00610   let y=bt1(1,2) : let y=fndate_mmddyy_to_ccyymmdd(y)
00620   let x=0: if d1>0 then let x=d1 : let x=fndate_mmddyy_to_ccyymmdd(x)
00630   if y<x then goto L800
00640   let x=0: if d2>0 then let x=d2 : let x=fndate_mmddyy_to_ccyymmdd(x) else goto L660
00650   if y>x then goto L800
00660 L660: service=1
00670   let t1(1)=bt1(1,1) ! date
00680   for j=1 to 10
00690     if trim$(servicename$(j))="" then goto L720
00700     service=service+1
00710     let t1(service)=bt1(j+1,2) !:
          let totalbudget=totalbudget+bt1(j+1,1) !:
          let totactual=totactual+bt1(j+1,2)
00720 L720: next j
00730   let t1(service+1)=bt1(12,2)
00740   pr #255,using "Form POS 1,PIC(ZZ/ZZ/ZZ),TOTSERV1*N 10.2": mat t1 pageoflow NEWPGE
00750   let t1(1)=0
00760   mat t2=t2+t1 : mat t3=t3+t1
00770   let t2+=1 : let t3+=1
00780   if bt1(14,1)=0 then bt1=bt1+1: else goto L800 !:
          ! Total Budget Bills Not Paid
00790   for j=1 to 10 : btdue=btdue+bt1(j+1,1) : next j
00800 L800: let ta1=nba : goto L570
00810 L810: pr #255: underline$ ! "{\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }" Pageoflow NEWPGE
00820   pr #255,using "Form POS 1,C 7,PIC(Z),TOTSERV1*N 10.2": "Total",mat t2 pageoflow NEWPGE
00830   if t2<2 then goto L870
00840   for j=1 to udim(t2)
00850     if t2(j)>0 then let t2(j)=t2(j)/t2
00860   next j
00870 L870: pr #255: underline$ ! "{\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }" Pageoflow NEWPGE
00880   pr #255,using "Form POS 1,C 7,PIC(Z),TOTSERV1*N 10.2": "Average",mat t2 pageoflow NEWPGE
00890   pr #255: "" pageoflow NEWPGE
00900   budget$="Current Budget Amounts: "
00910   for j=1 to 10
00920     if trim$(servicename$(j))="" then goto L950
00930     if ba(j+1)=0 then goto L950
00940     budget$=budget$&"  "&trim$(servicename$(j))&"="&trim$(cnvrt$("pic($$$$$.$$",ba(j+1)))
00950 L950: next j
00960   pr #255: budget$ pageoflow NEWPGE
00970   pr #255: "Current Balance:"&cnvrt$("PIC($$$,$$$.$$ CR",bal) pageoflow NEWPGE
00980   pr #255: "Number of Budget Payments Not Received:"&cnvrt$("PIC(ZZ#)",bt1) pageoflow NEWPGE
00990   pr #255: "Total Budget Payments Not Received:"&cnvrt$("PIC($$$,$$$.$$ CR",btdue) pageoflow NEWPGE
01000   pr #255: "Excess Budget Billings Over Actual Billing (Under=Cr):"&cnvrt$("PIC($$$,$$$.$$ CR",(totalbudget-totactual)) pageoflow NEWPGE
01010   pr #255, using "Form Pos 1,C 78": "{\ul \strike "&rpt$(" ",58)&"}" pageoflow NEWPGE
01020   mat t2=(0) : let t2=0
01030   goto READ_BUDMSTR
01040 ! ______________________________________________________________________
01050 NEWPGE: ! !:
        pr #255: newpage !:
        gosub HEADING !:
        continue 
01060 ! ______________________________________________________________________
01070 HEADING: ! 
01080   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01090   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
01100   pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zz/zz/zz)",d1)&"To: "&cnvrt$("pic(zz/zz/zz)",d2)&"}"
01110   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
01120   pr #255: ""
01130   return 
01140 ! ______________________________________________________________________
01150 DONE: ! 
01160   fncloseprn
01170 XIT: let fnxit
01180 ! ______________________________________________________________________
01190 ! <Updateable Region: ERTN>
01200 ERTN: let fnerror(program$,err,line,act$,"xit")
01210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01240 ERTN_EXEC_ACT: execute act$ : goto ERTN
01250 ! /region
01260 ! ______________________________________________________________________
