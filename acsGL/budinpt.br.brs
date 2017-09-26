00010 ! Replace S:\acsGL\BudInpt
00020 ! used to enter new budget figures at beginning of new year
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnrgl$,fnerror,fnwait,fnoldmsgbox,fnsearch,fnopenprn,fncloseprn,fntos,fnfra,fnopt,fncmdset,fnacs,fnagl$,fnlbl,fnqgl,fntxt
00050   on error goto ERTN
00060   let fntop(program$,"Budget Amounts")
00070 ! ______________________________________________________________________
00080   dim dat$*20,bm(13),io1$(4),in1(4),fl2$(2),sc2$(2)*50
00090   dim revb(13),resp$(10)*256
00100   dim heading$*70,form$*80,numeric_format$*20,selection$*70
00110 ! ______________________________________________________________________
00130 ! 
00140   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative  
00142   read #1,using 'Form Pos 384,N 2',rec=1: nap 
00144   close #1: 
00150 !
00160   let fntos(sn$="BudgetAmount") 
00162   let mylen=50: let mypos=mylen+3 : let right=1
00170   let fnfra(1,1,2,55,"Method of Budget Entry"," ",0)
00180   let fnopt(1,3,"Enter new Budget amounts",0,1) 
00182   let resp$(rc+=1)="True"
00190   let fnopt(2,3,"Pull Budget from Budget Management System",0,1) 
00192   let resp$(rc+=1)="False"
00200   let fnfra(5,1,3,55,"Method of Allocating Budget"," ",0)
00210   let fnopt(1,3,"Divide new budget evenly between months",0,2) 
00212   let resp$(rc+=1)="True"
00220   let fnopt(2,3,"Allocate full amount to month 12",0,2) 
00222   let resp$(rc+=1)="False"
00230   let fnopt(3,3,"Allocate full amount to month 1",0,2) 
00232    let resp$(rc+=1)="False"
00240   let fncmdset(2)
00250 ! 
00260   let fnacs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   if resp$(1)="True" then let method=1 else let method=2
00290   if resp$(3)="True" then let method_to_allocate=1 ! divide evenly
00300   if resp$(4)="True" then let method_to_allocate=2 ! all to month 12
00310   if resp$(5)="True" then let method_to_allocate=3 ! all to month 1
00320   if method=2 then gosub L790
00330   if method=2 then gosub BUDGET_FILE_NUM
00340   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00350   let fnopenprn
00370 ! 
00380   print #255: "   GL Number    New Budget  Old Budget"
00390   print #255: "--------------  ----------  ----------"
00400 ENTER_BUDGET: ! 
00410   if method=2 then goto L1090
00420   let fntos(sn$="BudgetAmount2") 
00422   let mylen=25: let mypos=mylen+3 : let right=1
00430   let k$="": read #1,using L440: k$ ioerr L450 eof L450 ! try to read next account
00440 L440: form pos 1,c 12
00450 L450: let fnlbl(1,1," General Ledger Number:",mylen,right)
00460   let fnqgl(1,mypos,0,2) 
00462   let resp$(1)=fnrgl$(k$)
00470   let fnlbl(2,1," Budget Amount:",mylen,right)
00480   let fntxt(2,mypos,12,0,1,"10",0,"Enter the total budget amount for this account.  Use negative amounts on revenues (any negative balance accounts).") 
00482   let resp$(2)=""
00490   let fncmdset(2)
00500   let fnacs(sn$,0,mat resp$,ckey)
00510   if ckey=5 then goto XIT
00520   let k$=fnagl$(resp$(1))
00530   let budgetamt=val(resp$(2)) ! new budget amount
00540   read #1,using L550,key=k$: mat bm,mat revb
00550 L550: form pos 249,13*pd 6.2,pos 339,13*pd 6.2
00560 L560: let o1=sum(bm)
00570   mat bm=(0)
00580   if method_to_allocate=2 then let bm(12)=budgetamt: goto L650 ! allocate all to month 12
00590   if method_to_allocate=3 then let bm(1)=budgetamt: goto L650 ! allocate all to month 1
00600   let m1=round(budgetamt/nap,2)
00610   for j=1 to nap-1
00620     let bm(j)=m1
00630   next j
00640   let bm(j)=budgetamt-sum(bm)
00650 L650: mat revb=bm
00660   print #255,using L670: mat in1,o1
00670 L670: form pos 1,pic(zzz),n 7,pic(zzzz),2*n 12.2,skip 1
00680   if budyear=1 then rewrite #1,using L690,key=k$: mat revb: goto L710 ! only rewrite the revised budget figures if record only changes
00690 L690: form pos 339,13*pd 6.2
00700   rewrite #1,using L550,key=k$: mat bm,mat revb
00710 L710: goto ENTER_BUDGET
00720 ! ______________________________________________________________________
00730 EOF2: ! 
00740   let fncloseprn
00750 ! 
00760 ! 
00770 XIT: let fnxit
00780 ! ______________________________________________________________________
00790 L790: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)
00820 ! 
00830   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative  
00832   read #1,using 'Form Pos 384,N 2',rec=1: nap 
00834   close #1: 
00840   print newpage
00850   let fntos(sn$="BudgetAmount3") 
00852   let mylen=50: let mypos=mylen+3 : let right=1
00860   let fnfra(1,1,2,55,"Update Current Budget or New Budget"," ",0)
00870   let fnopt(1,3,"Update Current Budget for Changes",0,1) 
00872   let resp$(rc+=1)="True"
00880   let fnopt(2,3,"Update for New Budget Year",0,1) 
00882   let resp$(rc+=1)="False"
00890 ! 
00900   let fncmdset(2)
00910 ! 
00920   let fnacs(sn$,0,mat resp$,ckey)
00930   if ckey=5 then goto XIT
00940   if resp$(1)="True" then let budyear=1 else let budyear=2
00950   if budyear=1 then let p1=37 else let p1=43 ! CURRENT YEARS BUDGET OR NEXT YEARS BUDGET
00960   return 
00970 ! ______________________________________________________________________
00980 BUDGET_FILE_NUM: ! r:
00990   let fntos(sn$="BudgetAmount4") 
00992   let mylen=50: let mypos=mylen+3 : let right=1
01000   let fnlbl(1,1," Budget File Number to Pull:",mylen,right)
01010   let fntxt(1,mypos,3,0,1,"30",0,"You can have different budget files in the budget management system.  Enter the budget file you wish to pull.") 
01012   let resp$(1)=""
01020   let fncmdset(2)
01030   let fnacs(sn$,0,mat resp$,ckey)
01040   if ckey=5 then goto XIT
01050   let bud=val(resp$(1)) ! budget file number to pull
01060   open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".h"&env$('cno')&",Shr",internal,outin,keyed ioerr BUDGET_FILE_NUM
01070 return ! /r
01080 ! ______________________________________________________________________
01090 L1090: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)
01100 L1100: read #2,using 'Form POS 1,N 3,N 6,N 3,POS P1,PD 6.2,POS 149,C 1': mat in1,cd$ eof EOF2
01110   if cd$<>"B" then goto L1100
01120   let k$=cnvrt$("N 3",in1(1))&cnvrt$("N 6",in1(2))&cnvrt$("N 3",in1(3))
01130   rewrite #2,using L1140: 0 ! SET CHANGES TO ZERO IN BUDGET WORKFILE
01140 L1140: form pos 31,pd 6.2
01150   read #1,using L550,key=k$: mat bm,mat revb nokey L1100
01160   goto L560
01170 ! ______________________________________________________________________
01180 ! ______________________________________________________________________
01190 ! <Updateable Region: ERTN>
01200 ERTN: let fnerror(program$,err,line,act$,"xit")
01210   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01230   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01240 ERTN_EXEC_ACT: execute act$ : goto ERTN
01250 ! /region
