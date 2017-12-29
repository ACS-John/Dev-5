00010 ! Replace S:\acsGL\BudInpt
00020 ! used to enter new budget figures at beginning of new year
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnrgl$,fnerror,fnoldmsgbox,fnsearch,fnopenprn,fncloseprn,fnTos,fnFra,fnOpt,fnCmdSet,fnAcs,fnagl$,fnLbl,fnqgl,fnTxt
00050   on error goto ERTN
00060   fntop(program$,"Budget Amounts")
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
00160   fnTos(sn$="BudgetAmount") 
00162   mylen=50: mypos=mylen+3 : right=1
00170   fnFra(1,1,2,55,"Method of Budget Entry"," ",0)
00180   fnOpt(1,3,"Enter new Budget amounts",0,1) 
00182   resp$(rc+=1)="True"
00190   fnOpt(2,3,"Pull Budget from Budget Management System",0,1) 
00192   resp$(rc+=1)="False"
00200   fnFra(5,1,3,55,"Method of Allocating Budget"," ",0)
00210   fnOpt(1,3,"Divide new budget evenly between months",0,2) 
00212   resp$(rc+=1)="True"
00220   fnOpt(2,3,"Allocate full amount to month 12",0,2) 
00222   resp$(rc+=1)="False"
00230   fnOpt(3,3,"Allocate full amount to month 1",0,2) 
00232    resp$(rc+=1)="False"
00240   fnCmdSet(2)
00250 ! 
00260   fnAcs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   if resp$(1)="True" then method=1 else method=2
00290   if resp$(3)="True" then method_to_allocate=1 ! divide evenly
00300   if resp$(4)="True" then method_to_allocate=2 ! all to month 12
00310   if resp$(5)="True" then method_to_allocate=3 ! all to month 1
00320   if method=2 then gosub L790
00330   if method=2 then gosub BUDGET_FILE_NUM
00340   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
00350   fnopenprn
00370 ! 
00380   pr #255: "   GL Number    New Budget  Old Budget"
00390   pr #255: "--------------  ----------  ----------"
00400 ENTER_BUDGET: ! 
00410   if method=2 then goto L1090
00420   fnTos(sn$="BudgetAmount2") 
00422   mylen=25: mypos=mylen+3 : right=1
00430   k$="": read #1,using L440: k$ ioerr L450 eof L450 ! try to read next account
00440 L440: form pos 1,c 12
00450 L450: fnLbl(1,1," General Ledger Number:",mylen,right)
00460   fnqgl(1,mypos,0,2) 
00462   resp$(1)=fnrgl$(k$)
00470   fnLbl(2,1," Budget Amount:",mylen,right)
00480   fnTxt(2,mypos,12,0,1,"10",0,"Enter the total budget amount for this account.  Use negative amounts on revenues (any negative balance accounts).") 
00482   resp$(2)=""
00490   fnCmdSet(2)
00500   fnAcs(sn$,0,mat resp$,ckey)
00510   if ckey=5 then goto XIT
00520   k$=fnagl$(resp$(1))
00530   budgetamt=val(resp$(2)) ! new budget amount
00540   read #1,using L550,key=k$: mat bm,mat revb
00550 L550: form pos 249,13*pd 6.2,pos 339,13*pd 6.2
00560 L560: o1=sum(bm)
00570   mat bm=(0)
00580   if method_to_allocate=2 then bm(12)=budgetamt: goto L650 ! allocate all to month 12
00590   if method_to_allocate=3 then bm(1)=budgetamt: goto L650 ! allocate all to month 1
00600   m1=round(budgetamt/nap,2)
00610   for j=1 to nap-1
00620     bm(j)=m1
00630   next j
00640   bm(j)=budgetamt-sum(bm)
00650 L650: mat revb=bm
00660   pr #255,using L670: mat in1,o1
00670 L670: form pos 1,pic(zzz),n 7,pic(zzzz),2*n 12.2,skip 1
00680   if budyear=1 then rewrite #1,using L690,key=k$: mat revb: goto L710 ! only rewrite the revised budget figures if record only changes
00690 L690: form pos 339,13*pd 6.2
00700   rewrite #1,using L550,key=k$: mat bm,mat revb
00710 L710: goto ENTER_BUDGET
00720 ! ______________________________________________________________________
00730 EOF2: ! 
00740   fncloseprn
00750 ! 
00760 ! 
00770 XIT: fnxit
00780 ! ______________________________________________________________________
00790 L790: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)
00820 ! 
00830   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative  
00832   read #1,using 'Form Pos 384,N 2',rec=1: nap 
00834   close #1: 
00840   pr newpage
00850   fnTos(sn$="BudgetAmount3") 
00852   mylen=50: mypos=mylen+3 : right=1
00860   fnFra(1,1,2,55,"Update Current Budget or New Budget"," ",0)
00870   fnOpt(1,3,"Update Current Budget for Changes",0,1) 
00872   resp$(rc+=1)="True"
00880   fnOpt(2,3,"Update for New Budget Year",0,1) 
00882   resp$(rc+=1)="False"
00890 ! 
00900   fnCmdSet(2)
00910 ! 
00920   fnAcs(sn$,0,mat resp$,ckey)
00930   if ckey=5 then goto XIT
00940   if resp$(1)="True" then budyear=1 else budyear=2
00950   if budyear=1 then p1=37 else p1=43 ! CURRENT YEARS BUDGET OR NEXT YEARS BUDGET
00960   return 
00970 ! ______________________________________________________________________
00980 BUDGET_FILE_NUM: ! r:
00990   fnTos(sn$="BudgetAmount4") 
00992   mylen=50: mypos=mylen+3 : right=1
01000   fnLbl(1,1," Budget File Number to Pull:",mylen,right)
01010   fnTxt(1,mypos,3,0,1,"30",0,"You can have different budget files in the budget management system.  Enter the budget file you wish to pull.") 
01012   resp$(1)=""
01020   fnCmdSet(2)
01030   fnAcs(sn$,0,mat resp$,ckey)
01040   if ckey=5 then goto XIT
01050   bud=val(resp$(1)) ! budget file number to pull
01060   open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".h"&env$('cno')&",Shr",internal,outIn,keyed ioerr BUDGET_FILE_NUM
01070 return ! /r
01080 ! ______________________________________________________________________
01090 L1090: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)
01100 L1100: read #2,using 'Form POS 1,N 3,N 6,N 3,POS P1,PD 6.2,POS 149,C 1': mat in1,cd$ eof EOF2
01110   if cd$<>"B" then goto L1100
01120   k$=cnvrt$("N 3",in1(1))&cnvrt$("N 6",in1(2))&cnvrt$("N 3",in1(3))
01130   rewrite #2,using L1140: 0 ! SET CHANGES TO ZERO IN BUDGET WORKFILE
01140 L1140: form pos 31,pd 6.2
01150   read #1,using L550,key=k$: mat bm,mat revb nokey L1100
01160   goto L560
01170 ! ______________________________________________________________________
01180 ! ______________________________________________________________________
01190 ! <Updateable Region: ERTN>
01200 ERTN: fnerror(program$,err,line,act$,"xit")
01210   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01240 ERTN_EXEC_ACT: execute act$ : goto ERTN
01250 ! /region
