00010 ! Replace S:\acsGL\Company
00020 ! GL Company Information File Editor
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnerror,fnwin3b,fnchain,fnstyp,fnTos,fnLbl,fnTxt,fnqgl,fnagl$,fnAcs,fnCmdKey,fnChk,fnrgl$,fncomboa
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,actrcde$*1
00080   dim rpnames$(86)*20,rpnames2$(10)*6,x$(10)*20,rpscr1$(90)*24,na$(125)*8
00090   dim miscname$(10)*20,dedcode(10),dedfed(10),dedfica(10),dedst(10)
00100   dim x1(3),x2(3),x3(3),x4(3),x5(3),x6(3),x7(3),x8(3),x9(3),x10(3),sck(4)
00110   dim io1$(18),sc1$(16)*54,fl1$(16),io2$(7)*20,misc$(90)*20,resp$(100)*40
00120   dim prgl(5,3),fl3$(13),sc3$(11)*47,io3$(27),cap$*128,deduc(10)
00130   dim miscgl$(10)*12,option$(2)*30,gl$(5)*12,option2$(2)
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="Company Information")
00160   gltyp=7
00180   fnstyp(0)
00190 ! ____________________________________________________
00200   open #glmstr=11: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outIn,keyed ioerr L220
00210 ! ____________________
00220 L220: open #20: "Name="&env$('Q')&"\GLmstr\GLBucket.h"&env$('cno')&",RecL=1,Use",internal,outIn,relative 
00230   if lrec(20)=0 then !:
          write #20,using 'Form POS 1,N 1',rec=1: 1
00240   read #20,using 'Form POS 1,N 1',rec=1: glb
00250   close #20: 
00260 ! ______________________________________________________________________
00270   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outIn,relative ioerr BLD_COINFO
00280   goto READ_COINFO
00290 ! ____________________
00300 COINFO_READ_ERR: close #company: ioerr BLD_COINFO : goto BLD_COINFO
00310 ! ____________________
00320 BLD_COINFO: ! 
00330   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",RecL=882,Replace",internal,outIn,relative 
00340 COINFO_WRITE: ! !:
        write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00350 ! ____________________
00360 READ_COINFO: ! !:
        read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$ conv COINFO_WRITE, ioerr COINFO_READ_ERR
00370 ! ______________________________________________________________________
00380   if actr$="0" or actr$="1" then actr=val(actr$)
00390   if uprc$(actr$)="Y" then actr=1
00400   if uprc$(actr$)="N" then actr=0
00410   lastgl$=cnvrt$("pic(zz#)",a1)&cnvrt$("pic(zzzzz#)",a2)&cnvrt$("pic(zz#)",a3)
00420 SCREEN_1: ! 
00430   fnTos(sn$="Company-1") !:
        mylen=30: mypos=mylen+3 : right=1
00440   fnLbl(1,30,"Company # "&env$('cno'))
00450   fnLbl(3,1,"Company Name:",mylen,right)
00460   fnTxt(3,mypos,40,0,left,"",0,"",0 ) !:
        resp$(1)=a$(1)
00470   fnLbl(4,1,"Company Address:",mylen,right)
00480   fnTxt(4,mypos,40,0,left,"",0,"",0 ) !:
        resp$(2)=a$(2)
00490   fnLbl(5,1,"City, State, Zip:",mylen,right)
00500   fnTxt(5,mypos,40,0,left,"",0,"",0 ) !:
        resp$(3)=a$(3)
00510   fnLbl(6,1,"Federal ID #:",mylen,right)
00520   fnTxt(6,mypos,12,0,left,"",0,"",0 ) !:
        resp$(4)=b$(1)
00530   fnLbl(7,1,"State ID #:",mylen,right)
00540   fnTxt(7,mypos,12,0,left,"",0,"",0 ) !:
        resp$(5)=b$(2)
00550   fnLbl(8,1,"State U/C Rate:",mylen,right)
00560   fnTxt(8,mypos,5,0,left,"",0,"",0 ) !:
        resp$(6)=c$
00570   fnLbl(9,1,"State U/C Maximum:",mylen,right)
00580   fnTxt(9,mypos,10,0,left,"10",0,"",0 ) !:
        resp$(7)=str$(ucm)
00590   fnLbl(10,1,"Type of Business:",mylen,right)
00600   fnTxt(10,mypos,30,0,left,"",0,"",0 ) !:
        resp$(8)=tb$
00610   fnLbl(11,1,"Number of Periods:",mylen,right)
00620   fnTxt(11,mypos,2,0,left,"30",0,"",0 ) !:
        resp$(9)=str$(nap)
00630   fnChk(12,60,"Use Department Number Field:",1) !:
        resp$(10)="False"
00640   if d(1)=1 then resp$(10)="True" else resp$(10)="False"
00650   fnChk(13,60,"Use Sub Number Field:",1)
00660   if d(2)=1 then resp$(11)="True" else resp$(11)="False"
00670   fnChk(14,60,"Maintain Accumulated Transactions:",1)
00680   if actr$="Y" or actr$="1" then resp$(12)="True" else resp$(12)="False"
00690   fnChk(15,60,"Utilize Bank Reconciliation:",1)
00700   if recc$="Y" or reccode=1 then resp$(13)="True" else resp$(13)="False"
00710   fnLbl(16,1,"Last Balance Sheet Account #:",mylen,right)
00720   fnqgl(16,mypos,0,2,pas) !:
        resp$(14)=fnrgl$(lastgl$)
00730   fnChk(17,60,"Allocate Expenses to Job Cost:",1)
00740   if jcc$="Y" or jccode=1 then resp$(15)="True" else resp$(15)="False"
00750   fnLbl(18,1,"Posting Method:",mylen,right)
00760   option$(1)="Post Immediately" !:
        option$(2)="Retain in Holding Files"
00770   fncomboa("PostMethod",18,mypos,mat option$,"Normally you would post immediately. You would only consider posting to holding files if the general ledger is months behind.",mylen)
00780   if glb=1 or glb=0 then resp$(16)=option$(1) else resp$(16)=option$(2)
00790   fnCmdKey("&Next",1,1,0,"Moves to 2nd screen of company information.")
00800   fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
00810   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
00820   fnAcs(sn$,0,mat resp$,ckey)
00830   if ckey=5 then goto XIT
00840   a$(1)=resp$(1)
00850   a$(2)=resp$(2)
00860   a$(3)=resp$(3)
00870   b$(1)=resp$(4)
00880   b$(2)=resp$(5)
00890   c$=resp$(6)
00900   ucm=val(resp$(7))
00910   tb$=resp$(8)
00920   nap=val(resp$(9))
00930   if resp$(10)="True" then d1$="Y": d(1)=1 else d1$="N": d(1)=0
00940   if resp$(11)="True" then d2$="Y": d(2)=1 else d2$="N": d(2)=0
00950   if resp$(12)="True" then actr$="Y": actr=1 else actr$="N": actr=0
00960   if resp$(13)="True" then recc$="Y": reccode=1 else recc$="N": reccode=0
00970   lastgl$=fnagl$(resp$(14)) ! gl number
00980   a1=val(lastgl$(1:3)) !:
        a2=val(lastgl$(4:9)) !:
        a3=val(lastgl$(10:12))
00990   if resp$(15)="True" then jcc$="Y": jccode=1 else jcc$="N": jccode=0
01000   if resp$(16)=option$(1) then glb$="P": glb=1 else glb$="R": glb=2
01010   if ckey=4 then gosub SAVE : goto XIT !  save and exit
01020   goto SCREEN_2
01030 ! ______________________________________________________________________
01040 SCREEN_2: ! 
01050   fnTos(sn$="Company-2") !:
        mylen=30: mypos=mylen+3 : right=1
01060   fnLbl(1,1,"   The system will allow you to summarize the Payroll Withholding entries into",85,left)
01070   fnLbl(2,1,"one entry for each Withholding Account on your Trial Balance.  If you wish",85,left)
01080   fnLbl(3,1,"to utilize this option, enter the Account Numbers, otherwise leave both numbers",85,left)
01090   fnLbl(4,1,"as blank to pr all details.",85,left)
01100 ! BEGCNT,BEGACCT,BEGSUB,ENDCNT,ENDACCT,ENCSUB
01110   fnLbl(6,1,"First Account to summarize:",mylen,right)
01120   fnqgl(6,mypos,0,2,pas) !:
        resp$(1)=fnrgl$(e$(1))
01130   fnLbl(7,1,"Last Account to summarize:",mylen,right)
01140   fnqgl(7,mypos,0,2,pas) !:
        resp$(2)=fnrgl$(e$(2))
01150   fnCmdKey("&Next",1,1,0,"Moves to 3nd screen of company information.")
01160   fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
01170   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
01180   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
01190   fnAcs(sn$,0,mat resp$,ckey)
01200   e$(1)=fnagl$(resp$(1)) ! Summary # 1
01210   e$(2)=fnagl$(resp$(2)) ! Summary # 1
01220   if ckey=4 then gosub SAVE : goto XIT
01230   if ckey=2 then goto SCREEN_1
01240   goto SCREEN_3
01250 ! ______________________________________________________________________
01260 SCREEN_3: ! 
01270   for j=1 to 5
01280     gl$(j)=cnvrt$("pic(zz#)",prgl(j,1))&cnvrt$("pic(zzzzz#)",prgl(j,2))&cnvrt$("pic(zz#)",prgl(j,3))
01290   next j
01300 ! FICARATE,FICAWAGE,FEDUCRAT,FEDUCWAG,MCR,MCM,MAT PRGL
01310   fnTos(sn$="Company-3") 
01312   mylen=32: mypos=mylen+3 : right=1
01320   fnLbl(1,25,"After-the Fact Payroll Information")
01330   fnLbl(3,1,"Social Security Rate:",mylen,right)
01340   fnTxt(3,mypos,8,0,left,"34",0,"Format would be 6.2 ",0 ) 
01342   resp$(1)=str$(ficarate)
01350   fnLbl(4,1,"Social Security Maximum Wage:",mylen,right)
01360   fnTxt(4,mypos,12,0,left,"10",0,"Example would be 90000.00 ",0 ) 
01362   resp$(2)=str$(ficawage)
01370   fnLbl(5,1,"Federal U/C Rate:",mylen,right)
01380   fnTxt(5,mypos,8,0,left,"34",0,"Example would be .800 ",0 ) 
01382   resp$(3)=str$(feducrat)
01390   fnLbl(6,1,"Federal U/C Maximum Wage:",mylen,right)
01400   fnTxt(6,mypos,12,0,left,"10",0,"An example of the Federal unemployment compensation rate would be 9000.00 ",0 ) 
01402   resp$(4)=str$(feducwag)
01410   fnLbl(7,1,"Medicare Rate:",mylen,right)
01420   fnTxt(7,mypos,8,0,left,"34",0,"Format would be 1.45",0 ) 
01422   resp$(5)=str$(mcr)
01430   fnLbl(8,1,"Medicare Maximum Wage:",mylen,right)
01440   fnTxt(8,mypos,12,0,left,"10",0,"There is no maximun at this time.  Ener enough 9s to exceed the highest paid employee.  (eg.  9999999.00 ",0 ) 
01442   resp$(6)=str$(mcm)
01450   fnLbl(10,25,"General Ledger Account Numbers")
01460   fnLbl(11,1,"Federal Withholding:",mylen,right)
01470   fnqgl(11,mypos,0,2,pas) 
01472   resp$(7)=fnrgl$(gl$(1))
01480   fnLbl(12,1,"FICA Withholding:",mylen,right)
01490   fnqgl(12,mypos,0,2,pas) 
01492   resp$(8)=fnrgl$(gl$(2))
01500   fnLbl(13,1,"State Withholding:",mylen,right)
01510   fnqgl(13,mypos,0,2,pas) 
01512   resp$(9)=fnrgl$(gl$(3))
01520   fnLbl(14,1,"Local Withholding:",mylen,right)
01530   fnqgl(14,mypos,0,2,pas) !:
        resp$(10)=fnrgl$(gl$(4))
01540   fnLbl(15,1,"Earned Income Credit:",mylen,right)
01550   fnqgl(15,mypos,0,2,pas) !:
        resp$(11)=fnrgl$(gl$(5))
01560   fnCmdKey("&Next",1,1,0,"Moves to 4th screen of company information.")
01570   fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
01580   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
01590   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
01600   fnAcs(sn$,0,mat resp$,ckey)
01610   if ckey=5 then goto XIT
01630   if ckey=2 then goto SCREEN_2
01640   ficarate=val(resp$(1))
01650   ficawage=val(resp$(2))
01660   feducrat=val(resp$(3))
01670   feducwag=val(resp$(4))
01680   mcr=val(resp$(5))
01690   mcm=val(resp$(6))
01700   for j=1 to 5
01710     gl$(j)=fnagl$(resp$(j+6))
01720     prgl(j,1)=val(gl$(j)(1:3)): prgl(j,2)=val(gl$(j)(4:9)): prgl(j,3)=val(gl$(j)(10:12))
01730   next j
01735   if ckey=4 then gosub SAVE : goto XIT
01740 ! ______________________________________________________________________
01750 SCREEN_4: ! 
01760   fnTos(sn$="Company-4") !:
        mylen=32: mypos=mylen+3 : right=1
01770   fnLbl(1,1,"Enter the names of the 10 Miscellaneous Deductions and indicate",90,left)
01780   fnLbl(2,1,"how each deduction is to be handled by the system.  A check",90,left)
01790   fnLbl(3,1,"mark will indicate that the miscellaneous deduction should be ",90,left)
01800   fnLbl(4,1,"subtracted from gross before calculating federal withholding, ",90,left)
01810   fnLbl(5,1,"fica and social security, state withholdings, or state U/C.",80,left)
01820   fnLbl(7,29,"Ded         Ded  Ded   Ded    Ded",40,left)
01830   fnLbl(8,1,"Deduction Name              Add         Fed  FICA  State  U/C     GL Number",80,left)
01840   resp=0
01850   for j=1 to 10
01860     fnTxt(j+8,1,20,0,left,"",0,"Enter you deduction name.",0 ) !:
          resp$(resp+=1)=miscname$(j)
01870     option2$(1)="Deducttion" !:
          option2$(2)="Addition"
01880     fncomboa("MIscdeduct",j+8,26,mat option2$,"Indicate whether the deduction should be deducted from the check or added to the check.",10)
01890     if dedcode(j)=0 then dedcode(j)=1
01900     resp$(resp+=1)=option2$(dedcode(j))
01910     fnChk(j+8,41,"",1)
01920     if dedfed(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01930     fnChk(j+8,47,"",1)
01940     if dedfica(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01950     fnChk(j+8,53,"",1)
01960     if dedst(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01970     fnChk(j+8,59,"",1)
01980     if deduc(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01990     linecount=j+8
02000     fnqgl(linecount,64,0,2,pas) !:
          resp$(resp+=1)=fnrgl$(miscgl$(j))
02010   next j
02020   fnCmdKey("&Next",1,1,0,"Saves changes and returns to main menu.")
02030   fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu.")
02040   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
02050   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
02060   fnAcs(sn$,0,mat resp$,ckey)
02070   if ckey=5 then goto XIT
02080   resp=0
02090   for j=1 to 10
02100     miscname$(j)=resp$(resp+=1)
02110     if resp$(resp+=1)=option2$(1) then dedcode(j)=1 else dedcode(j)=2
02120     if resp$(resp+=1)="True" then dedfed(j)=1 else dedfed(j)=0
02130     if resp$(resp+=1)="True" then dedfica(j)=1 else dedfica(j)=0
02140     if resp$(resp+=1)="True" then dedst(j)=1 else dedst(j)=0
02150     if resp$(resp+=1)="True" then deduc(j)=1 else deduc(j)=0
02160     miscgl$(j)=fnagl$(resp$(resp+=1))
02170   next j
02180   gosub SAVE
02190   close #company: 
02200   goto XIT
02210 ! ______________________________________________________________________
02220 ! ______________________________________________________________________
02230 XIT: fnxit ! fnCHAIN("S:\acsGL\ACglscr")
02300 ! <Updateable Region: ERTN>
02310 ERTN: fnerror(program$,err,line,act$,"xit")
02320   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02330   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02340   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02350 ERTN_EXEC_ACT: execute act$ : goto ERTN
02360 ! /region
02380 SAVE: ! r:
02390   rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
02400   open #20: "Name="&env$('Q')&"\GLmstr\GLBucket.h"&env$('cno')&",RecL=1,Use",internal,outIn,relative  
02402   rewrite #20,using 'Form POS 1,N 1',rec=1: glb 
02404   close #20: 
02410   return ! /r
