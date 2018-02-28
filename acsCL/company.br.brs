00010 ! Replace S:\acsCL\Company
00020 ! maintain company information file for checkbook management
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fncursys$,fnTos,fnLbl,fnAcs,fnCmdSet,fnTxt,fncombof,fnChk,fnButton,fnFra,fncomboa,fnqgl,fnrgl$,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,actrcde$*1
00080   dim cap$*128,prgl(5,3),d$(2)*1
00090   dim miscname$(10)*20,dedcode(10),dedfed(10),dedfica(10),dedst(10)
00100   dim deduc(10),miscgl$(10)*12,actr$*1,reccode$*1,resp$(150)*40
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Company Information")
00130   fncno(cno)
00140   cancel=99 : right=1 : center=2 : left=0 !:
        ccyymmdd$='3' : mmddyy$='1' : on=1 : off=0 !:
        cancel=5 : save=1 : limit_to_list=1 : pointtwo$='32' !:
        pointthree$='33'
00150   open #glmstr=11: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr L160
00160 L160: open #company=1: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative ioerr BUILD_COMPANY
00170   goto READ_COMPANY
00180 ! ______________________________________________________________________
00190 READ_COMPANY: ! 
00200   read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00210   if actr$="0" or actr$="1" then actr=val(actr$)
00220   if uprc$(actr$)="Y" then actr=1 else !:
          if uprc$(actr$)="N" then actr=0
00230   gosub NEWSCREEN !:
        if ckey=save then gosub SAVE : goto XIT else !:
          if ckey=cancel then goto XIT !:
            ! to do it the old way change this whole line to read  GoTo Screen1
00240   if ckey=page1 then page=1 : gosub NEWSCREEN else !:
          if ckey=page2 then page=2 : gosub NEWSCREEN else !:
            if ckey=page3 then page=3 : gosub NEWSCREEN else !:
              if ckey=page4 then page=4 : gosub NEWSCREEN else !:
                if ckey=save then gosub SAVE : goto XIT else !:
                  if ckey=cancel then goto XIT !:
                    ! to do it the old way change this whole line to read  GoTo Screen1
00250 ! ______________________________________________________________________
00260 NEWSCREEN: ! 
00270   fnTos(sn$='Company-Pg'&str$(page)) !:
        lc=0
00280   page1=6 : page2=07 : page3=08 : page4=09
00290   fnButton(1,01,'&Basic            ',page1,'',height=02,23) !:
        fnButton(1,26,'&General Ledger   ',page2,'',height=2,23) !:
        fnButton(1,51,'&Rates and Maxs   ',page3,'',height=2,23) !:
        fnButton(1,76,'&Deductions       ',page4,'',height=2,23)
00300   if page=0 then page=1
00310   if page=1 then gosub PAGE1 else !:
          if page=2 then gosub PAGE2 else !:
            if page=3 then gosub PAGE3 else !:
              if page=4 then gosub PAGE4
00320   fnCmdSet(4) ! Save and Cancel
00330   fnAcs(sn$,0,mat resp$,ckey)
00340   if page=1 then 
00350     a$(1)=resp$(1) !:
          a$(2)=resp$(2) !:
          a$(3)=resp$(3) !:
          b$(1)=resp$(4) !:
          b$(2)=resp$(5) !:
          tb$=resp$(6) !:
          nap=val(resp$(7)) !:
          wbc=val(resp$(8)(1:2))
00360     if resp$(9)='True' then prenum=1 else prenum=2
00370     if resp$(10)='True' then reccode=1 else reccode=0
00380   end if 
00390   if page=2 then 
00400     if resp$(1)='True' then d(1)=1 else d(1)=0
00410     if resp$(2)='True' then d(2)=1 else d(2)=0
00420     lastact$=fnagl$(resp$(3))
00430     if resp$(4)='True' then ar1=1 else ar1=0
00440     resp$(5)=fnagl$(resp$(5)) !:
          prgl(1,1)=val(resp$(5)(1:3)) !:
          prgl(1,2)=val(resp$(5)(4:9)) !:
          prgl(1,3)=val(resp$(5)(10:12))
00450     resp$(6)=fnagl$(resp$(6)) !:
          prgl(2,1)=val(resp$(6)(1:3)) !:
          prgl(2,2)=val(resp$(6)(4:9)) !:
          prgl(2,3)=val(resp$(6)(10:12))
00460     resp$(7)=fnagl$(resp$(7)) !:
          prgl(3,1)=val(resp$(7)(1:3)) !:
          prgl(3,2)=val(resp$(7)(4:9)) !:
          prgl(3,3)=val(resp$(7)(10:12))
00470     resp$(8)=fnagl$(resp$(8)) !:
          prgl(4,1)=val(resp$(8)(1:3)) !:
          prgl(4,2)=val(resp$(8)(4:9)) !:
          prgl(4,3)=val(resp$(8)(10:12))
00480     resp$(9)=fnagl$(resp$(9)) !:
          prgl(5,1)=val(resp$(9)(1:3)) !:
          prgl(5,2)=val(resp$(9)(4:9)) !:
          prgl(5,3)=val(resp$(9)(10:12))
00490   end if 
00500   if page=3 then 
00510     c$=resp$(1) !:
          udm=val(resp$(2)) !:
          ficarate=val(resp$(3)) !:
          ficawage=val(resp$(4)) !:
          feducrat=val(resp$(5)) !:
          feducwag=val(resp$(6)) !:
          mcr=val(resp$(7)) !:
          mcm=val(resp$(8))
00520   end if 
00530   if page=4 then 
00540     rc=0
00550     for j=1 to 10
00560       miscname$(j)=resp$(rc+=1)
00570       rc+=1 : if resp$(rc)=item$(1) then dedcode(j)=1 else dedcode(j)=2
00580       if resp$(rc+=1)='True' then dedfed(j)=1 else dedfed(j)=0
00590       if resp$(rc+=1)='True' then dedfica(j)=1 else dedfica(j)=0
00600       if resp$(rc+=1)='True' then dedst(j)=1 else dedst(j)=0
00610       if resp$(rc+=1)='True' then deduc(j)=1 else deduc(j)=0
00615       rc+=1: resp$(rc)=fnagl$(resp$(rc))
00620       miscgl$(j)=resp$(rc)(1:12)
00630     next j
00640   end if 
00650 ! 
00660   if ckey=page1 then page=1 else !:
          if ckey=page2 then page=2 else !:
            if ckey=page3 then page=3 else !:
              if ckey=page4 then page=4 else !:
                if ckey=save then gosub SAVE : goto XIT else !:
                  if ckey=cancel then goto XIT
00670   goto NEWSCREEN
00680 PAGE1: ! _____________________________________________________________ !:
        lc=3 : mylen=40 : mypos=mylen+2
00690   fnLbl(lc+=1,1,'Company Name:',mylen,right)
00700   fnTxt(lc,mypos,40,0,left) !:
        resp$(1)=a$(1)
00710   fnLbl(lc+=1,1,'Address:',mylen,right)
00720   fnTxt(lc,mypos,40,0,left) !:
        resp$(2)=a$(2)
00730   fnLbl(lc+=1,1,'City State and Zip Code:',mylen,right)
00740   fnTxt(lc,mypos,40,0,left) !:
        resp$(3)=a$(3)
00750   fnLbl(lc+=1,1,'Federal Identification Number:',mylen,right)
00760   fnTxt(lc,mypos,12,0,left) !:
        resp$(4)=b$(1)
00770   fnLbl(lc+=1,1,'State Identification Number:',mylen,right)
00780   fnTxt(lc,mypos,12,0,left) !:
        resp$(5)=b$(2)
00790   fnLbl(lc+=1,1,'Type of Business:',mylen,right)
00800   fnTxt(lc,mypos,30,0,left) !:
        resp$(6)=tb$
00810   fnLbl(lc+=1,1,'Number of Periods:',mylen,right)
00820   fnTxt(lc,mypos,30,0,left,number$) !:
        resp$(7)=str$(nap)
00830   fnLbl(lc+=1,1,'Working Bank:',mylen,right)
00840   fncombof('bank',lc,mypos,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list) !:
        resp$(8)=str$(wbc)
00850   fnChk(lc+=1,mypos,'My Checks are Pre-Numbered',right) !:
        if prenum=1 then resp$(9)='True' else resp$(9)='False'
00860   fnChk(lc+=1,mypos,'Utilize Bank Reconciliation Features',right) !:
        if reccode=1 then resp$(10)='True' else resp$(10)='False'
00870   return 
00880 PAGE2: ! _____________________________________________________________ !:
        lc=3 : mylen=40 : mypos=mylen+2 !:
        fc=0 ! framecount
00890   fnFra(04,1,5,framewidth=110,'General Ledger') !:
        frame=fc+=1 : lc=0
00900   fnChk(lc+=1,mypos,'Utilize Department Number Field',right,frame) !:
        if d(1)=1 then resp$(1)='True' else resp$(1)='False'
00910   fnChk(lc+=1,mypos,'Utilize Sub Account Number Field',right,frame) !:
        if d(2)=1 then resp$(2)='True' else resp$(2)='False'
00920   fnLbl(lc+=1,1,'Last Balance Sheet Account Number:',mylen,right,0,frame)
00930 ! fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        ! rESP$(3)=STR$(WBC)
00932   fnqgl(lc,mypos,frame,2) !:
        resp$(3)=fnrgl$(lastact$)
00940   fnFra(11,1,2,framewidth,'Accounts Receivable') !:
        frame=fc+=1 : lc=0
00950   fnChk(lc+=1,mypos,'Post Deposits from Accounts Receivable',right,frame) !:
        if ar1=1 then resp$(4)='True' else resp$(4)='False'
00960   fnFra(15,1,5,framewidth,'Payroll') !:
        frame=fc+=1 : lc=0 : mylen=32 : mypos=mylen+2
00970   fnLbl(lc+=1,1,'FICA Withholding GL Account:',mylen,right,0,frame)
00980 ! fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        ! rESP$(5)=CNVRT$('pic(zz#)',PRGL(1,1))&CNVRT$('pic(zzzzz#)',PRGL(1,2))&CNVRT$('pic(zz#)',PRGL(1,3))
00982   fnqgl(lc,mypos,frame,2) !:
        resp$(5)=cnvrt$('pic(zz#)',prgl(1,1))&cnvrt$('pic(zzzzz#)',prgl(1,2))&cnvrt$('pic(zz#)',prgl(1,3)) !:
        resp$(5)=fnrgl$(resp$(5))
00990   fnLbl(lc+=1,1,'Federal Withholding GL Account:',mylen,right,0,frame)
01000 ! fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        ! rESP$(6)=CNVRT$('pic(zz#)',PRGL(2,1))&CNVRT$('pic(zzzzz#)',PRGL(2,2))&CNVRT$('pic(zz#)',PRGL(2,3))
01002   fnqgl(lc,mypos,frame,2) !:
        resp$(6)=cnvrt$('pic(zz#)',prgl(2,1))&cnvrt$('pic(zzzzz#)',prgl(2,2))&cnvrt$('pic(zz#)',prgl(2,3)) !:
        resp$(6)=fnrgl$(resp$(6))
01010   fnLbl(lc+=1,1,'State Withholding GL Account:',mylen,right,0,frame)
01020 ! fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        ! rESP$(7)=CNVRT$('pic(zz#)',PRGL(3,1))&CNVRT$('pic(zzzzz#)',PRGL(3,2))&CNVRT$('pic(zz#)',PRGL(3,3))
01022   fnqgl(lc,mypos,frame,2) !:
        resp$(7)=cnvrt$('pic(zz#)',prgl(3,1))&cnvrt$('pic(zzzzz#)',prgl(3,2))&cnvrt$('pic(zz#)',prgl(3,3)) !:
        resp$(7)=fnrgl$(resp$(7))
01030   fnLbl(lc+=1,1,'Local Withholding GL Account:',mylen,right,0,frame)
01040 !  fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        !  rESP$(8)=CNVRT$('pic(zz#)',PRGL(4,1))&CNVRT$('pic(zzzzz#)',PRGL(4,2))&CNVRT$('pic(zz#)',PRGL(4,3))
01042   fnqgl(lc,mypos,frame,2) !:
        resp$(8)=cnvrt$('pic(zz#)',prgl(4,1))&cnvrt$('pic(zzzzz#)',prgl(4,2))&cnvrt$('pic(zz#)',prgl(4,3)) !:
        resp$(8)=fnrgl$(resp$(8))
01050   fnLbl(lc+=1,1,'Earned Income Credit GL Account:',mylen,right,0,frame)
01060 ! fnCOMBOF('[Q]\GLmstr',LC,MYPOS,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,50,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST,0,'',FRAME) !:
        ! rESP$(9)=CNVRT$('pic(zz#)',PRGL(5,1))&CNVRT$('pic(zzzzz#)',PRGL(5,2))&CNVRT$('pic(zz#)',PRGL(5,3))
01062   fnqgl(lc,mypos,frame,2) !:
        resp$(9)=cnvrt$('pic(zz#)',prgl(5,1))&cnvrt$('pic(zzzzz#)',prgl(5,2))&cnvrt$('pic(zz#)',prgl(5,3)) !:
        resp$(9)=fnrgl$(resp$(9))
01070   return 
01080 PAGE3: ! _____________________________________________________________ !:
        lc=3 : mylen=44 : mypos=mylen+2 !:
        fc=0 ! frame count
01090   fnFra(04,1,2,framewidth=110,'State Unemployment Compensation') !:
        frame=fc+=1 : lc=0
01100   fnLbl(lc+=1,1,'State Unemployment Compensation Rate:',mylen,right,0,frame)
01110   fnTxt(lc,mypos,5,0,left,'',0,'',frame) !:
        resp$(1)=c$
01120   fnLbl(lc+=1,1,'State Unemployment Compensation Maximum:',mylen,right,0,frame)
01130   fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame) !:
        resp$(2)=str$(ucm)
01140   fnFra(08,1,2,framewidth=110,'Social Security') !:
        frame=fc+=1 : lc=0
01150   fnLbl(lc+=1,1,'Social Security Rate:',mylen,right,0,frame)
01160   fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame) !:
        resp$(3)=str$(ficarate)
01170   fnLbl(lc+=1,1,'Social Security Maximum:',mylen,right,0,frame)
01180   fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame) !:
        resp$(4)=str$(ficawage)
01190   fnFra(12,1,2,framewidth=110,'Federal Unemployment Compensation') !:
        frame=fc+=1 : lc=0
01200   fnLbl(lc+=1,1,'Federal Unemployment Compensation Rate:',mylen,right,0,frame)
01210   fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame) !:
        resp$(5)=str$(feducrat)
01220   fnLbl(lc+=1,1,'Federal Unemployment Compensation Maximum:',mylen,right,0,frame)
01230   fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame) !:
        resp$(6)=str$(feducwag)
01240   fnFra(16,1,2,framewidth=110,'MediCare') !:
        frame=fc+=1 : lc=0
01250   fnLbl(lc+=1,1,'MediCare Rate:',mylen,right,0,frame)
01260   fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame) !:
        resp$(7)=str$(mcr)
01270   fnLbl(lc+=1,1,'MediCare Maximum:',mylen,right,0,frame)
01280   fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame) !:
        resp$(8)=str$(mcm)
01290   return 
01300 PAGE4: ! _____________________________________________________________ !:
        lc=3 : mylen=40 : mypos=mylen+2 !:
        rc=0 ! Resp$ Counter
01310   fnLbl(lc+=1,1,'Enter the names of the 10 miscellaneous deductions.',width,center)
01320   fnLbl(lc+=1,1,'Indicate how the deductions are to be handled by the system.',width,center)
01330   fnLbl(lc+=1,1,'Place a Check in the appropriate column to indicate if it should be',width,center)
01340   fnLbl(lc+=1,1,'deducted for Federal, FICA, State, or State Unemployment Compensation.',width,center)
01350 ! 
01360   fnLbl(lc+=1,24,'Deduction or Addition',10,center)
01370   fnLbl(lc+=1,1,'Deduction Name')
01380   fnLbl(lc,40,'Fed')
01390   fnLbl(lc,45,'FICA')
01400   fnLbl(lc,50,'State')
01410   fnLbl(lc,56,'UC')
01420   fnLbl(lc,60,'General Ledger Number')
01430   for j=1 to 10
01440     fnTxt(j+lc,1,20) !:
          resp$(rc+=1)=miscname$(j)
01450     item$(1)='Deduction' : item$(2)='Addition' : mat item$(2) !:
          fncomboa('ded_or_add',j+lc,24,mat item$) !:
          resp$(rc+=1)=item$(dedcode(j))
01460     fnChk(j+lc,40,'',right) !:
          rc+=1 : if dedfed(j)=1 then resp$(rc)='True' else resp$(rc)='False'
01470     fnChk(j+lc,45,'',right) !:
          rc+=1 : if dedfica(j)=1 then resp$(rc)='True' else resp$(rc)='False'
01480     fnChk(j+lc,50,'',right) !:
          rc+=1 : if dedst(j)=1 then resp$(rc)='True' else resp$(rc)='False'
01490     fnChk(j+lc,55,'',right) !:
          rc+=1 : if deduc(j)=1 then resp$(rc)='True' else resp$(rc)='False'
01500 ! fnCOMBOF('[Q]\GLmstr',LC+J,62,0,'[Q]\CLmstr\GLmstr.h[cno]',1,12,13,30,'[Q]\CLmstr\GLIndex.h[cno]',LIMIT_TO_LIST) !:
          ! rESP$(RC+=1)=MISCGL$(J)
01502     fnqgl(lc+j,62,0,2) !:
          resp$(rc+=1)=fnrgl$(miscgl$(j))
01510   next j
01520   return 
01530 ! ______________________________________________________________________
01540 BUILD_COMPANY: ! 
01550   open #company=1: "Name=[Q]\CLmstr\Company.h[cno],Size=0,RecL=882,Replace",internal,outIn,relative 
01560   write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,1,0,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
01570   goto READ_COMPANY
01580 ! ______________________________________________________________________
01590 SAVE: ! 
01600   rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
01610   return 
01620 ! ______________________________________________________________________
01630 XIT: ! 
01640   close #company: 
01650   fnxit
01660 ! ______________________________________________________________________
01670 ! <Updateable Region: ERTN>
01680 ERTN: fnerror(program$,err,line,act$,"xit")
01690   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01710   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01720 ERTN_EXEC_ACT: execute act$ : goto ERTN
01730 ! /region
01740 ! ______________________________________________________________________
