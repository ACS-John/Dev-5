00010 ! Replace R:\acsGL\ACGLMrge
00020 ! GL Merge program, chained to from other systems, !:
        ! like Checkbook-post to GL; also used to merge entries entered directly
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnerror,fnwin3,fncno,fnprocess,fnchain,fnprg,fnxit,fntop,fnstyp,fnmsgbox,fnaddglpayee,fntos,fnlbl,fncmdkey,fnacs,fnagl$,fntxt,fncmdset,fnopt,fnqglbig,fnrglbig$
00050   let fntop(program$,"General Ledger Merge")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim adr(2),ta(2),prg$*256,k(10,8),gl$(5)*12,gl1(5),tr$(5)*35
00090   dim t$*12,n(2),l$*12,p$*30,ven$*8,zo(50),d$*50,ml$(3)*80,bank$*25
00100   dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,fl1$(6),cap$*128,resp$(10)*80
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="GL Merge")
00130   let fncno(cno)
00140   let fnprg(prg$)
00150   if fnstyp=99 then goto L210
00160   if fnstyp=9 then let prg$="R:\acsTM\tmMenu" else let prg$="R:\acsGL\acGLAuto"
00170   let fnprg(prg$,2)
00180   open #company=1: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input 
00190   read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub !:
        ! read fund and sub codes from general
00200   close #company: 
00210 L210: ! 
00220   open #1: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00230   open #2: "Name=Q:\GLmstr\GLTrans.h"&str$(cno)&",Shr",internal,outin,relative 
00240   open #3: "Name=Q:\GLmstr\GLWk1"&wsid$&".H"&str$(cno)&",NoShr",internal,outin 
00250   open #paymstr=4: "Name=Q:\GLmstr\PayMstr.h"&str$(cno)&",Version=1,KFName=Q:\GLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00260   if exists("Q:\GLmstr\bankrec.H"&str$(cno))=0 then goto L270 else goto L300
00270 L270: open #6: "Name=Q:\GLmstr\bankrec.H"&str$(cno)&",KFName=Q:\GLmstr\bankrec-idx.H"&str$(cno)&",Version=1,RecL=91,use,kps=79/3/4,kln=12/1/8,Shr",internal,outin,keyed 
00280   close #6: 
00290   execute "Index Q:\GLmstr\bankrec.H"&str$(cno)&" Q:\GLmstr\bankrec-idx.h"&str$(cno) &" 79/3/4 12/1/8 Replace,DupKeys"
00300 L300: open #6: "Name=Q:\GLmstr\BankRec.h"&str$(cno)&",KFName=Q:\GLmstr\BankRec-idx.h"&str$(cno)&",Shr",internal,outin,keyed 
00310   if exists("Q:\GLmstr\gltr1099.h"&str$(cno))=0 then goto L320 else goto L340
00320 L320: open #trans=5: "Name=Q:\GLmstr\GLTR1099.h"&str$(cno)&",KFName=Q:\GLmstr\gltrIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00330   goto L350
00340 L340: open #trans=5: "Name=Q:\GLmstr\GLTR1099.h"&str$(cno)&",KFName=Q:\GLmstr\gltrIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00350 L350: form pos 62,pd 3
00360 L360: read #3,using "Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8,POS 93,C 12": t$,s,k,mat n,l$,p$,ven$,key$ eof L1450
00365   let prtrans=0
00370   if n(1)=4 then let n(1)=1 : let prtrans=1 ! convert payroll transaction types to a regular disbursment
00380   form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,pos 93,c 12
00390   if n(2)=9 then goto L360 ! CHECK PREVIOUS POST
00400   if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L360
00410   if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L360
00420   if t$(1:3)="   " then let t$(3:3)="0"
00430   if t$(10:12)="   " then let t$(12:12)="0"
00440 L440: read #1,using L450,key=t$: cb,mat ta nokey L840
00450 L450: form pos 87,pd 6.2,pos 333,2*pd 3
00460 L460: ! READ #2,USING 460,REC=1: LR2
00470   let lr2=lrec(2)+1
00480   write #2,using L540,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L460
00490   if ta(1)=0 then let ta(1)=lr2
00500   if ta(2)>0 then rewrite #2,using L550,rec=ta(2): lr2
00510   let ta(2)=lr2
00520   let cb=cb+k
00530   rewrite #1,using L450,key=t$: cb,mat ta
00540 L540: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00550 L550: form pos 71,pd 3
00560   rewrite #3,using L570: 9
00570 L570: form pos 27,n 2
00580 BANK_REC_FILE: ! 
00590   if l$="999999999999" then goto VENDOR_FILE ! don't update bkrec for contra entries
00600   if n(1)>2 then goto VENDOR_FILE ! only allow receipts or disbursments to bank rec
00610   let l$=trim$(l$): let l$=l$(1:8)
00620   let l$=lpad$(rtrm$(l$),8)
00630   let bank$=key$&str$(n(1))&l$
00640   read #6,using L650,key=bank$: amt nokey WRITE_NEW_BANKREC ioerr VENDOR_FILE
00650 L650: form pos 18,pd 10.2
00660   let amt=amt+k
00670   rewrite #6,using L650: amt
00680   goto VENDOR_FILE
00690 WRITE_NEW_BANKREC: ! 
00700   let bankgl$=key$ !:
        let tcde=n(1) ! transaction code !:
        let tr$(1)=lpad$(rtrm$(l$),8) ! ref # !:
        let tr$(2)=str$(s) !  check date !:
        let tx3= k ! amount!:
        let tr$(4)=ven$ ! payee
00710   let tr$(5)=p$ ! name or desc !:
        let pcde=0 ! posting code !:
        let clr=0 ! cleared date !:
        let scd=0 ! source code
00720   if tcde=2 then let tx3=-tx3 ! turn sign around on bank rec file for receipts
00730   write #6,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd
00740   form pos 1,c 12,c 12,c 30,c 2,n 6,pd 5.2,n 1
00750 VENDOR_FILE: ! 
00760   if rtrm$(ven$)="" or ltrm$(rtrm$(ven$))="0" then goto L360
00770   if n(1)<>1 or prtrans=1 then goto L360 ! only disbursments and not payroll trans
00780   let ven$=lpad$(rtrm$(ven$),8)
00790 L790: let lr5=lrec(5)+1
00800   write #5,using L810,rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L790
00810 L810: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
00820   goto L360
00830 ! ______________________________________________________________________
00840 L840: ! 
00850   let fntos(sn$="GLmerge") !:
        let mylen=40: let mypos=mylen+3 : let right=1
00860   let fnlbl(1,10,"  Account Number: "&t$,mylen,left)
00870   let fnlbl(2,10,"            Date: "&str$(s),mylen,left)
00880   let fnlbl(3,10, "          Amount: "&str$(k),mylen,left)
00890   let fnlbl(4,10, "Reference Number: "&l$ ,mylen,left)
00900   let fnlbl(5,10, "     Description: "&p$ ,mylen,left)
00910   let fnlbl(7,5, "This general ledger account does not exist!" ,60,0)
00920   let fnopt(8,10,"Add this Account",0,0) !:
        let resp$(1)="True"
00930   let fnopt(9,10,"Change Account Number",0,0) !:
        let resp$(1)="False"
00940   let fncmdkey("&Next",1,1,0,"Allows you to either add the account or change the account #.")
00950   let fnacs(sn$,0,mat resp$,ckey)
00960   if resp$(1)="True" then goto ADD
00970   if resp$(2)="True" then goto CHANGE_ACCOUNTS
00980   goto L840
00990 ! ______________________________________________________________________
01000 ADD: ! 
01010   let dno=val(t$(1:3)) conv L1020
01020 L1020: let ano=val(t$(4:9)) conv L1030
01030 L1030: let sno=val(t$(10:12)) conv L1040
01040 L1040: let fntos(sn$="GLmerge3") !:
        let mylen=23: let mypos=mylen+3 : let right=1: let rc=0
01050   if use_dept =1 then let fnlbl(1,26,"Fund #",6,2)
01060   if use_sub =1 then let fnlbl(1,40,"Sub #",6,2)
01070   let fnlbl(2,1,"General Ledger Number:",mylen,right)
01080   if use_dept=1 then let fntxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) !:
          let resp$(rc+=1)=str$(dno)
01090   let fntxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) !:
        let resp$(rc+=1)=str$(ano)
01100   if use_sub=1 then let fntxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) !:
          let resp$(rc+=1)=str$(sno)
01110   let fnlbl(3,1,"Description:",mylen,right)
01120   let fntxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 ) !:
        let resp$(rc+=1)=""
01130 ! 
01140   let fncmdset(2)
01150   let fnacs(sn$,0,mat resp$,ckey)
01160   let pas=0
01170   let dno=ano=sno=0
01180   if use_dept=1 then let dno=val(resp$(1)) : let ano=val(resp$(2))
01190   if use_dept=0 then let ano=val(resp$(1))
01200   if use_dept=1 and use_sub=1 then let sno=val(resp$(3))
01210   if use_dept=0 and use_sub=1 then let sno=val(resp$(2))
01220 ! 
01230   if use_dept=1 and use_sub=1 then let d$=resp$(4)
01240   if use_dept=0 and use_sub=1 then let d$=resp$(3)
01250   if use_dept=0 and use_sub=0 then let d$=resp$(2)
01260   if use_dept=1 and use_sub=0 then let d$=resp$(3)
01270   let key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
01280   read #1,using 'Form POS 1,N 3',key=key$: dno nokey L1300
01290 ! msgbox
01300 L1300: mat ta=(0)
01310   let cb=0
01320   write #1,using L1330: t$,d$,mat zo
01330 L1330: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
01340   goto L460
01350 ! ______________________________________________________________________
01360 CHANGE_ACCOUNTS: ! 
01370   let fntos(sn$="GLmerge4") !:
        let mylen=23: let mypos=mylen+3 : let right=1
01380   let fnlbl(1,1,"General Ledger Number:",mylen,right)
01390   let fnqglbig(1,mypos,0,2) !:
        let resp$(1)=fnrglbig$(gl$)
01400   let fncmdkey("&Next",1,1,0,"Will change to the selected account.")
01410   let fnacs(sn$,0,mat resp$,ckey)
01420   if ckey=5 then goto L440
01430   let gl$=fnagl$(resp$(1)) : let t$=gl$ : goto L440
01440 ! ______________________________________________________________________
01450 L1450: if fnstyp><92 then goto L1620
01460   open #20: "Name=CNo.H"&wsid$,internal,outin,relative  !:
        read #20,using "Form POS 239,5*C 12,5*N 10.2",rec=1: mat gl$,mat gl1 conv L1620 !:
        close #20: 
01470   let ckgl=0
01480   for j=1 to 5
01490     if val(gl$(j)(4:9))=0 then goto L1570 else let gl2=0
01500     read #1,using L1510,key=gl$(j): gl2 nokey L1520
01510 L1510: form pos 87,pd 6.2
01520 L1520: if gl1(j)=gl2 then goto L1570
01530     if ckgl=0 then print newpage; bell
01540     print using L1550: "Account #:",gl$(j),"Client Balance:",gl1(j),"GL Balance:",gl2
01550 L1550: form pos 1,c 11,c 14,c 15,n 12.2,x 4,c 12,n 12.2,skip 1
01560     let ckgl=1
01570 L1570: next j
01580   if ckgl=0 then goto L1620
01590   print fields "24,35,Cc 10,B,1": "Next  (F1)"
01600 L1600: input fields "24,2,C 1,AE,N": pause$
01610   if cmdkey><1 then goto L1600
01620 L1620: close #1: 
01630   close #2: 
01640   close #3: 
01650   execute "Free Q:\GLmstr\GLPT"&wsid$&".H"&str$(cno) ioerr L1660
01660 L1660: ! 
01670   close #4: ioerr L1680
01680 L1680: if new1=1 or new2=1 then !:
          execute "Index Q:\GLmstr\GLBREC.h"&str$(cno)&" Q:\GLmstr\GLRecIdx.h"&str$(cno)&" 1 24 Replace DupKeys -n"
01690   if new1=1 then !:
          execute "Index Q:\GLmstr\GLmstr.h"&str$(cno)&" Q:\GLmstr\GLIndex.h"&str$(cno)&" 1 12 Replace DupKeys -n"
01700   if new2=1 then !:
          execute "Index Q:\GLmstr\GL1099.h"&str$(cno)&" Q:\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 Replace DupKeys -n"
01710   open #30: "Name=Q:\GLmstr\Process.h"&str$(cno)&",Shr",internal,outin,relative ioerr L1760
01720   read #30,using "form pos 1,n 1",rec=1: process norec L1760 ! read post payroll code
01730   rewrite #30,using "form pos 1,n 1",rec=1: 0 norec L1760 ! clear post payroll code
01740   close #30: 
01750   if process=1 or process=4 then let fnchain("R:\acsGL\prMerge")
01760 L1760: if fnprocess=1 then goto XIT else !:
          if fnprocess=2 then goto XIT else !:
            goto XIT
01770 ! ______________________________________________________________________
01780   mat ml$(3) !:
        let ml$(1)="Vendor # "&ven$&" (Ck # "&l$&") is not" !:
        let ml$(2)="in the payee file.  Click OK to enter the payee" !:
        let ml$(3)="information, else Cancel to skip updating this payee's record." !:
        let fnmsgbox(mat ml$,resp$,cap$,49)
01790   if uprc$(resp$)="OK" then let fnaddglpayee
01800   let new2=1
01810   goto L360
01820 ! ______________________________________________________________________
01830 XIT: let fnxit
01840 ! ______________________________________________________________________
01850   print newpage
01860   print fields "10,10,cc 60,n": "Company #"&str$(cno)&" does not exist!"
01870   print fields "12,10,cc 60,n": "Press Enter to Continue"
01880   input fields "12,60,c 1,e,n": pause$
01890   goto XIT
01900 ! ______________________________________________________________________
01910 ! <updateable region: ertn>
01920 ERTN: let fnerror(cap$,err,line,act$,"xit")
01930   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01940   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01950   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01960 ERTN_EXEC_ACT: execute act$ : goto ERTN
01970 ! /region
