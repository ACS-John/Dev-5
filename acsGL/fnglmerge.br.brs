00010 ! Replace S:\acsGL\fnglMerge
00020 ! GL Merge program, chained to from other systems, like Checkbook-post to GL
00030   def library fnglmerge
00040     library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3,fncno,fnprocess,fnchain,fnprg,fnstyp,fnmsgbox,fngethandle,fnTos,fnAcs,fnCmdSet,fncmbcno,fncursys$,fnLbl,fnTxt,fnCmdKey,fncombof,fnindex_it,fnfree
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim adr(2),ta(2),prg$*20,k(10,8),gl$(5)*12,gl1(5)
00080     dim t$*12,n(2),l$*12,p$*30,ven$*8,zo(50),d$*50
00090     dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,fl1$(6),cap$*128
00100     dim ml$(10)*80 ! for fnMsgBox
00110     dim resp$(10)*80 ! for Screen Ace
00120 ! ______________________________________________________________________
00130     on=1 : off=0 : cancel=5 : delete=4 : selbyrow=1 !:
          limit_to_list=1 : add_all=2 : right=1 : disable=1 !:
          center=2 : pointtwo$='32' : mmddyy$='1'
00140     fntop(program$,cap$="GL Merge")
00150     if fncursys$='GL' then let fncno(cno) else gosub ASK_GLCNO
00160     fnprg(prg$)
00170     if fnstyp=99 then goto L200
00180     if fnstyp=9 then prg$="S:\acsTM\tmMenu" else prg$="S:\acsGL\acGLAuto"
00190     fnprg(prg$,2)
00200 L200: open #glmstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.H[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed ioerr GLMSTR_OPEN_ERR
00210     open #gltrans:=fngethandle: "Name=[Q]\GLmstr\GLTrans.H[cno],Shr",internal,outIn,relative 
00220     open #glwk1:=fngethandle: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat,NoShr",internal,outIn 
00230     open #gl1099:=fngethandle: "Name=[Q]\GLmstr\GL1099.H[cno],KFName=[Q]\GLmstr\gl109idx.H[cno],Shr",internal,outIn,keyed 
00240     open #gltr1099:=fngethandle: "Name=[Q]\GLmstr\GLTR1099.H[cno],Shr",internal,outIn,relative 
00250 ! fnwait
00260 READ_GLWK1: ! 
00270     read #glwk1,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8': t$,s,k,mat n,l$,p$,ven$ eof EO_GLWK1
00280     if n(2)=9 then goto READ_GLWK1 ! CHECK PREVIOUS POST
00290     if k=0 and uprc$(trim$(p$))<>"VOID" then goto READ_GLWK1
00300 HERE_A: ! 
00310     if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then !:
            goto READ_GLWK1
00320     if t$(3:3)=" " then t$(3:3)="0"
00330     if t$(12:12)=" " then t$(12:12)="0"
00340     read #glmstr,using 'Form POS 87,PD 6.2,POS 333,2*PD 3',key=t$: cb,mat ta nokey REJECT_GL
00350 READ_GLTRANS: ! 
00360 ! READ #gltrans,USING 460,REC=1: LR2
00370     lr2=lrec(gltrans)+1
00380     write #gltrans,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,PD 3',rec=lr2: t$,s,k,mat n,l$,p$,0 duprec READ_GLTRANS
00390     if ta(1)=0 then ta(1)=lr2
00400     if ta(2)>0 then !:
            rewrite #gltrans,using 'Form POS 71,PD 3',rec=ta(2): lr2
00410     ta(2)=lr2
00420     cb+=k
00430     rewrite #glmstr,using 'Form POS 87,PD 6.2,POS 333,2*PD 3',key=t$: cb,mat ta
00440 ! REWRITE #gltrans,USING 460,REC=1,RELEASE: LR2
00450     rewrite #glwk1,using 'Form POS 27,N 2': 9
00460 L460: if trim$(ven$)="" or trim$(ven$)="0" then goto READ_GLWK1
00470     read #gl1099,using 'Form POS 104,PD 5.2,POS 122,2*PD 3',key=ven$: ytdp ,mat adr nokey L1250
00480     ytdp+=k
00490 ! READ #GLTR1099,USING 260,REC=1,RESERVE: LR5
00500 L500: lr5=lrec(gltr1099)+1 !:
          write #gltr1099,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L500
00510     if adr(2)=0 then adr(1)=lr5 else !:
            rewrite #gltr1099,using 'Form POS 62,PD 3',rec=adr(2),reserve: lr5
00520     rewrite #gltr1099,using 'Form POS 62,PD 3',rec=1,release: lr5
00530     adr(2)=lr5
00540     rewrite #gl1099,using 'Form POS 104,PD 5.2,POS 122,2*PD 3',key=ven$: ytdp,mat adr
00550     goto READ_GLWK1
00560 ! ______________________________________________________________________
00570 REJECT_GL: ! 
00580     fnTos(sn$='GLMerge-Reject_GL') !:
          lc=0 : mylen=20 : mypos=mylen+2
00590     fnLbl(lc+=1,1,'GL Account Reject',80,center)
00600     fnLbl(lc+=1,1,'Account Number:',mylen,right)
00610     fnTxt(lc,mypos,12,0,right,'',disable) !:
          resp$(1)=t$
00620     fnLbl(lc+=1,1,'Date:',mylen,right)
00630     fnTxt(lc,mypos,0,0,right,mmddyy$,disable) !:
          resp$(2)=str$(s)
00640     fnLbl(lc+=1,1,'Amount:',mylen,right)
00650     fnTxt(lc,mypos,11,0,right,pointtwo$,disable) !:
          resp$(3)=str$(k)
00660     fnLbl(lc+=1,1,'Reference Number:',mylen,right)
00670     fnTxt(lc,mypos,8,0,right,'',disable) !:
          resp$(4)=l$
00680     fnLbl(lc+=1,1,'Description:',mylen,right)
00690     fnTxt(lc,mypos,30,0,right,'',disable) !:
          resp$(5)=p$
00700     fnLbl(lc+=1,1,"Account Number "&trim$(t$)&" is not in the GL Master File",80,center) !:
          fnLbl(lc+=1,1,'Do you wish to add this account now?',80,center)
00710     fnCmdKey('&Yes',1,1,0) !:
          fnCmdKey('&No',5,0,1)
00720     fnAcs(sn$,0,mat resp$,ckey) !:
          if ckey=1 then in1$='Yes' else if ckey=5 then in1$='No'
00730   if in1$="Yes" then goto ADD_GL else goto CORRECT_GL
00740 ! ______________________________________________________________________
00750 CORRECT_GL: ! 
00760   fnTos(sn$='GLMerge-Correct_GL') !:
        lc=0 : mylen=31 : mypos=mylen+2
00770   fnLbl(lc+=1,1,'GL Account Reject',80,center)
00780   fnLbl(lc+=1,1,'Correct General Ledger Account:',mylen,right)
00790   fncombof("gla-[cno]",lc,mypos,0,"[Q]\GLmstr\GLmstr.h[cno]",13,20,1,12,"[Q]\GLmstr\glIndx2.h[cno]",limit_to_list) !:
        resp$(1)=''
00800   fnCmdKey('&Okay',1,1,1)
00810   fnAcs(sn$,0,mat resp$,ckey)
00820   t$=resp$(1)(22:33)
00830   goto HERE_A
00840 ! ______________________________________________________________________
00850 ADD_GL: ! 
00860   fnTos(sn$='GLMerge-Add_GL') !:
        lc=0 : mylen=32 : mypos=mylen+2
00870   fnLbl(lc+=1,1,'GL Account Reject',80,center)
00880   fnLbl(lc+=1,1,'New General Ledger Account Name:',mylen,right)
00890   fnTxt(lc,mypos,30) !:
        resp$(1)=''
00900   fnCmdKey('&Okay',1,1,1)
00910   fnAcs(sn$,0,mat resp$,ckey)
00920   d$=resp$(1)
00930   mat ta=(0) : cb=0
00940   write #glmstr,using 'Form POS 1,C 12,C 50,6*PD 3,42*PD 6.2,2*PD 3': t$,d$,mat zo
00950   new1=1
00960   goto READ_GLTRANS
00970 ! ______________________________________________________________________
00980 EO_GLWK1: ! 
00990   if fnstyp><92 then goto DONE
01000   open #20: "Name=CNo.H"&wsid$,internal,outIn,relative  !:
        read #20,using "Form POS 239,5*C 12,5*N 10.2",rec=1: mat gl$,mat gl1 conv DONE !:
        close #20: 
01010   ckgl=0
01020   for j=1 to 5
01030     if val(gl$(j)(4:9))=0 then goto L1090 else gl2=0
01040     read #glmstr,using 'Form POS 87,PD 6.2',key=gl$(j): gl2 nokey L1050
01050 L1050: if gl1(j)=gl2 then goto L1090
01060     if ckgl=0 then pr newpage; bell
01070     pr using 'Form POS 1,C 11,C 14,C 15,N 12.2,X 4,C 12,N 12.2': "Account #:",gl$(j),"Client Balance:",gl1(j),"GL Balance:",gl2
01080     ckgl=1
01090 L1090: next j
01100   if ckgl=0 then goto DONE
01110   pr f "24,35,Cc 10,B,1": "Next  (F1)"
01120 L1120: input fields "24,2,C 1,AE,N": pause$
01130   if cmdkey=1 then goto DONE else goto L1120
01139 ! ______________________________________________________________________
01140 DONE: close #glmstr: 
01150   close #gltrans: 
01160   close #glwk1: 
01170   fnFree("[Q]\GLmstr\GLPT"&wsid$&".H[cno]")
01180 L1180: ! 
01190   close #gl1099: ioerr L1200
01200 L1200: if new1=1 or new2=1 then !:
          fnindex_it("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
01210   if new1=1 then !:
          execute "Index [Q]\GLmstr\GLmstr.H[cno]"&' '&"[Q]\GLmstr\GLIndex.H[cno] 1 12 Replace DupKeys -n"
01220   if new2=1 then !:
          execute "Index [Q]\GLmstr\GL1099.H[cno]"&' '&"[Q]\GLmstr\GL109IDX.H[cno] 1 8 Replace DupKeys -n"
01230   if fnprocess=1 then let fnchain("S:\acsGL\acGLAuto") else !:
          if fnprocess=2 then let fnchain("S:\acsGL\glMenu") else !:
            if fnprocess=4 then let fnchain("S:\acsGL\prMerge") else !:
              goto XIT
01240 ! ______________________________________________________________________
01250 L1250: cap$="GL Account Reject" !:
        fnwin3(win=101,cap$,15,70,1,0,5)
01260   pr #win,fields "4,2,C 50,N": "    Account Number: "&t$ !:
        pr #win,fields "5,2,C 50,N": "              Date: "&str$(s) !:
        pr #win,fields "6,2,C 50,N": "            Amount: "&str$(k) !:
        pr #win,fields "7,2,C 50,N": "  Reference Number: "&l$ !:
        pr #win,fields "8,2,C 50,N": "       Description: "&p$ !:
        pr #win,fields "9,2,C 50,N": "    Vendor Number: "&ven$
01270   pr #win,fields "11,2,Cc 60,N": "Vendor Number "&ven$&" is not on the 1099 Master File"
01280   pr #win,fields "12,13,C 36,N": "Do you want to add the Vendor (Y/N)?"
01290 L1290: input #win,fields "12,50,Cu 1,UAET,N": in2$ conv L1290
01300   close #win: 
01310   if in2$="Y" then goto L1420
01320   if in2$<>"N" then goto L1250 else goto ASK_CORRECT_VN
01330 ! ______________________________________________________________________
01340 ASK_CORRECT_VN: ! 
01350   cap$="GL Account Reject" !:
        fnwin3(win=101,cap$,5,33,1,0,5)
01360   pr #win,fields "4,2,C 22,N": "Correct Vendor Number:"
01370 L1370: input #win,fields "4,25,Cu 8,UT,N": ven$ conv L1370
01380   close #win: 
01390   ven$=lpad$(trim$(ven$),8)
01400   goto L460
01410 ! ______________________________________________________________________
01420 L1420: cap$="Vendor Information" !:
        fnwin3(win=101,cap$,10,56,1,0,5)
01430   pr #win,fields "4,2,Cr 18,N": "Vendor Name:" !:
        pr #win,fields "5,2,Cr 18,N": "Vendor Address:" !:
        pr #win,fields "6,2,Cr 18,N": "Vendor Address:" !:
        pr #win,fields "7,2,Cr 18,N": "City, State Zip:" !:
        pr #win,fields "8,2,Cr 18,N": "1099 Type:" !:
        pr #win,fields "9,2,Cr 18,N": "Federal ID Number:"
01440   fl1$(1)="4,21,C 35,UT,N" : fl1$(2)="5,21,C 20,UT,N" !:
        fl1$(3)="6,21,C 20,UT,N" : fl1$(4)="7,21,C 20,UT,N" !:
        fl1$(5)="8,21,N 02,UT,N" : fl1$(6)="9,21,C 11,UT,N"
01450   pr f "18,35,C 09,B,1": "Next (F1)"
01460 L1460: input #win,fields mat fl1$: nam$,ad1$,ad2$,csz$,typ,ss$ conv CONV1
01470   if ce>0 then fl1$(ce)(ce1:ce2)="U": ce=0
01480   if cmdkey>0 then goto L1550 else ce=curfld+1
01490   if ce>udim(fl1$) then ce=1
01500 L1500: fl1$(ce)=rtrm$(uprc$(fl1$(ce))) : ce1=pos(fl1$(ce),"U",1)
01510   ce2=ce1+1 : fl1$(ce)(ce1:ce1)="UC" : goto L1460
01520 CONV1: if ce>0 then fl1$(ce)(ce1:ce2)="U"
01530   ce=cnt+1
01540 ERR1: pr f "24,78,C 1": bell : goto L1500
01550 L1550: ! 
01560   mat adr=(0)
01570   if k=0 then goto L1630
01580   read #gltr1099,using 'Form POS 62,PD 3',rec=1,reserve: lr5
01590 L1590: lr5=lrec(gltr1099)+1
01600   write #gltr1099,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L1590
01610   rewrite #gltr1099,using 'Form POS 62,PD 3',rec=1,release: lr5
01620   mat adr=(lr5)
01630 L1630: write #gl1099,using 'Form POS 1,C 8,C 35,3*C 20,PD 5.2,N 2,C 11,2*PD 3': ven$,nam$,ad1$,ad2$,csz$,k,typ,ss$,mat adr
01640   new2=1
01650   goto READ_GLWK1
01660 ! ______________________________________________________________________
01670 GLMSTR_OPEN_ERR: ! 
01690   mat ml$(3) !:
        ml$(1)='Company Number [cno] does not exists!' !:
        ml$(2)='Please try again.' !:
        ml$(3)='Nothing Posted.' !:
        fnmsgbox(mat ml$,ok$,cap$,16)
01700   goto XIT
01710 ! ______________________________________________________________________
01720 ! <updateable region: ertn>
01730 ERTN: fnerror(program$,err,line,act$,"xit")
01740   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01760   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01770 ERTN_EXEC_ACT: execute act$ : goto ERTN
01780 ! /region
01790 ! ______________________________________________________________________
01800 ASK_GLCNO: ! 
01810   fnTos(sn$='GLMerge_ask_glcno') !:
        lc=0
01820   fnLbl(lc+=1,1,'Select the General Ledger Company to Post to')
01830   fncmbcno(lc+=1,5,'GL')
01840   fnCmdKey('&Okay',1,1,1)
01850   fnAcs(sn$,0,mat resp$,ckey)
01860   cno=val(resp$(1)(43:47))
01870 ! 
01880   return 
01890 ! ______________________________________________________________________
01900 XIT: fnend 
01910 ! ______________________________________________________________________
