00010 ! Replace S:\acsPR\prRegFM
00020 ! Tax Deposit File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin, fnoldmsgbox,fncno,fnerror,fntop,fnxit,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim io1$(2),pt(26),rpnames$(10)*6,a$(28)*20,sc$(28)*20,in1$(28)
00080   dim cap$*128,msgline$(2)*60,response$(5)*1
00090 ! ______________________________________________________________________
00100   fntop('S:\acsPR\prRegFM',cap$="Tax Deposit")
00110   fncno(cno)
00115   fnconsole(1)
00120   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input, relative: read #1,using 'Form POS 648,10*C 6',rec=1: mat rpnames$ : close #1: 
00130   open #1: "Name="&env$('Q')&"\PRmstr\prTot.H"&str$(cno)&",Use,RecL=138,KFName="&env$('Q')&"\PRmstr\prTotIDX.H"&str$(cno)&",kps=1,kln=9",internal, outin,keyed 
00140 ! ______________________________________________________________________
00150   gosub BUILDSCREEN
00160 ! ______________________________________________________________________
00170 SCR1: ! 
00180   pr newpage
00190   fnopenwin(win=101,10,24,15,55,cap$)
00200   pr #win,fields "4,2,Cr 23,N": "Payroll Date to change:"
00210   pr #win,fields "5,2,Cr 23,N": "Department to change:"
00220   io1$(1)="4,26,Nz 6,UT,N"
00230   io1$(2)="5,26,Nz 3,UT,N"
00240   pr f "16,30,C 09,B,1": "Next (F1)"
00250   pr f "16,41,C 09,B,5": "Done (F5)"
00260 L260: input #win,fields mat io1$: idat,idep conv CONV2
00270   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00280   if cmdkey>0 then goto L350 else ce=curfld
00290 L290: ce=ce+1: if ce>udim(io1$) then ce=1
00300 L300: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L290
00310   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L260
00320 CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
00330   ce=cnt+1
00340 ERR2: pr f "24,78,C 1": bell : goto L300
00350 L350: if cmdkey=5 then goto L790
00360   if idat=0 then goto L790
00370   read #1,using L380, key=lpad$(str$(idat),6)&lpad$(str$(idep),3): dat,dep,mat pt nokey ADDOPTION
00380 L380: form pos 1,n 6,n 3,25*pd 5.2,n 4
00390 L390: pr newpage
00400   win=101
00410   fnopenwin(win,2,2,23,75,cap$)
00420   pr #win: newpage
00430   pr #win,fields mat sc$: mat a$
00440   pr f "24,22,c 11,B,1": "Save   (F1)"
00450   pr f "24,34,c 11,B,4": "Delete (F4)"
00460   pr f "24,46,c 11,B,5": "Cancel (F5)"
00470   dat=idat
00480   dep=idep
00490 L490: rinput #win,fields mat in1$: dat,dep,mat pt conv CONV1
00500   if ce>0 then in1$(ce)(ce1:ce2)="U": ce=0
00510   if cmdkey>0 then goto L580 else ce=curfld+1
00520   if ce>udim(in1$) then ce=1
00530 L530: in1$(ce)=rtrm$(uprc$(in1$(ce))) : ce1=pos(in1$(ce),"U",1)
00540   ce2=ce1+1 : in1$(ce)(ce1:ce1)="UC" : goto L490
00550 CONV1: if ce>0 then in1$(ce)(ce1:ce2)="U"
00560   ce=cnt+1
00570 ERR1: pr f "24,78,C 1": bell : goto L530
00580 L580: for ce=3 to udim(pt$)+2: pt(ce-2)=val(pt$(ce-2)) conv ERR1: next ce
00590   ce=0
00600   if cmdkey=5 then goto SCR1
00610   if add$="Y" and dat=0 then goto SCR1
00620   if cmdkey=4 or dat=0 then goto L660
00630   if dat<10100 or dat>123199 then ce=1: goto ERR1
00640   if dep<1 or dep>999 then ce=2: goto ERR1
00650   goto L680
00660 L660: delete #1,key=lpad$(str$(idat),6)&lpad$(str$(idep),3): nokey L670
00670 L670: goto SCR1
00680 L680: if idat=dat and idep=dep then goto L770
00690   read #1,using L700,key=lpad$(str$(dat),6)&lpad$(str$(dep),3): dat,dep nokey L720
00700 L700: form pos 1,n 6,n 3
00710   goto SCR1
00720 L720: delete #1,key=lpad$(str$(idat),6)&lpad$(str$(idep),3): 
00730 L730: write #1,using L380: dat,dep,mat pt
00740   add$=""
00750   indxcode=1
00760   goto SCR1
00770 L770: if add$="Y" then goto L730 else rewrite #1,using L380,key=lpad$(str$(idat),6)&lpad$(str$(idep),3): dat,dep,mat pt
00780   goto SCR1
00790 L790: close #1: 
00800   if indxcode=1 then pr 'supposta recreate index here - seems to be a loss of logic, better rewrite it.' : pr 'press enter to continue anyway' : input fields "1,1,C 1,N": pause$
00810   goto XIT
00820 ! ______________________________________________________________________
00830 BUILDSCREEN: ! 
00840   data "P/R Register Date:"
00850   data "Department Number:"
00860   data "Federal Withholding:"
00870   data "Soc-Sec Withholding:"
00880   data "State Withholding:"
00890   data "","","","","","","","","",""
00900   data "EIC:"
00910   data "Medicare W/H:"
00920   data "Regular Earnings:"
00930   data "Overtime Earnings:"
00940   data "Other Compensation:"
00950   data "Meals:"
00960   data "Tips:"
00970   data "Gross:"
00980   data "Net:"
00990   data "Employer's Match:"
01000   data "State U/C:"
01010   data "Federal U/C:"
01020   data "Num of Emp Records:"
01030   read mat a$
01040   for j=1 to 28
01050     if j>10 then goto L1070
01060     a$(j+5)=rpnames$(j)&":"
01070 L1070: if j>21 then goto L1100
01080     sc$(j)=str$(j)&",3,Cr 20,N"
01090     goto L1110
01100 L1100: sc$(j)=str$(j-6)&",40,Cr 20,N"
01110 L1110: if j=1 then goto L1120 else goto L1140
01120 L1120: in1$(j)=str$(j)&",24,G 6,UT,N"
01130     goto L1240
01140 L1140: if j=2 then goto L1150 else goto L1170
01150 L1150: in1$(j)=str$(j)&",24,G 3,UT,N"
01160     goto L1240
01170 L1170: if j>2 and j<22 then goto L1180 else goto L1200
01180 L1180: in1$(j)=str$(j)&",24,G 10.2,UT,N"
01190     goto L1240
01200 L1200: if j=28 then goto L1210 else goto L1230
01210 L1210: in1$(j)="22,61,G 4,UT,N"
01220     goto L1240
01230 L1230: in1$(j)=str$(j-6)&",61,G 10.2,UT,N"
01240 L1240: next j
01250   return 
01260 ! ______________________________________________________________________
01270 ADDOPTION: ! 
01280   msgline$(1)="A record for this Date and Department Number does not exist."
01290   msgline$(2)="Do you wish to add this record? (Y/N)"
01300   fnoldmsgbox(mat response$,cap$,mat msgline$,2)
01310   add$=response$(1)
01320   if add$="Y" then goto L390
01330   if add$="N" then goto SCR1
01340 ! ______________________________________________________________________
01350 ! <Updateable Region: ERTN>
01360 ERTN: fnerror(program$,err,line,act$,"xit")
01370   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01380   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01390   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01400 ERTN_EXEC_ACT: execute act$ : goto ERTN
01410 ! /region
01420 ! ______________________________________________________________________
01430 XIT: fnxit
01440 ! ______________________________________________________________________
