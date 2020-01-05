00010 ! Replace S:\Core\Programs\Amortiz
00020 ! Amortization Program
00030   library 'S:\Core\Library': fnerror,fnxit,fnopenprn,fncloseprn,fnTos,fnLbl,fnTxt,fnAcs,fnCmdSet
00040   on error goto Ertn
00050 ! ______________________________________________________________________
00060   dim eX(50,2),io2$(6),io3$(2),io4$(2),wd4$(2),pfm$*200
00070   dim lor$*30,ln$*12,lee$*30,pfm2$*200,text$*60, mask$*25, dv$*40
00080   dim response$(20)*40
00090 ! ______________________________________________________________________
00100   y=12 : n=15 : ir=.05 : la=45000.
00110 ! ______________________________________________________________________
00120   wd4$(1)="1. Regular"
00130   wd4$(2)="2. Condensed"
00140   pfm$="FORM POS 1,N 6,x 3,pic(zz),""/"",pic(##),""/"",pic(####),"
00150   pfm2$="FORM POS 1,c 6,x 3,pic(zz/##/####),"
00160   for j=1 to 6
00170     pfm$=pfm$&"PIC(-----,---,---.##),"
00180     pfm2$=pfm2$&"PIC(-----,---,---.##),"
00190   next j
00200   pfm$=pfm$&"skip 1"
00210   pfm2$=pfm2$&"skip 1"
00220 ! ______________________________________________________________________
00230 MENU1: close #101: ioerr L230
00235 L230: fnTos("Amort1")
00240   lc=0: rc=0
00245   fnLbl(lc+=1,1,"Loan Amount:",18,1)
00250   fnTxt(lc,20,20,0,0,"10"): response$(rc+=1)=str$(la)
00255   fnLbl(lc+=1,1,"Interest Rate (ie: .15):",18,1)
00260   fnTxt(lc,20,20,0,0,"43") : response$(rc+=1)=str$(ir)
00265   fnLbl(lc+=1,1,"Years Loan is for:",18,1)
00270   fnTxt(lc,20,2,0,0,"30"): response$(rc+=1)=str$(n)
00275   fnLbl(lc+=1,1,"Payments Per Year:",18,1)
00280   fnTxt(lc,20,2,0,0,"30"): response$(rc+=1)=str$(y)
00285   fnLbl(lc+=2,1,"Note: An Interest Rate of 15% should be entered as 0.15",57,2)
00290 ! tEXT$="Payment:"
00295 ! fnPRF(PFX,6,1,22,1,TEXT$)
00300 ! tEXT$=STR$(P)
00305 ! fnPRF(PFX,6,30,20,0,TEXT$)
00310   fnCmdSet(2)
00315   fnAcs("Amort1",0,mat response$,ckey)
00320   rc=0
00325   s=la=val(response$(rc+=1))
00330   ir=val(response$(rc+=1))
00335   n=val(response$(rc+=1))
00340   y=val(response$(rc+=1))
00345 ! ______________________________________________________________________
00347 L330: ! Rinput Fields MAT IO1$: S,R,N,Y
00349   if ckey=99 or ckey=5 then goto XIT
00351   if ckey=2 then goto L1870
00353   if s=0 then goto XIT
00355   la=s
00357 ! ir=r
00359   p=la*(ir/y)/(1-(1/(1+ir/y)**(n*y))) ! that's the magic
00361   p=fna(p+.009)
00363   pr f "11,47,NZ 8.2,N": p
00365   if ckey=4 then goto L440 else goto L330
00440 L440: close #101: ioerr L450
00450 L450: open #101: "SRow=03,SCol=14,ERow=12,ECol=65,Border=SR,Caption=<Print Amortization Schedule",display,outIn 
00460   pr #101: newpage
00470   i2=p2=0
00480   on fkey 5 goto L1830
00490   pr #101,fields "01,15,C 20,N": "            Lendor: "
00500   pr #101,fields "02,15,C 20,N": "          Borrower: "
00510   pr #101,fields "03,15,C 20,N": "       Loan Number: "
00520   pr #101,fields "04,15,C 20,N": "         Loan Date: "
00530   pr #101,fields "05,15,C 20,N": " First Payment Due: "
00540   pr #101,fields "06,15,C 20,N": "   Monthly Payment: "
00550   pr #101,fields "07,20,C 39,H,N": " NOTE: Enter dates in MMDDYYYY format."
00560   pr #101,fields "08,30,Cc 09,B,1": "Next (F1)"
00570   pr #101,fields "09,41,Cc 09,B,5": "Stop (F5)"
00590 L590: ! 
00600   rinput #101,fields mat io2$: lor$,lee$,ln$,ld,fpd,p conv L590
00610   if cmdkey=5 then goto MENU1
00620   d=fpd
00630   d$=str$(d)
00640   d$=lpad$(rtrm$(d$),8)
00650   d1=val(d$(1:2))
00660   d2=val(d$(3:4))
00670   d3=val(d$(5:8))
00680   d5=d3*10000+d2+d1*100
00685 L680: open #101: "SRow=8,SCol=21,ERow=14,ECol=58,Border=SR,Caption=<Amortization Payments",display,outIn 
00690   pr #101: newpage
00695   pr f "09,22,C 36,N": "Enter any extra payments made toward"
00700   pr f "10,22,C 36,N": "  principal (blank when completed)"
00705   pr f "12,26,C 17,N": "Date (MMDDYYYY): "
00710   pr f "13,26,C 17,N": " Payment Amount: "
00715   pr f "15,30,Cc 09,B,1": "Next (F1)"
00720   pr f "15,41,Cc 09,B,5": "Stop (F5)"
00725   input fields mat io3$: eX(j6+1,1),eX(j6+1,2)
00730   if cmdkey=5 then goto MENU1
00735   eX(j6+1,1)=val(str$(eX(j6+1,1))(5:8)&str$(eX(j6+1,1))(1:4))
00740   if eX(j6+1,1)=0 then goto L820
00745   j6=j6+1
00750   goto L680
00820 L820: close #101: ioerr L830
00830 L830: open #101: "SROW=11,SCOL=30,EROW=14,ECOL=50,BORDER=SR,CAPTION=<Select pr Type",display,outIn 
00840   pr #101: newpage
00850   pr f mat io4$: mat wd4$
00860   pr f "15,34,Cc 11,B,5": "Cancel (F5)"
00870   input select mat io4$,attr "R": mat wd4$
00880   typ=curfld
00890   if cmdkey=5 then goto MENU1
00900   close #101: ioerr L910
00910 L910: open #101: "SROW=9,SCOL=9,EROW=13,ECOL=71,BORDER=SR,CAPTION=<Print Amortization Schedule",display,outIn 
00920   pr #101: newpage
00930   pr f "10,27,C 26,H,N": " Printing: Please wait..."
00940   pr f "12,30,C 15,N": "Printing Page: "
00950   pr f "14,34,Cc 11,B,5": "Cancel (F5)"
00960   pge=0
00970   fnopenprn
00980   gosub PR_HDR
00990   goto L1200
01000 ! ______________________________________________________________________
01010 NPG: pr #255: newpage
01020   gosub PR_HDR
01030   continue 
01040 ! ______________________________________________________________________
01050 PR_HDR: pge=pge+1
01060   pr f "12,45,CL 5,N": str$(pge)
01070   pr #255,using L1080: "* Amortization Schedule *",pge
01080 L1080: form pos 38,c 70,"Page",n 4
01090   pr #255: ""
01100   pr #255: "Lendor: ";lor$;"   Borrower: ";lee$;"   Loan No.: ";ln$
01110   pr #255,using L1120: "Loan Date: ",ld,"Amount of Loan: ",la,"Interest Rate: ",str$(r*100)&"%","Monthly Payment: ",p
01120 L1120: form pos 1,c 12,pic(zz/zz/zzzz),x 3,c 17,pic(----,---,---.##),x 3,c 16,c 11,c 17,pic(----,---.##)
01130   pr #255: ""
01140   pr #255: "Payment                   Beginning        Interest       Principal          Ending        Interest       Principal"
01150   pr #255,using L1160: " Number   Due Date          Balance         Payment         Payment        Balance         To  Date       To   Date"
01160 L1160: form pos 1,c 119
01170   pr #255: "_______   ________        _________        ________       _________        ________        ________       _________"
01180   return 
01190 ! ______________________________________________________________________
01200 L1200: for j=1 to n*y
01210     b1=s
01220     s9=0
01230     i1=0
01240     eq=0
01250     if j6=0 then goto L1440
01260     for j1=1 to j6+1
01270       if eX(j1,1)=0 then goto L1440
01280       if eX(j1,1)>d4 and eX(j1,1)<d5 then goto L1320
01290       if eX(j1,1)=d5 then eq=j1
01300       if eX(j1,1)=d5 then goto L1440
01310     next j1
01320 L1320: d9=(eX(j1,1)-int(eX(j1,1)*.0001)*10000)*10000+int(eX(j1,1)*.0001)
01330     d8=val(lpad$(str$(d9),8)(3:4))
01340     i1=fna(s*r/365*max(d2-d8,d8-d2))
01350     i2=i2+i1
01360     p2=p2+eX(j1,2)-i1
01370     i3=i3+i1
01380     p3=p3+eX(j1,2)-i1
01390     s=s+i1
01400     s=s-eX(j1,2)
01410     s9=eX(j1,2)
01420     pr #255,using pfm2$: "Extra",d9,b1,i1,eX(j1,2)-i1,s,i2,p2 pageoflow NPG
01430     b1=s
01440 L1440: i=fna(s*r/y-i1)
01450     d4=d5
01460     s=s+i
01470     if s>p then goto L1500
01480     x9=9
01490     p=s
01500 L1500: s=s-p
01510     i2=i2+i
01520     p2=p2+p-i
01530     i3=i3+i
01540     p3=p3+p-i
01550     pr #255,using pfm$: j,d1,d2,d3,b1,i,p-i,s,i2,p2 pageoflow NPG
01560     if eq=0 then goto L1640
01570     b1=s
01580     d9=(eX(eq,1)-int(eX(eq,1)*.0001)*10000)*10000+int(eX(eq,1)*.0001)
01590     s=b1-eX(eq,2)
01600     p2=p2+eX(eq,2)
01610     p3=p3+eX(eq,2)
01620     pr #255,using pfm2$: "Extra",d9,b1,0,eX(eq,2),s,i2,p2 pageoflow NPG
01630     b1=s
01640 L1640: if y=1 then goto L1720
01650     d1=fna(d1+12/y)
01660     if d1<13 then goto L1730
01670     pr #255,using L1680: "Year End:",d3,"  Totals: ",i3,p3 pageoflow NPG
01680 L1680: form pos 10,c 10,pic(zzzz),c 12,pos 37,pic(----,---,---.##),pic(-----,---,---.##),skip 2
01690     d1=1
01700     i3=0
01710     p3=0
01720 L1720: d3=d3+1
01730 L1730: if x9=9 then goto L1760
01740     d5=fna(d3*10000+d2+d1*100)
01750   next j
01760 L1760: pr #255,using L1680: "Year End: ",d3,"  Totals:",i3,p3 pageoflow NPG
01770   pr #255,using L1790: "Last Payment Amount: ",p
01780   pr #255,using L1790: "Total Payments: ",i2+p2
01790 L1790: form skip 2,c 24,pic(---,---,---.##)
01800   pr #255,using L1790: "Total Interest Paid: ",i2
01810   pr #255,using L1790: "Total Principal Paid: ",p2
01820   pr #255,using L1790: "Balance Remaining: ",s
01830 L1830: fncloseprn
01840   on fkey 5 ignore 
01850   x9=0
01860   goto MENU1
01870 L1870: lor$=" "
01880   lee$=" "
01890   ln$=" "
01900   ld=fpd=p=s=r=n=y=i2=p2=i3=p3=0
01910   goto MENU1
01920 ! ______________________________________________________________________
01930 XIT: end  ! fnxit("")
01940 ! ______________________________________________________________________
01950 ! <Updateable Region: ERTN>
01960 ERTN: fnerror(program$,err,line,act$,"xit")
01970   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02000 ERTN_EXEC_ACT: execute act$ : goto ERTN
02010 ! /region
02020 ! ______________________________________________________________________
02030   def fna(r)
02040     fna=int(r*100+.5)/100
02050   fnend 
