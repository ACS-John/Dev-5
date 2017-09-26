00010 ! Replace S:\acsGL\AcGlIncB6
00020 ! -- INCOME STATEMENT WITH BUDGET (six columns)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fncno,fnglfs,fncch$,fnpedat$,fnactpd$,fnactpd,fnfscode,fnUseDeptNo,fnpriorcd,fntos,fnprocess,fnlbl,fntxt,fncmdkey,fnacs,fnps
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,p$(20)*50
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00100   dim pedat$*20,actpd$*6,bm(13),bp(13),by(13),cap$*128,udf$*256
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Six Column Income Statement with Budget")
00130   on fkey 5 goto L2350
00140   let fncno(cno,cnam$)
00150   let udf$=env$('temp')&'\'
00160   let actpd=fnactpd
00170   let actpd$=fnactpd$
00180   let pedat$=rtrm$(fnpedat$)
00190   let x=pos(pedat$," ",1)
00200   let curmonth$=pedat$(1:x)
00210   let curyear$=pedat$(len(rtrm$(pedat$))-4:len(rtrm$(pedat$)))
00220   let curyear=val(curyear$) conv L230
00230 L230: let prioryr=curyear-1
00240   if fnglfs=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00250 ! ______________________________________________________________________
00260   print newpage
00270   let pors=1
00280   let mp1=69
00290   if fnps=2 then let mp1=mp1+3
00300   let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00310   if fnps=2 then let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&str$(cno)&",Shr"
00320   form c 9,skip 0
00330   form c 7,skip 0
00340   let nametab=int(44-len(rtrm$(cnam$))/2)
00350   open #1: fl1$,internal,input,keyed 
00360   if fnprocess=1 or fnUseDeptNo=0 then goto L450
00370   let fntos(sn$="ACglincb") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00380   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00390   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 ) !:
        let resp$(1)=""
00400   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00410   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00420   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00430   let fnacs(sn$,0,mat resp$,ckey)
00440   if ckey=5 then goto XIT
00450 L450: let costcntr=val(resp$(1))
00460   let cnam$=rtrm$(cnam$)
00470   let pf1=len(cnam$)+int((43-len(cnam$))/2)
00480   let report$="STATEMENT OF INCOME AND EXPENSES"
00490   let fnopenprn(cp,58,220,process)
00500   let redir=0: if file$(255)(1:4)<>"PRN:" then let redir=1
00510   if fnps=2 then goto L540 ! secondary
00520   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 69 3 Replace DupKeys -N"
00530   goto L550
00540 L540: execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&" "&udf$&"fsindex.H"&str$(cno)&" 72 3 Replace DupKeys -N"
00550 L550: open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&udf$&"fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
00560 L560: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2350
00570   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
00580   if costcntr=0 then goto L610
00590   if fc=0 and te$="F" then goto L620 ! 5/08/1989
00600   if costcntr><fc then goto L560
00610 L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00620 L620: if te$="S" or te$="F" then goto L640
00630   if heading=0 and te$><"R" then gosub L2190
00640 L640: on pos ("RFHDTS",te$,1) goto L1640,L1680,L650,L700,L1430,L1640 none L560
00650 L650: print #255,using L660: d$(1:40)
00660 L660: form pos sp,c 40,skip 1
00670   gosub L1860
00680   gosub L1750
00690   goto L560
00700 L700: if notrans=1 then goto L1010
00710   if ir>=val(r$) and val(r$)><0 then goto L830
00720 L720: ! read amounts from gl master file
00730 L730: read #3,using L820: ir,bb,cb,mat by,mat bp,mat bm eof L1000
00740   if ir=0 then goto L730 ! skip accounts with no income reference #
00750   if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L820
00760   if fnfscode<1 or fnfscode>13 then let fnfscode=1
00770   if fnpriorcd=1 then let cb=by(fnfscode) else let cb=bp(fnfscode)
00780   if fnpriorcd=2 then goto L810
00790   if fnfscode>1 then let bb=by(fnfscode-1) else let bb=0
00800   goto L820
00810 L810: if fnfscode>1 then let bb=bp(fnfscode-1) else let bb=0
00820 L820: form pos mp1,pd 3,pos 81,41*pd 6.2
00830 L830: if ir=val(r$) then let total=total+(cb-bb) else goto L980
00840   let total2=total2+cb
00850   for z=1 to 13
00860     let annualb=annualb+bm(z)
00870   next z
00880   if fnfscode=0 then let pmonth=pmonth+bm(actpd) else let pmonth=pmonth+bm(fnfscode) ! 11/24/86
00890   if fnfscode=0 then goto L900 else goto L940 ! 11/24/86
00900 L900: for j=1 to actpd
00910     let lastyr=lastyr+bm(j)
00920   next j
00930   goto L720
00940 L940: for j=1 to fnfscode ! 11/24/86
00950     let lastyr=lastyr+bm(j) ! 11/24/86
00960   next j ! 11/24/86
00970   goto L720 ! 11/24/86
00980 L980: if ir<val(r$) then goto L720
00990   if ir>val(r$) then goto L1010
01000 L1000: let notrans=1
01010 L1010: let overundr=lastyr-total2
01020   let unexpend=annualb-total2
01030   for j=1 to 9
01040     if ac(j)=9 then goto L1120 ! 10/14/87
01050     let accum(j,1)=accum(j,1)+total
01060     let accum(j,2)=accum(j,2)+total2
01070     let accum(j,3)=accum(j,3)+annualb
01080     let accum(j,4)=accum(j,4)+pmonth
01090     let accum(j,5)=accum(j,5)+lastyr
01100     let accum(j,6)=accum(j,6)+overundr
01110     let accum(j,7)=accum(j,7)+unexpend
01120 L1120: next j
01130   if rs=1 then let total=-total else goto L1200
01140   let total2=-total2
01150   let annualb=-annualb
01160   let pmonth=-pmonth
01170   let lastyr=-lastyr
01180   let overundr=overundr
01190   let unexpend=unexpend
01200 L1200: if ds=1 then let dollar$="$" else let dollar$=" "
01210   goto L1250 ! print all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
01220   if annualb><0 or total2><0 then goto L1250
01230   if total<>0 then goto L1250
01240   if ls+ds+ul+ic>0 then goto L1250 else goto L560
01250 L1250: let sp2=26-sp-1
01260   if ul=1 then print #255,using L1570: d$(1:sp2),dollar$,"{\ul ",annualb,"}",ar$,"{\ul ",total,"}",ar$,"{\ul ",pmonth,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",lastyr,"}",dollar$,"{\ul ",unexpend,"}" pageoflow L2040 : goto L1300
01280   print #255,using L1290: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,pmonth,dollar$,total2,dollar$,lastyr,dollar$,unexpend pageoflow L2040
01290 L1290: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,skip 1
01300 L1300: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
01310   let total=0
01320   let total2=0
01330   let annualb=0
01340   let pmonth=0
01350   let lastyr=0
01360   let overundr=0
01370   let unexpend=0
01380   gosub L1750
01390   if ul=1 then goto L1410
01400   gosub L2050
01410 L1410: gosub L1860
01420   goto L560
01430 L1430: if ap=0 then let ap=1
01440   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01450   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01460   if rs=1 then let accum3=-accum(ap,3) else let accum3=accum(ap,3)
01470   if rs=1 then let accum4=-accum(ap,4) else let accum4=accum(ap,4)
01480   if rs=1 then let accum5=-accum(ap,5) else let accum5=accum(ap,5)
01490   if rs=1 then let accum6=accum(ap,6) else let accum6=accum(ap,6)
01500   if rs=1 then let accum7=accum(ap,7) else let accum7=accum(ap,7)
01510   if ds=1 then let dollar$="$" else let dollar$=" "
01520   let sp2=26-sp-1
01530   if ul=1 then print #255,using L1570: d$(1:sp2),dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum5,"}",dollar$,"{\ul ",accum7,"}" pageoflow L2040 : goto L1580
01540 ! Print #255,Using 1210: D$(1:SP2),DOLLAR$,ACCUM3,DOLLAR$,ACCUM1,DOLLAR$,ACCUM4,DOLLAR$,ACCUM2,DOLLAR$,ACCUM5,DOLLAR$,ACCUM6,DOLLAR$,ACCUM7 Pageoflow 1890
01550   print #255,using L1560: d$(1:sp2),dollar$,accum3,dollar$,accum1,dollar$,accum4,dollar$,accum2,dollar$,accum5,dollar$,accum7 pageoflow L2040
01560 L1560: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1
01570 L1570: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1
01580 L1580: let ft1=0
01590   gosub L1750
01600   if ul=1 then goto L1620
01610   gosub L2050
01620 L1620: gosub L1860
01630   goto L560
01640 L1640: if te$="R" then let report$=d$
01650   if te$="S" then let secondr$=d$
01660   gosub L1860
01670   goto L560
01680 L1680: if foot1=1 then goto L1730
01690   let tabnote=sp
01700   let foot1=1
01710   let foot$=d$
01720   goto L560
01730 L1730: let foot$=rtrm$(foot$)&d$
01740   goto L560
01750 L1750: for j=1 to 9
01760     if ac(j)=0 or ac(j)=9 then goto L1840 ! 10/14/87
01770     let accum(j,1)=0
01780     let accum(j,2)=0
01790     let accum(j,3)=0
01800     let accum(j,4)=0
01810     let accum(j,5)=0
01820     let accum(j,6)=0
01830     let accum(j,7)=0
01840 L1840: next j
01850   return 
01860 L1860: if ls=0 then goto L2020
01870   if ls=99 then goto L1910
01880   print #255,using L1890: " "
01890 L1890: form pos 1,c 1,skip ls
01900   goto L2020
01910 L1910: ! If FT1=1 Then Goto 1870
01920   let fnpglen(pglen)
01930 ! If PGLEN<>42 Then Let PGLEN=58
01940   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01950 ! If PGLEN=42 Then Let SK=SK+1
01960   print #255,using L1970: rtrm$(foot$),"Page "&str$(pt1)
01970 L1970: form skip sk,pos tabnote,c fl,pos 115,c 8,skip 1
01980 ! Let FT1=1
01990   if eofcode=1 then goto L2020
02000   print #255: newpage
02010   gosub L2190
02020 L2020: return 
02030 ! ______________________________________________________________________
02040 L2040: gosub L1910: continue 
02050 L2050: if ul=0 then goto L2150
02060   if ul=1 then goto L2100
02070   let underlin$="=============="
02080   goto L2110
02090   goto L2150
02100 L2100: let underlin$="______________"
02110 L2110: ! Print #255,Using 1980: UNDERLIN$,UNDERLIN$(1:12),UNDERLIN$(1:12),UNDERLIN$,UNDERLIN$,UNDERLIN$,UNDERLIN$
02120   print #255,using L2130: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$
02130 L2130: form pos 30,c 15,2*c 13,4*c 15,skip 0
02140   form skip redir,pos 26,c 15,2*c 13,4*c 15,skip redir
02150 L2150: if redir=0 then print #255,using L2160: " " pageoflow L2040
02160 L2160: form skip 1,c 1,skip 0
02170   return 
02180 ! ______________________________________________________________________
02190 L2190: let heading=1
02200   let pt1+=1
02210   print #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
02220   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
02230   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
02240   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
02250   print #255: "\ql "
02260   print #255: 
02270 ! Print #255: TAB(33);"ANNUAL";TAB(40);"<--";FNCCH$;"-->";TAB(66);" <--     YEAR TO DATE      -->";TAB(97);"<--     BUDGET TO DATE    -->"
02280   print #255,using L2290: "ANNUAL",curmonth$,curmonth$,"BAL YTD","BAL YTD","UNEXPENDED"
02290 L2290: form pos 27,cr 13,cr 13,cr 13,cr 15,cr 15,cr 15,skip 1
02300   print #255,using L2290: "BUDGET",str$(curyear),str$(prioryr),str$(curyear),str$(prioryr),str$(curyear)
02310 ! Print #255: TAB(33);"BUDGET";TAB(45);"BALANCE";TAB(60);"BUDGET";TAB(73);"BALANCE";TAB(90);"BUDGET";TAB(101);"OVER/UNDER";TAB(116);"UNEXPENDED"
02320   print #255: 
02330   return 
02340 ! ______________________________________________________________________
02350 L2350: let eofcode=1
02360   gosub L1910
02370   let fnfscode(actpd)
02380   let fnpriorcd(1)
02388   let fnfscode(actpd)
02389   let fnpriorcd(1)
02390   let fncloseprn
02400 ! 
02410 XIT: let fnxit
02420 ! ______________________________________________________________________
02430 ! <updateable region: ertn>
02440 ERTN: let fnerror(program$,err,line,act$,"xit")
02450   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02470   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02480 ERTN_EXEC_ACT: execute act$ : goto ERTN
02490 ! /region
