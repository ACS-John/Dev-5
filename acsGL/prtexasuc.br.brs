00010 ! Replace S:\acsGL\PRTexasUC
00020 ! Quarterly UC Report (From the after-the-fact payroll files in gl) for Texas
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess,fntos,fnlbl,fntxt,fnacs,fncmdset,fngethandle,fnreg_read,fnreg_write
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2),cap$*128
00080   dim fa$(3),sa$(3)*40,cnam$*40
00090   dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11,pedat$*5
00100   dim resp$(3)*255,csvpath$*255
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Print Texas Unemployment Report")
00130   fncno(cno,cnam$)
00150   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,POS 188,PD 7.2,POS 658,10*N 1': mat a$,mat b$,c$,ucm,mat deduc !:
        close #1: 
00160   if fnprocess=1 then goto L280
00170 ! 
00175 L170: let fntos(sn$="PrTexasUc1") !:
        let mylen=60: let mypos=mylen+3 : let right=1
00180   fnlbl(1,1,"Quarter Ending Date(mm-yy):",mylen,right)
00185   fntxt(1,mypos,5,0,left,"",0,"Enter the date as two numeric digits for the month, then a dash, and two digits for the year." ,0 ) !:
        let resp$(1)=""
00190   fnlbl(2,1,"Name Format (F=first name first; L=Last name first):",mylen,right)
00195   fntxt(2,mypos,1,0,left,"",0,"Enter 'F' if first name first; else 'L' if last name shown first." ,0 ) !:
        let resp$(2)=""
00200   fnlbl(3,1,"Enter the location to save employees to CSV for QuickFile",mylen,right)
00205   fntxt(3,mypos,60,0,left,"",0,"Enter a CSV file path.",0)
00210   fnreg_read("TexasUCFile",csvpath$)
00215   if trim$(csvpath$)="" then csvpath$=env$('Q')&"\GLmstr\txuc.csv"
00220   let resp$(3)=csvpath$
00225   fncmdset(2)
00230   fnacs(sn$,0,mat resp$,ckey)
00235   if ckey=5 then goto XIT
00240   let pedat$=resp$(1)
00245   let namcde$=uprc$(resp$(2))
00250   csvpath$=resp$(3)
00255   if trim$(namcde$)="" then goto L170
00280 L280: open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00290   open #(h_csv:=fngethandle): "Name="&csvpath$&",REPLACE",display,output 
00300   fnopenprn
00310   gosub HDR
00315 L310: read #2,using L320: mat k,mat k$,mat l$,mat m eof L600
00320 L320: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
00330   if m(2)=0 or k(1)=0 then goto L500
00340   let deducy=deducq=0
00350   for j=1 to 10
00360     if deduc(j)=1 then let deducy=deducy+m(j*2+9)
00370     if deduc(j)=1 then let deducq=deducq+m(j*2+10)
00380   next j
00390   let m(1)=m(1)-deducy
00400   let m(2)=m(2)-deducq
00410   if p1<57 then goto L450
00420   gosub L820
00430   pr #255: newpage
00440   gosub HDR
00450 L450: gosub L670
00460   let t1=t1+m(2)
00470   let t2=t2+h3
00480   let t3=t3+h2
00490   let t4=t4+m(34)
00500 L500: goto L310
00510 ! ______________________________________________________________________
00520 HDR: ! 
00530   pr #255,using L540: b$(2),b$(1),pedat$
00540 L540: form skip 5,pos 2,c 12,pos 52,c 12,pos 67,c 6,skip 3
00550   pr #255,using L560: a$(1),a$(2),a$(3)
00560 L560: form pos 22,c 40,skip 1,pos 22,c 40,skip 1,pos 22,c 40,skip 6
00570   let p1=16
00580   return 
00590 ! ______________________________________________________________________
00600 L600: gosub L820
00610   close #2: 
00620   fncloseprn
00630   fnreg_write("TexasUCFile",csvpath$)
00640   close #h_csv: 
00650   goto XIT
00660 ! ______________________________________________________________________
00670 XIT: let fnxit
00680 ! ______________________________________________________________________
00685 L670: let p3=p3+1
00690   if m(1)<ucm then goto L740
00695   if m(1)-m(2)>ucm then goto L720
00700   let h2=ucm-(m(1)-m(2))
00705   goto L750
00720 L720: let h2=0
00730   goto L750
00740 L740: let h2=m(2)
00750 L750: let h3=m(2)-h2
00760   gosub L1000 ! break name down
00770   let f$=first$(1:1): let m$=mid$(1:1)
00790   pr #255,using L790: l$(1),f$,m$,last$,m(2)
00800   pr #h_csv: l$(1)&","""&srep$(first$,"""","""""")&""","&m$&","""&srep$(last$,"""","""""")&""","&str$(m(2))
00805 L790: form pos 4,c 11,pos 21,c 1,pos 24,c 1,pos 27,c 16,pos 51,pic(zzz,zzz.##),skip 2
00810   let p1=p1+2
00815   return 
00820 L820: let j1=68-p1
00830   pr #255,using L840: t1
00840 L840: form skip j1,pos 47,pic(zzz,zzz,zzz.##)
00850   let p3=0
00860   let t1=0
00870   let t2=0
00880   let t3=0
00890   let t4=0
00900   return 
00910 ! ______________________________________________________________________
00920 ! <Updateable Region: ERTN>
00930 ERTN: let fnerror(program$,err,line,act$,"xit")
00940   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00950   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00960   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00970 ERTN_EXEC_ACT: execute act$ : goto ERTN
00980 ! /region
00990 ! ______________________________________________________________________
01000 L1000: dim first$*15,mid$*15,last$*20,em$(3)*30
01010   let k$(1)=uprc$(rtrm$(k$(1))): ! Let NAMCDE$="s"
01020   let x1=pos(k$(1)," ",1)
01030   let x2=pos(k$(1)," ",x1+1)
01040   let x3=pos(k$(1)," ",x2+1)
01050   if uprc$(namcde$)="L" then goto L1100
01060   let first$=k$(1)(1:max(min(15,x1-1),1))
01070   if x2>0 then let mid$=k$(1)(x1+1:x2-1): last$=k$(1)(x2+1:len(k$(1)))
01080   if x2=0 then last$=k$(1)(x1+1:len(k$(1))): let mid$=""
01090   goto L1140
01100 L1100: ! last name first
01110   if x1>0 and k$(1)(x1-1:x1-1)="," then last$=k$(1)(1:x1-2) else last$=k$(1)(1:max(x1-1,1))
01120   if x2>0 then let first$=k$(1)(x1+1:x2-1): let mid$=k$(1)(x2+1:len(k$(1)))
01130   if x2=0 then let first$=k$(1)(x1+1:len(k$(1))): let mid$=""
01140 L1140: ! pr FIRST$,MID$,LAST$
01150   return 
