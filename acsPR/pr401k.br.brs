00010 ! Replace S:\acsPR\pr401k
00020 ! Electronic 401k Filing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fncno,fnerror, fntop, fnxit, fndate_mmddyy_to_ccyymmdd,fncreg_read
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$*40,em$(3)*30,ta(2),cp(22),tcp(22),hc(5),thc(5),d$*20,whc(10)
00080   dim dedcode(10),calcode(10),dedfed(10),k$(20)*30,em$*30,cap$*128
00090   dim dv$*1,message$*40
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Electronic 401K")
00120   fncno(cno)
00150   fnwait(message$='',1)
00160   fncreg_read('calculation date',tmp$) : ppd=val(tmp$) 
00162   fncreg_read('calculation date text',d$)
00170   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #1,using 'Form POS 1,C 40,POS 618,30*N 1': a$,mat dedcode,mat calcode,mat dedfed : close #1: 
00180   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",Shr",internal,input,relative 
00190   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&",Shr",internal,input,relative 
00200   open #3: "Name="&env$('Q')&"\PRmstr\praddr1.h"&env$('cno'),internal,input 
00210   open #4: "Name="&env$('Q')&"\PRmstr\PR401K.DAT,RecL=235,Replace",display,output 
00220 L220: read #3,using 'Form POS 1,PD 3': address eof END1
00230   read #1,using L240,rec=address: eno,mat em$,ss$,em16,lpd,mat ta,bd noRec L220
00240 L240: form pos 1,n 8,3*c 30,c 11,pos 156,2*n 6,pos 173,2*pd 3,pos 191,n 6
00250   if lpd><ppd then goto L220
00260   a=pos (rtrm$(em$(1))," ",1)
00270   b=pos (rtrm$(em$(1))," ",a+1)
00280   c=a+1
00290   em$=ltrm$(rtrm$(em$(1)(max(a,b):30))&", "&em$(1)(1:a))
00300   if c<b then em$=em$&em$(1)(c:c)
00310   csz$=em$(3) : gosub CSZ
00320   ss$=ltrm$(rtrm$(ss$))
00330   for j=1 to len(ss$)
00340     if ss$(j:j)<"0" or ss$(j:j)>"9" then ss$(j:j)=""
00350   next j
00360   ss=0
00370   ss=val(ss$) conv L380
00380 L380: em16=fndate_mmddyy_to_ccyymmdd(em16)
00390   bd=fndate_mmddyy_to_ccyymmdd(bd)
00400   mat thc=(0)
00410   mat tcp=(0)
00420   adr=ta(1)
00430 L430: read #2,using L440,rec=adr: lpd,mat hc,mat cp,nta
00440 L440: form pos 42,n 6,pos 150,5*pd 3.2,pos 358,22*pd 5.2,pos 468,pd 3
00450   if lpd><ppd then goto L480
00460   mat tcp=tcp+cp
00470   mat thc=thc+hc
00480 L480: if nta=0 then goto L520
00490   adr=nta
00500   goto L430
00510 ! ______________________________________________________________________
00520 L520: ! 
00530   k$(1)=cnvrt$("PIC(#########)",ss)
00540   k$(2)=cnvrt$("PIC(#########.##)",tcp(12))
00550   k$(3)=cnvrt$("PIC(#########.##)",0)
00560   k$(4)=cnvrt$("PIC(#########.##)",tcp(13))
00570   k$(5)=cnvrt$("PIC(##)",0)
00580   k$(6)=cnvrt$("PIC(#######.##)",tcp(11))
00590   k$(7)=cnvrt$("PIC(##)",0)
00600   k$(8)=cnvrt$("PIC(#######.##)",0)
00610   k$(9)=em$
00620   k$(10)=em$(2)
00630   k$(12)=city$
00640   k$(13)=state$
00650   k$(14)=zip5$
00660   k$(15)=cnvrt$("PIC(########)",bd)
00670   k$(16)=cnvrt$("PIC(########)",em16)
00680   k$(17)=cnvrt$("PIC(########)",0)
00690   k$(18)=cnvrt$("PIC(########)",0)
00700   k$(19)=cnvrt$("PIC(######.##)",tcp(21))
00710   k$(20)=cnvrt$("PIC(########)",0)
00720   pr #4,using L730: mat k$
00730 L730: form pos 1,c 9,3*c 12,c 2,c 10,c 2,c 10,3*c 30,c 20,c 2,c 5,4*c 8,c 9,c 8
00740   goto L220
00750 ! ______________________________________________________________________
00760 CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
00770   dim csz$*30
00780   csz$=uprc$(rtrm$(csz$))
00790 L790: p1=pos(csz$,".",1)
00800   if p1>0 then csz$(p1:p1)="": goto L790
00810 L810: p1=pos(csz$,"  ",1)
00820   if p1>0 then csz$(p1:p1)="": goto L810
00830   p1=pos(csz$,",",1)-1
00840   if p1=-1 then p1=pos(csz$," ",1)-1
00850   if csz$(p1+2:p1+2)><" " then csz$(p1:p1+1)=csz$(p1:p1+1)&" "
00860   p2=pos(csz$," ",p1+3)
00870   city$=uprc$(rtrm$(csz$(1:p1))(1:15))
00880   if city$(1:3)="FT " then city$(1:3)="FORT "
00890   if city$(1:3)="FT. " then city$(1:3)="FORT "
00900   state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2))
00910   l1=len(csz$)
00920   zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1))))
00930   zip5$=zip$(1:5)
00940   zip4$=""
00950   l2=len(zip$)
00960   if l2<9 then goto L980
00970   zip4$=zip$(l2-3:l2)
00980 L980: return 
00990 ! ______________________________________________________________________
01000 END1: ! 
01010   close #1: 
01020   close #2: 
01030   close #3: 
01040   close #4: 
01050 L1050: pr newpage ! COPY TO DISKETTE
01060   fnopenwin(win=101,7,8,11,44,cap$)
01070   io1$(1)="8,42,CU 1,UET,N"
01080   if driv=1 then pr #win,fields "3,2,Cc 30,n": "Drive Not Ready!"
01090   pr #win,fields "4,2,C 30,n": "Insert 401k Diskette in Drive:"
01100   pr f "12,15,C 9,B,1": "Next (F1)"
01110   pr f "12,26,C 11,B,5": "Cancel (F5)"
01120   if dv$="" then dv$="A"
01130 L1130: rinput #win,fields "4,33,Cu 1,UET,N": dv$
01140   if cmdkey=5 then goto XIT
01150   if dv$="A" or dv$="B" then goto L1160 else goto L1130
01160 L1160: execute "Copy "&env$('Q')&"\PRmstr\PR401K.DAT "&dv$&": -N" ioerr L1180
01170 XIT: fnxit
01180 L1180: driv=1 ! drive not ready
01190   goto L1050
01200 ! ______________________________________________________________________
01210 ! <Updateable Region: ERTN>
01220 ERTN: fnerror(program$,err,line,act$,"xit")
01230   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01240   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01250   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01260 ERTN_EXEC_ACT: execute act$ : goto ERTN
01270 ! /region
01280 ! ______________________________________________________________________
