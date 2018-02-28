00010 ! Replace S:\acsPR\conversion\fixmedicare
00020 ! special program to fix medicare wh
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$*30,em(6),cap$*128,message$*40
00080   dim cnam$*40,dat$*20,t1(20),t2(20)
00090   dim dedcode(20),calcode(20),dedfed(20),cnam$*40
00100   dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00110   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
00120   dim e$(10)*12,tpt(32),cap$*128,message$*40,resp$(15)*30
00130   dim tcp(32),tdc(10),ytdtotal(32),ss$*11,d1$*20
00140 ! ______________________________________________________________________
00150   fntop("S:\acsPR\fixmedicare",cap$="Fix Medicare")
00160   fncno(cno,cnam$)
00170   fnopenprn
00180   open #2: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
00190   open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
00200 ! 
00210 L210: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof XIT
00220   if tcp(3)=0 and tcp(2)>0 then tcp(3)=round(tcp(2)*.189542,2): tcp(2)=tcp(2)-tcp(3) : goto L240
00230   goto L210
00240 L240: rewrite #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp
00250   goto L210
00260 ! ______________________________________________________________________
00270 XIT: fnxit
00280 ! ______________________________________________________________________
00290 ! <Updateable Region: ERTN>
00300 ERTN: fnerror(program$,err,line,act$,"xit")
00310   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00330   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00340 ERTN_EXEC_ACT: execute act$ : goto ERTN
00350 ! /region
00360 ! ______________________________________________________________________
