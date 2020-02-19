00010 ! Replace S:\acsPR\newjcNamLst
00020 ! pr Name and Number List
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncloseprn,fnopenprn,fncno,fnerror,fndat,fnprocess,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnChk
00050   on error goto Ertn
00060 !
00070   dim dat$*20,jn$*6,n$*40,cn$*11,cnt$*5,k$*25,cap$*128,p$(20)*50,io1$(2)
00080   dim cnam$*40,message$*40
00090 !
00100   fntop("S:\acsPR\NEWjcNamLst",cap$="Name and Number List")
00110   fncno(cno,cnam$) !:
        fndat(dat$)
00120 ! 
00130 !
00140 !
00150   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00160   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00170   prtcat$="N"
00180   if fnprocess=1 then goto L330
00190 !
00200 MAIN_SCREEN: ! 
00210   fnTos(sn$="namlst1") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00220   fnLbl(1,1,"Report Heading Date:",23,left)
00230   fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        resp$(resp+=1)=dat$
00240   fnChk(2,mypos,"Print Category Names:",left) !:
        resp$(resp+=1)="False"
00250   fnCmdSet(2)
00260   fnAcs(sn$,0,mat resp$,ck)
00270   if ck=5 then goto XIT
00280   dat$=resp$(1) ! heading date
00290   if resp$(2)="True" then prtcat$="Y" else prtcat$="N"
00300   dattab=60-len(rtrm$(dat$))/2
00310   fndat(dat$,2)
00320 !
00330 L330: on fkey 5 goto L540
00340   fnopenprn
00350   gosub HDR
00360 L360: read #1,using L370: jn$,n$ eof L510
00370 L370: form pos 1,c 6,c 40
00380   first=0
00390   goto L420
00400   pr #255: newpage
00410   gosub HDR
00420 L420: if prtcat$="N" then goto L360
00430   cnt$="    0"
00440   read #2,using L470,key>=jn$&cnt$: cn$,k$ exit L510
00450   goto L480
00460 L460: read #2,using L470: cn$,k$ exit L510
00470 L470: form pos 1,c 11,c 25
00480 L480: if cn$(1:6)><jn$ then goto L360
00490   gosub L700
00500   goto L460
00510 L510: exit eof L520,nokey L520
00520 L520: close #1: 
00530   close #2: 
00540 L540: fncloseprn
00550   fnxit
00560 !
00570 HDR: ! 
00580   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00590   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00600   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00610   pr #255: "\qc  {\f201 \fs20 \b "&trim$(dat$)&"}"
00620   pr #255: "\ql   "
00630   if prtcat$="N" then pr #255,using L640: "  Job #   Job Name"
00640 L640: form pos 1,c 50,skip 0
00650   if prtcat$<>"N" then pr #255,using L660: "  Job #   Job Name","Category #   Category Name"
00660 L660: form pos 1,c 50,pos 47,c 50,skip skh
00670   skh=1
00680   return 
00690 !
00700 L700: if first=0 then pr #255,using L710: jn$,n$,cn$(7:11),k$ pageoflow L750: first =1: goto L740
00710 L710: form skip 2,pos 1,c 6,pos 10,c 40,x 1,c 5,pos 60,c 25,skip 1
00720   pr #255,using L730: cn$(7:11),k$ pageoflow L750
00730 L730: form pos 51,c 5,pos 60,c 25
00740 L740: goto L770
00750 L750: pr #255: newpage
00760   gosub HDR
00770 L770: return 
00780 !
00790 XIT: fnxit
00800 !
00810 ! <Updateable Region: ERTN>
00820 ERTN: fnerror(program$,err,line,act$,"xit")
00830   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00840   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00850   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00860 ERTN_EXEC_ACT: execute act$ : goto ERTN
00870 ! /region
00880 !
