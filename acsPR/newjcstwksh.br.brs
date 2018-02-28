00010 ! Replace S:\acsPR\newjcStWkSh
00020 ! pr Job Status Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnTos,fnLbl,fnTxt,fnChk,fnCmdSet,fnAcs,fncmbjob,fnmsgbox,fnCmdKey
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,next$*5,cn$*11,cnt$*5,k$*25,cap$*128,resp$(3)*40,cnam$*40
00080   dim sc1$(3),sd1$(3),se1$(3)*50,prtj$(100)*6,dat$*20
00090   dim ml$(1)*80
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Job Status Worksheet")
00120   fncno(cno,cnam$) !:
        fndat(dat$)
00130 ! 
00140   prtjob$="N" : perpag$="N"
00150   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00160   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00170 ! ______________________________________________________________________
00180   if fnprocess=1 then goto ASKJOB
00190 MAIN_SCREEN: ! 
00200   fnTos(sn$="namlst1") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00210   fnLbl(1,1,"Report Heading Date:",23,left)
00220   fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        resp$(resp+=1)=dat$
00230   fnChk(2,mypos,"Print All Jobs:",left) !:
        resp$(resp+=1)="False"
00240   fnChk(3,mypos,"Print One Job Per Page:",left) !:
        resp$(resp+=1)="False"
00250   fnCmdSet(2)
00260   fnAcs(sn$,0,mat resp$,ck)
00270   if ck=5 then goto XIT
00280   dat$=resp$(1) ! heading date
00290   if resp$(2)="True" then prtjob$="Y" else prtjob$="N"
00300   if resp$(3)="True" then perpag$="Y" else perpad$="N"
00310 ! ______________________________________________________________________
00320   fndat(dat$,2)
00330   if prtjob$="N" then goto ASKJOB
00340   if fnprocess=1 then goto L510
00350 ! ______________________________________________________________________
00360   mat ml$(1) !:
        ml$(1)="Do you wish to skip all completed jobs?" !:
        fnmsgbox(mat ml$,resp$,cap$,4)
00370   if resp$="Yes" then skpcom$="Y" else skpcom$="N"
00380   goto L510
00390 ASKJOB: ! 
00400   for j=1 to 100
00410     fnTos(sn$="prtdet2") !:
          mylen=12 : mypos=mylen+3: resp=0: left=1 !:
          fnLbl(1,1,"Job Number:",mylen,1) !:
          fncmbjob(1,mypos) !:
          resp$(respc+=1)=jn$
00420     prtj$(j)=lpad$(rtrm$(prtj$(j)),6)
00430     fnCmdKey("&Next",1,1,0,"Print this job." ) !:
          fnCmdKey("&Complete",5,0,1,"No more jobs. Release the print.")
00440     fnAcs(sn$,0,mat resp$,ck)
00450     if ck=5 then goto L490
00460     prtj$(j)=lpad$(rtrm$(resp$(1)(1:6)),6)
00470   next j
00480   goto L510
00490 L490: j=j-1
00500 ! ______________________________________________________________________
00510 L510: on fkey 5 goto DONE
00520   fnopenprn !:
        if file$(255)(1:3)<>"PRN" then jbskip=1
00530   gosub HDR
00540 L540: if prtjob$="Y" then goto L590
00550 L550: j1+=1: if j1>j then goto DONE
00560   read #1,using L570,key=prtj$(j1): jn$,n$,b4 nokey L550
00570 L570: form pos 1,c 6,c 40,pos 157,n 2
00580   goto L600
00590 L590: read #1,using L570: jn$,n$,b4 eof DONE
00600 L600: if skpcom$="Y" and b4=9 then goto L540
00610   gosub L870
00620   cnt$="    0"
00630 L630: read #2,using L640,key>=jn$&cnt$: cn$,k$,l12,l13 eof L540,nokey L540
00640 L640: form pos 1,c 11,c 25,pos 114,2*pd 2
00650   if cn$(1:6)><jn$ then goto L540
00660   gosub L960
00670   cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
00680   goto L630
00690 ! ______________________________________________________________________
00700 DONE: close #1: 
00710   close #2: 
00720   fncloseprn
00730   goto XIT
00740 ! ______________________________________________________________________
00750 HDR: ! 
00760   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00770   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00780   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00790   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00800   pr #255: "\ql   "
00810   form pos 56,c 20,skip 1,pos 1,cc 132,skip 2
00820   pr #255: "{\b  Job       Job Name                           Category   Description                 Old %   Percent     Units}"
00830   pr #255: "{\b Number                                         Number                               Complete Complete  Complete}"
00840   pr #255: "\ql   "
00850   return 
00860 ! ______________________________________________________________________
00870 L870: if fst=1 then goto L880 else goto L910
00880 L880: if perpag$="N" then goto L920
00890   pr #255: newpage
00900   gosub HDR
00910 L910: fst=1
00920 L920: pr #255,using L930: jn$,n$
00930 L930: form pos 1,c 6,pos 8,c 40,skip jbskip
00940   return 
00950 ! ______________________________________________________________________
00960 L960: pr #255,using L1000: cn$(7:11),k$,"LABOR",l12,"%","___%","_____.__" pageoflow L980
00970   goto L1010
00980 L980: pr #255: newpage
00990   gosub HDR
01000 L1000: form pos 49,c 5,pos 55,c 25,pos 81,c 5,pos 89,n 3,c 1,pos 97,c 4,pos 107,c 8,skip 1
01010 L1010: pr #255,using L1000: " "," ","OTHER",l13,"%","___%","        " pageoflow L1040
01020   pr #255: pageoflow L1040
01030   goto L1060
01040 L1040: pr #255: newpage
01050   gosub HDR
01060 L1060: return 
01070 ! ______________________________________________________________________
01080 XIT: fnxit
01090 ! ______________________________________________________________________
01100 ! <Updateable Region: ERTN>
01110 ERTN: fnerror(program$,err,line,act$,"xit")
01120   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01130   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01140   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01150 ERTN_EXEC_ACT: execute act$ : goto ERTN
01160 ! /region
01170 ! ______________________________________________________________________
