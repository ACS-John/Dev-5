00020 ! Maintain Department Matching GL Numbers
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncloseprn,fnopenprn,fncno,fntop,fnxit,fntos,fnlbl,fncmdkey,fnacs,fnqgl,fnrgl$,fncombof,fnagl$,fnmsgbox,fntxt,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim label1$(22)*20,io2$(34),mgl$(11)*12
00080   dim rpnames$(86)*20,label1p$(12)*14,io1$(6),wrd1$(6)*40,err$(2)*65
00090   dim fullname$(20)*20,abbrevname$(20)*8,calcode(20),dedfed(20),dedfica(20)
00100   dim dedst(20),deduc(20),gl$(20)*12,ml$(3)*80,resp$(15)*60,dedcode(20)
00110   dim deptname$*20
00120   dim cap$*128
00130 ! ______________________________________________________________________
00140   fntop(program$,cap$="Accrued Payroll Tax Information")
00160   gosub BLDSCR
00170 ! ______________________________________________________________________
00180   if exists(env$('Q')&"\PRmstr\MGLMstr.h"&env$('cno'))=0 then gosub CREATE_FILES
00190   open #1: "Name="&env$('Q')&"\PRmstr\MGLMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\MGLIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00200   open #9: "Name="&env$('Q')&"\PRmstr\DeptName.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptNameIdx.h"&env$('cno')&",Shr",internal,input,keyed 
00210 ! ______________________________________________________________________
00220 MENU1: ! 
00230 ASKDEPARTMENT: ! 
00240   fntos(sn$="Department-ask") !:
        respc=0
00250   fnlbl(1,1,"Department #:",15,1)
00260   fncombof("Dept",1,18,3,env$('Q')&"\PRmstr\mglmstr.h"&env$('cno'),1,3,0,0,env$('Q')&"\PRmstr\mglidx1.h"&env$('cno'),0,0, "Set the matching g/l numbers for accruing payroll taxes by department. Choose a department.",0,0) !:
        resp$(respc+=1)=""
00270   fncmdkey("&Add",1,0,0,"Enter accrual information on a new department." ) !:
        fncmdkey("E&dit",2,1,0,"Change or review then highlighted record") !:
        fncmdkey("&Delete",3,0,0,"Deletes the highlited record.") !:
        fncmdkey("E&xit",5,0,1,"Returns to menu")
00280   fnacs(sn$,0,mat resp$,ckey) ! ask department #
00290   if ckey=5 then goto XIT
00300   dp$=resp$(1)
00310   dp$=lpad$(uprc$(rtrm$(dp$)),3)
00320   addrec=editrec=0
00330   if ckey=1 then addrec=1 else !:
          if ckey=2 then editrec=1 else !:
            if ckey=3 then goto DELETE_RECORD
00340 ! ______________________________________________________________________
00350 ADD_EDIT_REC: ! 
00360   deptname$="": read #9,using "form pos 4,c 20",key=rpad$(ltrm$(dp$),3): deptname$ nokey L370
00370 L370: if addrec=1 then dp$="": mat mgl$=(""): goto L400
00380   k$=dp$=lpad$(uprc$(rtrm$(dp$)),3)
00390   read #1,using "Form POS 1,G 3,11*C 12",key=k$,release: dp$,mat mgl$ nokey MENU1
00400 L400: fntos(sn$="Department-gl") !:
        respc=0
00410   fnlbl(1,1,deptname$,50,2)
00420   fnlbl(2,1,"Department #:",15,1)
00430   fntxt(2,18,3,3,1,"30",0,"Department # to change or add.") !:
        resp$(respc+=1)=dp$
00440   fnlbl(3,1,label1$(1),15,1)
00450   fnqgl(3,18,0,2,0) !:
        resp$(respc+=1)=fnrgl$(mgl$(1)) ! fica
00460   let x=1 : let y=3
00470   for j=1 to 20
00480     if dedcode(j)=3 then goto L490 else goto L510
00490 L490: fnlbl(y+=1,1,label1$(x+=1),15,1)
00500     fnqgl(y,18,0,2,0) !:
          resp$(respc+=1)=fnrgl$(mgl$(j+1))
00510 L510: next j
00520   fncmdkey("&Next",1,1,0,"Save changes and move to next record" ) !:
        fncmdkey("&Complete",5,0,1,"Returns to menu")
00530   fnacs(sn$,0,mat resp$,ckey) ! ask gl numbers
00540   if ckey=5 then goto XIT
00550   dp$=resp$(1)
00560   dp$=lpad$(uprc$(rtrm$(dp$)),3)
00570   mgl$(1)=fnagl$(resp$(2)) ! fica
00580   let x=2
00590   for j=1 to 20
00600     if dedcode(j)=3 then goto L610 else goto L620
00610 L610: mgl$(j+1)=fnagl$(resp$(x+=1))
00620 L620: next j
00630   if addrec=1 then goto ADD_RECORD else goto REWRITE_RECORD
00640 ! ______________________________________________________________________
00650 DELETE_RECORD: ! 
00660   mat ml$(2) !:
        ml$(1)="You have chosen to delete department # "&dp$ ! " !:
        ml$(2)="Take OK to delete; else Cancel to retain the record." !:
        fnmsgbox(mat ml$,resp$,cap$,1)
00670   if resp$="OK" then goto L680 else goto MENU1
00680 L680: delete #1,key=dp$: nokey MENU1
00690   goto MENU1
00700 REWRITE_RECORD: ! 
00710   rewrite #1,using "Form POS 1,G 3,11*C 12",key=dp$: dp$,mat mgl$ nokey L740
00720   goto MENU1
00730 ADD_RECORD: ! 
00740 L740: write #1,using "Form POS 1,G 3,11*C 12": dp$,mat mgl$
00750   goto MENU1
00760 ! ______________________________________________________________________
00770 CREATE_FILES: ! 
00780   close #1: ioerr L790
00790 L790: close #2: ioerr L800
00800 L800: open #1: "Name="&env$('Q')&"\PRmstr\MGLMstr.h"&env$('cno')&",RecL=135,Replace",internal,output 
00810   close #1: 
00820   execute "Index "&env$('Q')&"\PRmstr\MGLMstr.h"&env$('cno')&","&env$('Q')&"\PRmstr\MGLIdx1.h"&env$('cno')&",1,3,Replace,DupKeys"
00830   return 
00840 ! ______________________________________________________________________
00850   restore #1: 
00860   pg=0
00870   hp1=66-int(len(rtrm$(env$('cnam')))/2)
00880   fnopenprn (cp,58,230,process)
00890   gosub HDR4
00900 L900: read #1,using "Form POS 1,G 3,11*C 12",release: dp$,mat mgl$ eof END4
00910   pr #255,using L920: dp$,mat mgl$ pageoflow NWPG
00920 L920: form pos 1,c 6,11*c 14,skip 1
00930   goto L900
00940 ! ______________________________________________________________________
00950 NWPG: pr #255: newpage: gosub HDR4: continue 
00960 ! ______________________________________________________________________
00970 HDR4: pg=pg+1
00980   pr #255,using L990: "Page",pg,env$('cnam')
00990 L990: form pos 1,c 4,n 4,pos hp1,c 40,skip 1
01000   pr #255: date$;tab(50);"Department GL Number File Listing"
01010   pr #255: 
01020   pr #255,using L920: mat label1p$
01030   pr #255: "____  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
01040   return 
01050 ! ______________________________________________________________________
01060 END4: on fkey 5 ignore 
01070   if nw=0 then pr #255: newpage
01080   fncloseprn
01090   goto MENU1
01100 ! ______________________________________________________________________
01110 BLDSCR: ! 
01120   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
01130   label1$(1)="Fica Match"
01140   let x=1
01150   for j=1 to 20
01160     if dedcode(j)=3 then goto L1170 else goto L1180
01170 L1170: label1$(x+=1)=rtrm$(fullname$(j))
01180 L1180: next j
01190   mat label1$(x)
01200   return 
01210 ! ______________________________________________________________________
01220 XIT: fnxit
01230 ! ______________________________________________________________________
01240 ! <Updateable Region: ERTN>
01250 ERTN: fnerror(program$,err,line,act$,"xit")
01260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01290 ERTN_EXEC_ACT: execute act$ : goto ERTN
01300 ! /region
