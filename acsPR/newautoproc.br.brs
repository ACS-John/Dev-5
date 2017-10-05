00010 ! Replace S:\acsPR\newAutoProc
00020 ! PR - Begin Atomatic Processing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3b,fncno,fnopenprn,fncloseprn,fnputcno,fnprocess,fnpgnum,fncursys$,fntos,fnlbl,fncomboa,fncmdkey,fnacs,fngetdir,fnflexinit1,fnflexadd1,fnfra,fnopt
00050   fntop(program$,cap$="Begin Automatic Processing")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim clnum(20),clnam$(20)*40,cnam$*40,io1$(2)*18 ,a$*40,cap$*128,oldcnam$*40
00090   dim filename$(100)*40,opt$(100)*40,sys$*100,item$(2)*40,resp$(10)*100
00100   dim wk(20),mo(20),qt(20)
00110 ! ______________________________________________________________________
00120   cap$="Begin Automatic Processing"
00130   fncno(cno,oldcnam$)
00170 ! ______________________________________________________________________
00180 BUILD_PRCLNT: ! 
00190   open #prclnt=1: "Name="&env$('Q')&"\PRmstr\prclnt.dat,Size=0,RecL=48,REPLACE",internal,outin,relative 
00200   for j=1 to 20 !:
          write #prclnt,using 'Form POS 1,N 5,C 40,3*N 1',rec=j: 0," ",0,0,0 !:
        next j
00210 ! ______________________________________________________________________
00220   if trim$(mysys$)='' then !:
          sys$=fncursys$&"mstr" else !:
          sys$=mysys$&"mstr"
00230 ! ______________________________________________________________________
00240   gosub BLD_ACNO
00250 ! ______________________________________________________________________
00260 MAIN: ! 
00270   fntos(sn$="autoproc") !:
        respc=0
00280   fnlbl(1,20,"Select From the Following Companies:",50,1)
00290   fnlbl(2,1,"Company:",10,1)
00300   for j=1 to udim(opt$)
00310     if trim$(oldcnam$)=trim$(opt$(j)(1:30)) then resp$(respc+=1)=opt$(j): goto L340
00320   next j
00330   resp$(1)=opt$(1) ! default to 1st in none found
00340 L340: fncomboa('CmbAuto',2,13,mat opt$,'Select the companies that should be included in this automatic processing run. Highlite and press enter or Next to register your selection.',55) ! fnCMBCNO(1, 13)
00350   fnlbl(4,30,"Selected Companies:")
00360   mat chdr$(2) : mat cmask$(2) : mat item$(2) !:
        chdr$(1)='Company #' !:
        chdr$(2)='Company Name'
00370   cmask$(1)='30' !:
        cmask$(2)='' !:
        fnflexinit1('autoproc',5,25,15,35,mat chdr$,mat cmask$,1,0,frame)
00380   for j=1 to max(count,1)
00390     item$(1)=str$(clnum(j)) !:
          item$(2)=clnam$(j) !:
          fnflexadd1(mat item$)
00400   next j
00410   fnfra(22,1,4,30,"Period to Print","Select the type of processing.")
00420   fnopt(1,3,"Weekly",0,1) !:
        if wmq=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00430   fnopt(2,3,"Monthly",0,1) !:
        if wmq=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00440   fnopt(3,3,"Quarterly",0,1) !:
        if wmq=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
00450   fncmdkey("&Next",1,1,0,"Selects the highlited company to be included in automatic processing.") !:
        fncmdkey("C&omplete",2,0,0,"Finished selecting companies; begin porcessing.") !:
        fncmdkey("&Cancel",5,0,1)
00460   fnacs(sn$,0,mat resp$,ckey)
00470   if ckey=5 then goto XIT
00480   if ckey=2 and count>0 then goto L550
00490   clnam$(count+=1)=resp$(1)(1:30)
00500   clnum(count)=val(resp$(1)(33:37))
00510   if resp$(3)="True" then let wk(count)=1: let wmq=1
00520   if resp$(4)="True" then mo(count)=1: let wmq=2
00530   if resp$(5)="True" then let qt(count)=1: let wmq=3
00540   if ckey=1 then goto MAIN
00550 L550: goto WRITE_EM
00560 ! ______________________________________________________________________
00570 BLD_ACNO: ! 
00580   dir$=env$('Q')&'\&'&fncursys$&"mstr"
00582   let filter$="Company.*"
00584   fngetdir(dir$,mat filename$,empty$,filter$)
00590   mat acno(99999): cav=0
00600 L600: if trim$(filename$(fx+=1))="" then goto L660
00610   acno(cav+=1)=val(filename$(fx)(10:14)) conv L600
00620   end=len(filename$(fx))
00630   let x=115
00631   open #x: "Name="&sys$&"\Company.h"&filename$(fx)(10:14),internal,input ioerr L650
00632   read #x,using "Form pos 1,c 40": cnam$
00633   close #x: 
00640   opt$(fx)=cnam$(1:30)&" ("&cnvrt$("pic(#####)",val(filename$(fx)(10:14)))&")"(1:40)
00650 L650: goto L600
00660 L660: mat opt$(fx)
00670   return 
00680 WRITE_EM: ! 
00690   restore #prclnt: 
00700   for j=1 to 20
00702     rewrite #prclnt,using 'Form POS 1,N 5,C 40,3*N 1',rec=j: clnum(j),clnam$(j),wk(j),mo(j),qt(j)
00704   next j
00710   goto BEGIN_AUTO
00720 ! ______________________________________________________________________
00730 BEGIN_AUTO: ! 
00740   close #prclnt: 
00750   execute "Load S:\Core\Process.br,RESIDENT"
00760   fnprocess(1)
00770   fnputcno(clnum(1))
00774   fnpgnum(-1) ! resets the last program processed back to 0 befor going to NEWPRauto
00800 XIT: fnxit
00810 ! ______________________________________________________________________
00820 ! <Updateable Region: ERTN>
00830 ERTN: fnerror(program$,err,line,act$,"xit")
00840   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00850   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00860   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00870 ERTN_EXEC_ACT: execute act$ : goto ERTN
00880 ! /region
00890 ! ______________________________________________________________________
