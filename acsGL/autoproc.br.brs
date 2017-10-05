00010 ! Replace S:\acsGL\AutoProc
00020 ! GL - Begin Atomatic Processing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnopenprn,fncloseprn,fnputcno,fnchain,fnprocess,fnpgnum,fncursys$,fntos,fnlbl,fncomboa,fncmdkey,fnacs,fngetdir,fnflexinit1,fnflexadd1
00050   fntop(program$,cap$="Begin Automatic Processing")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim clnum(20),clnam$(20)*40,cnam$*40,a$*40,cap$*128,oldcnam$*40
00090   dim filename$(100)*40,opt$(100)*40,sys$*100,item$(2)*40,resp$(10)*100
00100 ! ______________________________________________________________________
00110   cap$="Begin Automatic Processing"
00120   fncno(cno,oldcnam$)
00130   open #glclnt=1: "Name="&env$('Q')&"\GLmstr\glClnt.dat",internal,outin,relative ioerr BUILD_GLCLNT
00140   rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=1: cno,cnam$
00150   goto L210
00160 ! ______________________________________________________________________
00170 BUILD_GLCLNT: ! 
00180   open #glclnt=1: "Name="&env$('Q')&"\GLmstr\glClnt.dat,Size=0,RecL=45",internal,outin,relative 
00190   for j=1 to 20 !:
          write #glclnt,using 'Form POS 1,N 5,C 40',rec=j: 0," " !:
        next j
00200 ! ______________________________________________________________________
00210 L210: if trim$(mysys$)='' then !:
          sys$=fncursys$&"mstr" else !:
          sys$=mysys$&"mstr"
00220 ! ______________________________________________________________________
00230   gosub BLD_ACNO
00240 ! ______________________________________________________________________
00250 MAIN: ! 
00260   fntos(sn$="autoproc") !:
        respc=0
00270   fnlbl(1,20,"Select From the Following Companies:",50,1)
00280   fnlbl(2,1,"Company:",10,1)
00290   for j=1 to udim(opt$)
00300     if trim$(oldcnam$)=trim$(opt$(j)(1:30)) then resp$(1)=opt$(j): goto L330
00310   next j
00320   resp$(1)=opt$(1) ! default to 1st in none found
00330 L330: fncomboa('CmbAuto',2,13,mat opt$,'Select the companies that should be included in this automatic processing run. Highlite and press enter or Next to register your selection.',55) ! fnCMBCNO(1, 13)
00340   fnlbl(4,30,"Selected Companies:")
00350   mat chdr$(2) : mat cmask$(2) : mat item$(2) !:
        chdr$(1)='Company #' !:
        chdr$(2)='Company Name'
00360   cmask$(1)='30' !:
        cmask$(2)='' !:
        fnflexinit1('autoproc',5,25,15,35,mat chdr$,mat cmask$,1,0,frame)
00370   for j=1 to max(count,1)
00380     item$(1)=str$(clnum(j)) !:
          item$(2)=clnam$(j) !:
          fnflexadd1(mat item$)
00390   next j
00400   fncmdkey("&Select",1,1,0,"Selects the highlited company to be included in automatic processing.") !:
        fncmdkey("C&omplete",2,0,0,"Finished selecting companies; begin porcessing.") !:
        fncmdkey("&Cancel",5,0,1)
00410   fnacs(sn$,0,mat resp$,ckey)
00420   if ckey=5 then goto XIT
00430   if ckey=2 then goto L470
00440   clnam$(count+=1)=resp$(1)(1:30)
00450   clnum(count)=val(resp$(1)(33:37))
00460   if ckey=1 then goto MAIN
00470 L470: goto WRITE_EM
00480 ! ______________________________________________________________________
00490 BLD_ACNO: ! 
00500   dir$=fncursys$&"mstr" !:
        let filter$="Company.*" !:
        fngetdir(dir$,mat filename$,empty$,filter$)
00510   mat acno(99): cav=0
00520 L520: if trim$(filename$(fx+=1))="" then goto L580
00530   acno(cav+=1)=val(filename$(fx)(10:14)) conv L520
00540   end=len(filename$(fx))
00550   let x=115: open #x: "Name="&sys$&"\Company.h"&filename$(fx)(10:14),internal,input ioerr L570 !:
        read #x,using "Form pos 1,c 40": cnam$ !:
        close #x: 
00560   opt$(fx)=cnam$(1:30)&" ("&cnvrt$("pic(#####)",val(filename$(fx)(10:14)))&")"(1:40)
00570 L570: goto L520
00580 L580: mat opt$(fx)
00590   return 
00600 WRITE_EM: ! 
00610   restore #glclnt: 
00620   for j=1 to 20 !:
          rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=j: clnum(j),clnam$(j) !:
        next j
00630   goto BEGIN_AUTO
00640 ! ______________________________________________________________________
00650 BEGIN_AUTO: ! 
00660   close #glclnt: 
00670   execute "Load S:\Core\Process.br,RESIDENT"
00680   fnprocess(1)
00690   fnputcno(clnum(1)) !:
        fnpgnum(-1) !:
        ! resets the last program processed back to 0 befor going to acglauto
00700   fnchain("S:\acsGL\acglAuto")
00710 ! ______________________________________________________________________
00720 XIT: fnxit
00730 ! ______________________________________________________________________
00740 ! <Updateable Region: ERTN>
00750 ERTN: fnerror(program$,err,line,act$,"xit")
00760   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00770   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00780   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00790 ERTN_EXEC_ACT: execute act$ : goto ERTN
00800 ! /region
00810 ! ______________________________________________________________________
