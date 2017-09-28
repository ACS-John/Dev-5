00010 ! Replace S:\acsUB\TypeOfService
00020 ! -- Select Type of Service
00030   fn_setup
00100   fntop(program$,cap$="Type of Service")
00130   open #service:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",RecL=280,use",internal,outin,relative 
00140   read #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply norec SERVICE_WRITE
00150 F_SERVICE: form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*n 2,10*n 2
00160   goto TYPEOSERVICE
00170 SERVICE_WRITE: ! 
00180   write #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
00190   goto TYPEOSERVICE
00200 ! ______________________________________________________________________
00210 TYPEOSERVICE: ! 
00220   sn$="TypeOService"
00230   fntos(sn$)
00240   fnlbl(2,13,"Full Name",20,2)
00250   fnlbl(2,34,"Code",4,0)
00260   fnlbl(2,39,"Taxable",7,0)
00270   fnlbl(2,47,"Penalty",7,0)
00280   fnlbl(1,55,"Subject",7,2)
00290   fnlbl(2,55,"To",7,2)
00300   fnlbl(1,66,"Order to apply",14,2)
00310   fnlbl(2,66,"Collection",14,2)
00320   for a=1 to 10
00330     text$="Service "&str$(a)&":"
00340     fnlbl(a+2,1,text$,11,1)
00350     resp$(a*6-5)=servicename$(a)
00360     fntxt(a+2,13,20)
00370     resp$(a*6-4)=servicecode$(a)
00380     fntxt(a+2,34,3)
00390     fnchk(a+2,41,"",align=0,container=0,tabcon=0)
00400     if tax_code$(a)="Y" then let resp$(a*6-3)="True" else let resp$(a*6-3)="False"
00410     fnchk(a+2,49,"",align=0,container=0,tabcon=0)
00420     if penalty$(a)="Y" then let resp$(a*6-2)="True" else let resp$(a*6-2)="False"
00430     resp$(a*6-1)=str$(subjectto(a))
00440     fntxt(a+2,58,2,0,0,"30")
00450     resp$(a*6)=str$(ordertoapply(a))
00460     fntxt(a+2,72,2,0,0,"30")
00470   next a
00480   fncmdset(4)
00490   fnacs(sn$,0,mat resp$,ckey)
00500   if ckey=5 then goto XIT
00510   for a=1 to 10
00520     servicename$(a)=resp$(a*6-5)
00530     servicecode$(a)=uprc$(resp$(a*6-4))
00540     if resp$(a*6-3)="True" then 
00550       tax_code$(a)="Y"
00560     else 
00570       tax_code$(a)="N"
00580     end if 
00590     if resp$(a*6-2)="True" then 
00600       penalty$(a)="Y"
00610     else 
00620       penalty$(a)="N"
00630     end if 
00640     subjectto(a)=val(resp$(a*6-1))
00650     ordertoapply(a)=val(resp$(a*6))
00660   next a
00670   rewrite #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
00680   goto XIT
00690 ! ______________________________________________________________________
00700 XIT: ! 
00710   close #service: 
00720   fnxit
00730 ! ______________________________________________________________________
32000 ! <Updateable Region: ERTN>
32020 ERTN: fnerror(program$,err,line,act$,"xit")
32040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
32060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
32080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
32100 ERTN_EXEC_ACT: execute act$ : goto ERTN
32120 ! /region
34000   def fn_setup
34020     if ~setup then 
34040       setup=1
34060       library 'S:\Core\Library': fntop,fnxit, fntxt,fnlbl,fntos,fnacs,fnerror,fncmdset,fnchk,fngethandle
34080       on error goto ERTN
34100 ! 
34120       dim resp$(60)*20,servicename$(10)*20,servicecode$(10)*2
34140       dim tax_code$(10)*1,cap$*128
34160 ! 
34180     end if 
34200   fnend 
36000   def library fnget_services(mat servicename$; mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
36020     fn_setup
36040     fnget_services=fn_get_services(mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
36060   fnend 
38000   def fn_get_services(mat servicename$; mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
38020     open #h_temp_service:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",Shr",internal,input,relative ioerr GsXit
38040     read #h_temp_service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
38060     close #h_temp_service: 
38080     GsXit: !
38100   fnend 
