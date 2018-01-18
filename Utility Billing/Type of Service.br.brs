00020 fn_setup
00030 fntop(program$)
00120 fn_readService
00230 fnTos(sn$="TypeOfService") ! r:
00240   fnLbl(2,13,"Full Name",20,2)
00250   fnLbl(2,34,"Code",4,0)
00260   fnLbl(2,39,"Taxable",7,0)
00270   fnLbl(2,47,"Penalty",7,0)
00280   fnLbl(1,55,"Subject",7,2)
00290   fnLbl(2,55,"To",7,2)
00300   fnLbl(1,66,"Order to apply",14,2)
00310   fnLbl(2,66,"Collection",14,2)
00320   for a=1 to 10
00330     text$="Service "&str$(a)&":"
00340     fnLbl(a+2,1,text$,11,1)
00350     resp$(a*6-5)=serviceName$(a)
00360     fnTxt(a+2,13,20)
00370     resp$(a*6-4)=serviceCode$(a)
00380     fnTxt(a+2,34,3)
00390     fnChk(a+2,41,"",align=0,container=0,tabcon=0)
00400     if taxCode$(a)="Y" then resp$(a*6-3)="True" else resp$(a*6-3)="False"
00410     fnChk(a+2,49,"",align=0,container=0,tabcon=0)
00420     if penalty$(a)="Y" then resp$(a*6-2)="True" else resp$(a*6-2)="False"
00430     resp$(a*6-1)=str$(subjectTo(a))
00440     fnTxt(a+2,58,2,0,0,"30")
00450     resp$(a*6)=str$(orderToApply(a))
00460     fnTxt(a+2,72,2,0,0,"30")
00470   next a
00480   fnCmdSet(4)
00490   fnAcs(sn$,0,mat resp$,ckey)
22000   if ckey<>5 then 
22020     for a=1 to 10
22040       serviceName$(a)=resp$(a*6-5)
22060       serviceCode$(a)=uprc$(resp$(a*6-4))
22080       if resp$(a*6-3)="True" then 
22100         taxCode$(a)="Y"
22120       else 
22140         taxCode$(a)="N"
22160       end if 
22180       if resp$(a*6-2)="True" then 
22200         penalty$(a)="Y"
22220       else 
22240         penalty$(a)="N"
22260       end if 
22280       subjectTo(a)=val(resp$(a*6-1))
22300       orderToApply(a)=val(resp$(a*6))
22320     next a
22340     open #hService:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",RecL=280,use",internal,outIn,relative 
22360     rewrite #hService,using F_service,rec=1: mat serviceName$,mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply
22380     close #hService:
22390   end if
22400 goto Xit ! /r
28000 Xit: fnxit
32000 ! <Updateable Region: ERTN>
32020 ERTN: fnerror(program$,err,line,act$,"xit")
32040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
32060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
32080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
32100 ERTN_EXEC_ACT: execute act$ : goto ERTN
32120 ! /region
34000 def fn_setup
34020   if ~setup then 
34040     setup=1
34060     library 'S:\Core\Library': fntop,fnxit, fnTxt,fnLbl,fnTos,fnAcs,fnerror,fnCmdSet,fnChk,fngethandle,fnAddOneC
34070     library 'S:\Core\Library': fnArrayWasPassedC,fnArrayWasPassedN
34080     on error goto ERTN
34100     ! 
34120     dim resp$(60)*20,serviceName$(10)*20,serviceCode$(10)*2
34140     dim taxCode$(10)*1
34160     ! 
34180   end if 
34200 fnend 
36000 def fn_readService
36020   if ~readServiceSetup=val(env$('cno')) then
36040     readServiceSetup=val(env$('cno'))
36060     dim cacheServiceName$(10)*20
36080     dim cacheServiceCode$(10)*2
36100     dim cacheTaxCode$(10)*1
36120     dim cacheePenalty$(10)*1
36140     dim cacheSubjectTo(10)
36160     dim cacheOrderToApply(10)
36180     mat cacheServiceName$ =('')
36200     mat cacheServiceCode$ =('')
36220     mat cacheTaxCode$     =('')
36240     mat cacheePenalty$    =('')
36260     mat cacheSubjectTo    =(0)
36280     mat cacheOrderToApply =(0)
36300     open #hService:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",RecL=280,use",internal,outIn,relative 
36320     if lrec(hService)<1 then 
36340       write #hService,using F_service,rec=1: mat cacheServiceName$,mat cacheServiceCode$,mat cacheTaxCode$,mat cacheePenalty$,mat cacheSubjectTo,mat cacheOrderToApply
36360       pr 'A new empty Type of Service file was created.  Only ACS can edit this file type.  Type GO and press Enter to continue.' : pause
36380     end if
36400     read #hService,using F_service,rec=1: mat cacheServiceName$,mat cacheServiceCode$,mat cacheTaxCode$,mat cacheePenalty$,mat cacheSubjectTo,mat cacheOrderToApply
36440     F_service: form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*n 2,10*n 2
36460     close #hService:
36480   end if
36500   mat serviceName$(udim(mat cacheServiceName$)) : mat serviceName$=cacheServiceName$
36520   if fnArrayWasPassedC( mat serviceCode$) then mat serviceCode$(udim(mat cacheServiceCode$)) : mat serviceCode$=cacheServiceCode$
36540   if fnArrayWasPassedC( mat taxCode$    ) then mat taxCode$    (udim(mat cacheeTaxCode$   )) : mat taxCode$    =cacheTaxCode$
36560   if fnArrayWasPassedC( mat penalty$    ) then mat penalty$    (udim(mat cacheePenalty$   )) : mat penalty$    =cacheePenalty$
36580   if fnArrayWasPassedN( mat subjectTo   ) then mat subjectTo   (udim(mat cacheSubjectTo   )) : mat subjectTo   =cacheSubjectTo
36600   if fnArrayWasPassedN( mat orderToApply) then mat orderToApply(udim(mat cacheOrderToApply)) : mat orderToApply=cacheOrderToApply
36620 fnend
42000 def library fnget_services(mat serviceName$; mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
42020   fn_setup
42030   fn_readService
42060 fnend 
52000 def library fnGetServiceCodesMetered(mat serviceCodeMetered$)
52020   fn_setup
52040   fn_readService
52060   mat serviceCodeMetered$(0)
52080   for srv_item=1 to udim(mat serviceCode$)
52100     if (srv_item=1 and trim$(serviceCode$(srv_item))<>'') or (serviceName$(srv_item)="GAS" or serviceCode$(srv_item)="GA") or serviceCode$(srv_item)='EL' or serviceName$(srv_item)="Lawn Meter" then ! if it is a metered service
52120       fnAddOneC(mat serviceCodeMetered$,serviceCode$(srv_item))
52130       ! pr 'found metered service: '&serviceCode$(srv_item) 
52140     end if 
52160   next srv_item
52180 fnend
