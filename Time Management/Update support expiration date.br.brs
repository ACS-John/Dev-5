46000 ! load 'C:\ACS\Dev-5\Time Management\Update support expiration date.br'
46020 fn_setup
46040 fn_openFiles
46060 fntop(program$)
46080 fn_updateSupportExpirationDate
46100 goto XIT
46120 def fn_updateSupportExpirationDate(; clientKey$*5)
46140   if clientKey$='' then ! r: ask client(s) and process them
46160     do
46180       Screen1: !
46200       fnTos('')
46220       alignRight=1
46240       lenCol1=10
46260       posCol2=lenCol1+2
46280       ! (sfn$*100,lbuttonYNe,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
46300       fnLbl(1,1,'Client:',lenCol1,alignRight)
46320       fncombof('',1,posCol2,37,env$('Q')&'\TMmstr\CLmstr.H'&env$('cno'),1,5,6,30,env$('Q')&'\TMmstr\CLIndex.H'&env$('cno'),1)
46340       fnCmdSet( 2)
46360       fnAcs('',0,mat resp$, fk)
46380       if fk<>5 then 
46400         dim selectedClient$*128
46420         selectedClient$=resp$(1)
46440         clientKey$=selectedClient$(1:5)
46460         dim msgConfirm$(0)*256
46480         mat msgConfirm$(0)
46500         fnaddonec(mat msgConfirm$,'Client:'&tab$&selectedClient$)
46520         fnaddonec(mat msgConfirm$,'')
46540         fn_addSupportMsgLines$(mat msgConfirm$)
46560         fnaddonec(mat msgConfirm$,'')
46580         fnaddonec(mat msgConfirm$,'Total Cost:'&tab$&tab$&cnvrt$('pic($$$,$$#.##)',sum(mat cost)))
46600         fnaddonec(mat msgConfirm$,'')
46620         fnaddonec(mat msgConfirm$,'Update Expiration Dates? (adds one year to each)')
46640         fnmsgbox(mat msgConfirm$, resp$,'',buttonYN+iconQuestion+buttonDefaultTwo)
46660         if resp$='Yes' then 
46680           fn_updateOneSupportExpDate(selectedClient$(1:5))
46700           mat msgConfirm$(0)
46720           fnaddonec(mat msgConfirm$,'Client:'&tab$&selectedClient$)
46740           fnaddonec(mat msgConfirm$,'')
46760           fn_addSupportMsgLines$(mat msgConfirm$)
46780           fnaddonec(mat msgConfirm$,'')
46800           fnaddonec(mat msgConfirm$,'Total Cost:'&tab$&tab$&cnvrt$('pic($$$,$$#.##)',sum(mat cost)))
46820           fnaddonec(mat msgConfirm$,'')
46840           fnaddonec(mat msgConfirm$,'UPDATE COMPLETED')
46860           fnmsgbox(mat msgConfirm$,resp$,'',0+iconInformation)
46880         end if
46900       end if
46920     loop until fk=5
46940     ! /r
46960   else !  just process the client passed
46980     fn_updateOneSupportExpDate(clientKey$)
47000   end if
47020   ! usedXit: !
47040 fnend
47060 def fn_addSupportMsgLines$(mat msgText$)
47080   fn_getSupportArrayByClient(clientKey$,mat sysno,mat sysid$,mat dateStart,mat timeFrame$,mat dateExpire,mat cost)
47100   for supItem=1 to udim(mat sysno)
47120     fnaddonec(mat msgText$,rpad$(str$(sysno(supItem)),2)&'. '&sysid$(supItem)&tab$&timeFrame$(supItem)&' '&date$(days(dateExpire(supItem),'ccyymmdd'),'mm/dd/ccyy')&tab$&cnvrt$('pic(zzz,zzz.zz)',cost(supItem)))
47140   nex supItem
47160 fnend
47180 def fn_getSupportArrayByClient(client$,mat sysno,mat sysid$,mat dateStart,mat timeFrame$,mat dateExpire,mat cost)
47200   mat sysno(0)
47220   mat sysid$(0)
47240   mat dateStart(0)
47260   mat timeFrame$(0)
47280   mat dateExpire(0)
47300   mat cost(0)
47320   read #hSupport,using fSupport,key=>rpad$(client$,kln(hSupport)): clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
47340   do while rtrm$(client$)=rtrm$(clientId$)
47360     fnAddOneN(mat sysno,SysNo)
47380     fnaddoneC(mat sysid$,SysId$)
47400     fnAddOneN(mat dateStart,dateStart)
47420     fnaddoneC(mat timeFrame$,timeFrame$)
47440     fnAddOneN(mat dateExpire,dateExpire)
47460     fnAddOneN(mat cost,cost)
47480     read #hSupport,using fSupport: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof G1Finis
47500   loop
47520   G1Finis: ! 
47540 
47560 fnend
47580 def fn_updateOneSupportExpDate(client$)
47600   client$=rpad$(client$,kln(hSupport))
47620   read #hSupport,using fSupport,key=>client$: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
47640   do while rtrm$(client$)=rtrm$(clientId$)
47660     ! pr uCount+=1;rec(hSupport);clientId;SysNo;SysId$;timeFrame$,dateExpire;cost
47680     dateExpireNew=val(str$(date(days(dateExpire,'ccyymmdd'),'ccyy')+1)&date$(days(dateExpire,'ccyymmdd'),'mmdd'))
47700     ! pr 'dateExpireNew=';dateExpireNew
47720     dateExpire=dateExpireNew
47740     rewrite #hSupport,using fSupport: clientId$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost
47760     read #hSupport,using fSupport: client$,SysNo,SysId$,dateStart,timeFrame$,dateExpire,cost eof U1Finis
47780   loop
47800   U1Finis: ! 
47820   ! pause
47840 fnend
47860 def fn_openFiles
47880   if ~openFiles then
47900     openFiles=1
47920     open #hClientKey :=fngethandle: "Name="&env$('Q')&"\TMmstr\CLmstr.h420,Version=0,KFName="&env$('Q')&"\TMmstr\CLIndex.h420,Use,RecL=534,KPs=1,KLn=5,Shr",internal,outIn,keyed 
47940     open #hClientName:=fngethandle: "Name="&env$('Q')&"\TMmstr\CLmstr.h420,Version=0,KFName="&env$('Q')&"\TMmstr\CLIndx2-Idx.h420,Use,RecL=534,KPs=6,KLn=28,Shr",internal,outIn,keyed 
47960     open #hSupport   :=fngethandle: "Name="&env$('Q')&"\TMmstr\Support.h420,Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed
47980   end if
48000   fSupport: form pos 1,C 6,n 2,c 2,n 8,c 2,n 8,n 10
48020 fnend
48040 def fn_setup
48060   library 'S:\Core\Library': fntop,fnxit, fnerror
48080   library 'S:\Core\Library': fngethandle,fnaddonec,fnAddOneN
48100   library 'S:\Core\Library': fnmsgbox
48120   library 'S:\Core\Library': fnAcs,fnTos,fnLbl,fncombof,fnCmdSet
48140   dim resp$(10)*128
48160   tab$=chr$(9)
48180   buttonYN=4 : iconQuestion=32 : iconInformation=64 : buttonDefaultTwo=256 
48200 fnend
48220 XIT: fnxit
48240 IGNORE: continue 
48260 ! <Updateable Region: ERTN>
48280 ERTN: fnerror(program$,err,line,act$,"xit")
48300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
48320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
48340   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
48360 ERTN_EXEC_ACT: execute act$ : goto ERTN
48380 ! /region
